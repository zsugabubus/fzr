//! This crate implements a fuzzy matching algorithm.
//!
//! # Example
//!
//! ```
//! use fzr::*;
//!
//! let mut memory = Memory::new();
//! let pat = Pattern::new("fb").unwrap();
//! let haystack = Haystack::parse("foobar");
//!
//! assert!(find_fuzzy(&haystack.as_ref(), &pat, &mut memory).is_some());
//! assert!(find_exact(&haystack.as_ref(), &pat, false, false, &mut memory).is_none());
//! ```
use std::{
    fmt,
    marker::PhantomData,
    num::{NonZeroU64, Saturating},
    ops::{Add, Range},
};

mod bits;
mod pattern;
mod unicode;

pub use pattern::*;

use crate::bits::BitsExt;

/// Represents match score.
///
/// It can be retreived from the [`Match::score`] method.
///
/// Higher score means better match. Exact match has the highest score.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(transparent)]
pub struct Score(NonZeroU64);

impl Score {
    /// Returns the unit score for an exact match.
    #[must_use]
    #[inline]
    pub fn exact_match() -> Self {
        Self(NonZeroU64::new(1 << 45).unwrap())
    }

    /// Returns the higher score.
    #[must_use]
    #[inline]
    pub fn max(self, other: Self) -> Self {
        Self(self.0.max(other.0))
    }
}

impl Add for Score {
    type Output = Self;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        Self(self.0.saturating_add(other.0.get()))
    }
}

#[derive(Clone, Copy, Debug)]
enum TokenKind {
    StartField,
    StartWord,
    StartSlashWord,
    StartSubword,
    StartPath,
    StartSkip,
    EndSkip,
    End,
}

/// Opaque object emitted by [`parse_haystack`].
#[derive(Clone, Copy)]
pub struct Token {
    pos: u32,
    kind: TokenKind,
    subwords_or_slashes: Saturating<u8>,
    letters: Saturating<u8>,
}

impl Token {
    fn new(pos: u32, kind: TokenKind) -> Self {
        Self {
            pos,
            kind,
            subwords_or_slashes: Saturating(0),
            letters: Saturating(0),
        }
    }

    fn has_slashes(&self) -> bool {
        matches!(self.kind, TokenKind::StartPath)
    }

    fn has_subwords(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::StartField | TokenKind::StartWord | TokenKind::StartSlashWord
        )
    }

    fn has_letters(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::StartField
                | TokenKind::StartWord
                | TokenKind::StartSlashWord
                | TokenKind::StartSubword
        )
    }

    fn slashes(&self) -> Saturating<u8> {
        debug_assert!(self.has_slashes());
        self.subwords_or_slashes
    }

    fn slashes_mut(&mut self) -> &mut Saturating<u8> {
        debug_assert!(self.has_slashes());
        &mut self.subwords_or_slashes
    }

    fn subwords(&self) -> Saturating<u8> {
        debug_assert!(self.has_subwords());
        self.subwords_or_slashes
    }

    fn subwords_mut(&mut self) -> &mut Saturating<u8> {
        debug_assert!(self.has_subwords());
        &mut self.subwords_or_slashes
    }

    fn letters(&self) -> Saturating<u8> {
        debug_assert!(self.has_letters());
        self.letters
    }

    fn letters_mut(&mut self) -> &mut Saturating<u8> {
        debug_assert!(self.has_letters());
        &mut self.letters
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut builder = f.debug_struct("Token");
        builder.field("pos", &self.pos);
        builder.field("kind", &self.kind);
        if self.has_slashes() {
            builder.field("slashes", &self.slashes());
        }
        if self.has_subwords() {
            builder.field("subwords", &self.subwords());
        }
        if self.has_letters() {
            builder.field("letters", &self.letters());
        }
        builder.finish()
    }
}

/// Reusable working memory for [`find_fuzzy`] and [`find_exact`].
#[derive(Default)]
pub struct Memory {
    y_ranges: [[Range<u32>; 32]; 32],
}

impl Memory {
    /// Returns some memory.
    #[must_use]
    #[inline]
    pub fn new() -> Self {
        Memory::default()
    }
}

/// Haystack.
pub struct Haystack<'a, S = &'a str, T = &'a [Token]> {
    value: S,
    tokens: T,
    _phantom: &'a PhantomData<()>,
}

impl<S: AsRef<str>> Haystack<'_, S, Box<[Token]>> {
    /// Returns the preprocessed haystack.
    ///
    /// This function is a safe wrapper around [`parse_haystack`] however it needs to allocate an
    /// auxiliary [`Vec`] on every call. See [`Self::from_parts`] for a more efficient but unsafe
    /// alternative.
    #[must_use]
    pub fn parse(value: S) -> Self {
        let mut tokens = Vec::new();
        parse_haystack(value.as_ref(), &mut tokens);
        let tokens = tokens.into_boxed_slice();

        // SAFETY: `tokens` is derived from `value`.
        unsafe { Self::from_parts(value, tokens) }
    }
}

impl<S, T> Haystack<'_, S, T> {
    /// Creates a [`Haystack`] from parts.
    ///
    /// # Safety
    ///
    /// `tokens` must be produced by [`parse_haystack`] from the given `value`.
    #[must_use]
    #[inline]
    pub unsafe fn from_parts(value: S, tokens: T) -> Self {
        Self {
            value,
            tokens,
            _phantom: &PhantomData,
        }
    }

    #[must_use]
    #[inline]
    pub fn value(&self) -> &S {
        &self.value
    }
}

impl<S: AsRef<str>, T: AsRef<[Token]>> Haystack<'_, S, T> {
    #[inline]
    pub fn as_ref(&self) -> Haystack<'_> {
        // SAFETY: Taking reference should not affect safety.
        unsafe { Haystack::from_parts(self.value.as_ref(), self.tokens.as_ref()) }
    }
}

/// Preprocesses haystack.
///
/// # Panics
///
/// Panics if `s.len()` >= [`u32::MAX`].
pub fn parse_haystack(s: &str, tokens: &mut Vec<Token>) {
    use std::str::CharIndices;

    fn is_next_ascii_digit(iter: &CharIndices) -> bool {
        matches!(*iter.as_str().as_bytes(), [b'0'..=b'9', ..])
    }

    fn next_if_ascii_leading_zero(iter: &mut CharIndices) -> Option<usize> {
        if matches!(*iter.as_str().as_bytes(), [b'0', b'0'..=b'9', ..]) {
            Some(iter.next().unwrap().0)
        } else {
            None
        }
    }

    assert!(s.len() < u32::MAX as usize);
    debug_assert!(tokens.is_empty());

    let mut path_index = 0;
    let mut word_index = 0;
    let mut subword_index = 0;

    let mut iter = s.char_indices();
    let mut prev_c = '\0';

    tokens.push(Token::new(0, TokenKind::StartField));

    while let Some((i, c)) = iter.next() {
        let i = i as u32;

        debug_assert!(!tokens.is_empty());
        let last = unsafe { tokens.get_unchecked(tokens.len() - 1) };
        let skipping = matches!(last.kind, TokenKind::EndSkip) && last.pos == i;

        macro_rules! add_subword_to_word {
            () => {
                let token = unsafe { tokens.get_unchecked_mut(word_index) };
                *token.subwords_mut() += 1;
            };
        }

        macro_rules! add_letter_to_subword {
            () => {
                let token = unsafe { tokens.get_unchecked_mut(subword_index) };
                *token.letters_mut() += 1;
            };
        }

        macro_rules! start_subword {
            ($pos:expr) => {
                subword_index = tokens.len();
                tokens.push(Token::new($pos, TokenKind::StartSubword));
            };
        }

        macro_rules! skip {
            ($to:expr) => {
                if skipping {
                    let end_skip = tokens.pop();
                    debug_assert!(matches!(end_skip.unwrap().kind, TokenKind::EndSkip));
                } else {
                    tokens.push(Token::new(i, TokenKind::StartSkip));
                }
                tokens.push(Token::new($to, TokenKind::EndSkip));
            };
        }

        match c {
            // Handle fields.
            '\t' => {
                path_index = tokens.len();
                word_index = tokens.len();
                subword_index = tokens.len();
                tokens.push(Token::new(
                    i + '\t'.len_utf8() as u32,
                    TokenKind::StartField,
                ));
                prev_c = '\0';
            }
            // Continue ignoring space.
            ' ' | '\u{A0}' | '\u{202F}' if skipping => {
                skip!(i + c.len_utf8() as u32);
            }
            // Collapse spaces.
            ' ' if prev_c == ' ' => {
                debug_assert!(!skipping);
                skip!(i + c.len_utf8() as u32);
            }
            // Handle paths.
            '/' => {
                let token = unsafe { tokens.get_unchecked_mut(path_index) };
                if matches!(token.kind, TokenKind::StartPath) {
                    *token.slashes_mut() += 1;
                } else {
                    let mut token = Token::new(token.pos, TokenKind::StartPath);
                    *token.slashes_mut() += 1;
                    path_index += 1;
                    tokens.insert(path_index, token);
                }
                word_index = tokens.len();
                subword_index = tokens.len();
                tokens.push(Token::new(
                    i + '/'.len_utf8() as u32,
                    TokenKind::StartSlashWord,
                ));
                prev_c = '\0';
            }
            // Handle kebab-case, snake_case and file.ext.
            '-' | '_' | '.' => {
                add_subword_to_word!();
                add_letter_to_subword!();
                start_subword!(i + 1);
                prev_c = '\0';
            }
            // Ignore SGR escape sequences.
            '\x1b' => {
                skip!(if let Some((i, _)) = iter.find(|(_, c)| *c == 'm') {
                    i as u32 + 'm'.len_utf8() as u32
                } else {
                    s.len() as u32
                });
            }
            // Ignore characters that (likely) has no textual representation, e.g. icons from Nerd
            // Fonts.
            '\u{E000}'..='\u{F8FF}'
            | '\u{F0000}'..='\u{FFFFD}'
            | '\u{100000}'..='\u{10FFFD}'
            | '\u{1F000}'..='\u{1F0FF}'
            | '\u{1F300}'..='\u{1FBFF}' => {
                skip!(i + c.len_utf8() as u32);
            }
            // Ignore leading zeros.
            '0' if !prev_c.is_ascii_digit() && is_next_ascii_digit(&iter) => {
                skip!({
                    let mut to = i;
                    while let Some(i) = next_if_ascii_leading_zero(&mut iter) {
                        to = i as u32;
                    }
                    to + '0'.len_utf8() as u32
                });
            }
            // Handle word breaks.
            _ if (prev_c == ' ' || prev_c.is_ascii_punctuation())
                && (c.is_alphabetic() || c.is_ascii_digit()) =>
            {
                debug_assert!(!matches!(prev_c, '\t' | '/' | '-' | '_' | '.'));
                path_index = tokens.len();
                word_index = tokens.len();
                subword_index = tokens.len();
                tokens.push(Token::new(i, TokenKind::StartWord));
                add_letter_to_subword!();
                prev_c = c;
            }
            // Handle camelCase.
            _ if prev_c.is_lowercase() && c.is_uppercase() => {
                add_subword_to_word!();
                start_subword!(i);
                add_letter_to_subword!();
                prev_c = c;
            }
            // Handle abc100 and 100abc.
            _ if (prev_c != '\0' && !prev_c.is_ascii_digit() && c.is_ascii_digit())
                || (prev_c.is_ascii_digit() && !c.is_ascii_digit()) =>
            {
                // FIXME: Same as above but merging them results in worse performance. Check why.
                add_subword_to_word!();
                start_subword!(i);
                add_letter_to_subword!();
                prev_c = c;
            }
            _ => {
                add_letter_to_subword!();
                prev_c = c;
            }
        }
    }

    tokens.push(Token::new(s.len() as u32, TokenKind::End));

    #[cfg(test)]
    println!("{:?}", tokens);
}

/// Finds pattern using a fuzzy matching algorithm.
///
/// Returns [`Some`] if matched, [`None`] otherwise.
#[must_use]
pub fn find_fuzzy<'m>(
    haystack: &Haystack,
    pat: &Pattern,
    memory: &'m mut Memory,
) -> Option<Match<'m>> {
    let Haystack { value, tokens, .. } = haystack;

    debug_assert!(matches!(
        tokens.first().unwrap().kind,
        TokenKind::StartField
    ));
    debug_assert!(matches!(tokens.last().unwrap().kind, TokenKind::End));
    debug_assert!(tokens.last().unwrap().pos as usize == haystack.value().len());

    let mut token_index = 0;
    debug_assert!(tokens.first().unwrap().pos == 0);
    let mut next_token_pos = 0;

    let mut slash = Saturating(0);
    let mut word = 0_u32;
    let mut subword = 0_u32;
    let mut letter = 0_u32;
    let mut letters = Saturating(0);
    let mut field = 0_u32;
    let mut subwords = Saturating(0);
    let mut slashes = Saturating(0);

    let mut prev_matches = 1_u32;
    let mut matches_mask = 1_u32;

    let mut y_max = [0_u64; 33];

    let mut i = 0;

    'outer: loop {
        while i >= next_token_pos {
            let token = unsafe { tokens.get_unchecked(token_index) };
            token_index += 1;

            match token.kind {
                TokenKind::StartField => {
                    field += 1;
                    word = 0;
                    subword = 0;
                    subwords = token.subwords();
                    letter = 0;
                    letters = token.letters();
                }
                TokenKind::StartWord => {
                    word += 1;
                    subword = 0;
                    subwords = token.subwords();
                    letter = 0;
                    letters = token.letters();
                }
                TokenKind::StartSubword => {
                    subword += 1;
                    subwords -= 1;
                    letter = 0;
                    letters = token.letters();
                }
                TokenKind::StartPath => {
                    slash = token.slashes();
                    slashes = slash;
                }
                TokenKind::StartSlashWord => {
                    slash -= 1;
                    subword = 0;
                    subwords = token.subwords();
                    letter = 0;
                    letters = token.letters();
                }
                TokenKind::StartSkip => {
                    let end_skip = unsafe { tokens.get_unchecked(token_index) };
                    debug_assert!(matches!(end_skip.kind, TokenKind::EndSkip));
                    i = end_skip.pos as usize;
                    token_index += 1;
                }
                TokenKind::EndSkip => {
                    // Skipped by `TokenKind::StartSkip`.
                    unreachable!();
                }
                TokenKind::End => {
                    break 'outer;
                }
            }

            next_token_pos = unsafe { tokens.get_unchecked(token_index) }.pos as usize;
        }

        // SAFETY:
        // - `TokenKind::End` terminates loop before `i` would reach end of the string.
        // - It is on UTF-8 sequence boundary because it always incremented by the size of the
        // sequence.
        let (utf8_len, matches) = unsafe { pat.map.parse_str_unchecked(value.get_unchecked(i..)) };

        letter += 1;

        #[cfg(test)]
        println!(
            "  [{:2}] = {:?} => field={} slash={}/{} word={} subword={}/{} letter={}/{}",
            i,
            value.as_bytes()[i] as char,
            field,
            slash,
            slashes,
            word,
            subword,
            subwords,
            letter,
            letters,
        );

        let matches = matches & matches_mask;

        let old_prev_matches = prev_matches;
        prev_matches = 0;

        if matches != 0 {
            for y in matches.one_bits_rev() {
                let y = y as usize;

                let prefix_score = y_max[y];

                let cont = (old_prev_matches & (1 << y)) != 0;
                let must_cont = letter > 1;
                let head = cont || !must_cont;

                let match_score = (15_u64.saturating_sub(if head { u64::from(field) } else { 16 })
                    << 36)
                    + (7_u64.saturating_sub(if y == 0 {
                        0
                    } else {
                        // Gap penalty.
                        // FIXME: Should ignore skipped ranges but currently only byte offsets are
                        // persisted. We would need global letter positions. However it probably does not
                        // worth it, it is good enough.
                        u64::from(i as u32 - memory.y_ranges[y - 1][y - 1].end) / 32
                    }) << 33)
                    + (15_u64.saturating_sub(if head { u64::from(slash.0) } else { 16 }) << 29)
                    + (31_u64.saturating_sub(if head { u64::from(subword) } else { 32 }) << 24)
                    + (15_u64.saturating_sub(if head { 0 } else { u64::from(letter) }) << 20)
                    + (7_u64.saturating_sub(u64::from(subwords.0)) << 17)
                    + (7_u64.saturating_sub(u64::from(slashes.0)) << 14)
                    + (63_u64.saturating_sub(u64::from(word)) << 8)
                    + (15_u64.saturating_sub(u64::from(field)) << 4)
                    + 15_u64.saturating_sub(u64::from(letters.0));

                let total_score = prefix_score + match_score;

                #[cfg(test)]
                println!(
                    "    y={} score={:6} + {:6} = {:6}{}{}",
                    y,
                    prefix_score,
                    match_score,
                    total_score,
                    if cont { " [cont]" } else { "" },
                    if must_cont { " [must cont]" } else { "" }
                );

                prev_matches |= if head { 2 } else { 0 } << y;

                // Give some hints to the compiler that the most likely thing this branch will do is
                // `continue`.
                if total_score <= y_max[y + 1] {
                    continue;
                }

                y_max[y + 1] = total_score;
                matches_mask |= (4_u32 << y).wrapping_sub(1);

                #[allow(clippy::assigning_clones)]
                if y > 0 {
                    memory.y_ranges[y] = memory.y_ranges[y - 1].clone();
                }
                memory.y_ranges[y][y] = (i as u32)..(i + utf8_len) as u32;
            }
        }

        i += utf8_len;
    }

    if let Some(score) = NonZeroU64::new(y_max[pat.len.get()]) {
        Some(Match {
            score: Score(score),
            ranges: &memory.y_ranges[pat.len.get() - 1][..pat.len.get()],
        })
    } else {
        None
    }
}

/// Finds pattern using an exact matching algorithm.
///
/// Returns [`Some`] if matched, [`None`] otherwise.
#[must_use]
pub fn find_exact<'m>(
    haystack: &Haystack,
    pat: &Pattern,
    anchor_start: bool,
    anchor_end: bool,
    memory: &'m mut Memory,
) -> Option<Match<'m>> {
    let Haystack { value, tokens, .. } = haystack;

    debug_assert!(matches!(tokens.last().unwrap().kind, TokenKind::End));
    debug_assert!(tokens.last().unwrap().pos as usize == haystack.value().len());

    let mut token_index = 0;
    debug_assert!(tokens.first().unwrap().pos == 0);
    let mut next_token_pos = 0;

    let mut prev_matches = 0_u32;
    let pat_match = 1 << (pat.len.get() - 1);

    let mut did_match = false;

    let mut i = 0;
    let mut k = 0;

    'outer: loop {
        while i >= next_token_pos {
            let token = unsafe { tokens.get_unchecked(token_index) };
            token_index += 1;

            match token.kind {
                TokenKind::StartSkip => {
                    // SAFETY: Always followed by `TokenKind::EndSkip`.
                    let end_skip = unsafe { tokens.get_unchecked(token_index) };
                    debug_assert!(matches!(end_skip.kind, TokenKind::EndSkip));
                    i = end_skip.pos as usize;
                    token_index += 1;
                }
                TokenKind::End => {
                    break 'outer;
                }
                _ => {}
            }

            next_token_pos = unsafe { tokens.get_unchecked(token_index) }.pos as usize;
        }

        // SAFETY:
        // - `TokenKind::End` terminates loop before `i` would reach end of the string.
        // - It is on UTF-8 sequence boundary because it always incremented by the size of the
        // sequence.
        let (utf8_len, matches) = unsafe { pat.map.parse_str_unchecked(value.get_unchecked(i..)) };

        prev_matches = matches & ((prev_matches << 1) | 1);

        memory.y_ranges[0][k % 32] = (i as u32)..(i + utf8_len) as u32;
        k += 1;

        if (prev_matches & pat_match) != 0 {
            did_match = true;

            for i in 0..pat.len.get() {
                memory.y_ranges[1][i] =
                    memory.y_ranges[0][(k + 32 + i - pat.len.get()) % 32].clone();
            }

            if !anchor_end {
                break;
            }
        } else if k == pat.len.get() && anchor_start {
            return None;
        }

        i += utf8_len;
    }

    if !did_match {
        return None;
    }

    if anchor_end && (prev_matches & pat_match) == 0 {
        return None;
    }

    Some(Match {
        score: Score::exact_match(),
        ranges: &memory.y_ranges[1][..pat.len.get()],
    })
}

/// Represents a [`Pattern`] match in the [`Haystack`].
#[derive(Debug)]
pub struct Match<'m> {
    score: Score,
    ranges: &'m [Range<u32>],
}

impl Match<'_> {
    /// Returns the score of the match.
    ///
    /// # Example
    ///
    /// ```
    /// use fzr::{Memory, Pattern, Haystack, find_fuzzy};
    ///
    /// let mut memory = Memory::new();
    /// let b = Pattern::new("b").unwrap();
    /// let bar = Haystack::parse("bar");
    /// let foobar = Haystack::parse("foobar");
    ///
    /// let bar_score = find_fuzzy(&bar.as_ref(), &b, &mut memory).unwrap().score();
    /// let foobar_score = find_fuzzy(&foobar.as_ref(), &b, &mut memory).unwrap().score();
    /// assert!(bar_score > foobar_score);
    /// ```
    #[must_use]
    pub fn score(&self) -> Score {
        self.score
    }

    /// Returns an iterator over the matched byte ranges of the haystack.
    ///
    /// # Example
    ///
    /// ```
    /// use fzr::{Memory, Pattern, Haystack, find_fuzzy};
    ///
    /// let mut memory = Memory::new();
    /// let fb = Pattern::new("fb").unwrap();
    /// let foobar = Haystack::parse("foobar");
    ///
    /// let m = find_fuzzy(&foobar.as_ref(), &fb, &mut memory).unwrap();
    /// assert_eq!(m.ranges().len(), 2);
    /// assert_eq!(&m.ranges().collect::<Vec<_>>(), &[0..1, 3..4]);
    /// ```
    #[must_use]
    #[inline]
    pub fn ranges(&self) -> Ranges<'_> {
        Ranges {
            index: 0,
            ranges: self.ranges,
        }
    }
}

/// An iterator over the ranges of a [`Match`].
///
/// This struct is created by the [`Match::ranges`] method.
pub struct Ranges<'m> {
    index: usize,
    ranges: &'m [Range<u32>],
}

impl Iterator for Ranges<'_> {
    type Item = Range<usize>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        fn cast(range: &Range<u32>) -> Range<usize> {
            range.start as usize..range.end as usize
        }

        let index = self.index;
        self.index += 1;
        self.ranges.get(index).map(cast)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let rem = self.ranges.len() - self.index;
        (rem, Some(rem))
    }
}

impl ExactSizeIterator for Ranges<'_> {}

#[cfg(test)]
#[allow(clippy::single_range_in_vec_init)]
mod tests {
    use super::*;

    #[track_caller]
    fn assert_order(pat: &str, haystacks: &[&str]) {
        assert!(haystacks.len() > 1);

        let pat = Pattern::new(pat).unwrap();
        let mut memory = Memory::default();

        let actual_order = {
            let mut haystacks = haystacks.to_vec();
            haystacks.reverse();
            haystacks.sort_by_cached_key(|haystack| {
                println!();
                let m =
                    find_fuzzy(&Haystack::parse(&haystack).as_ref(), &pat, &mut memory).unwrap();
                println!("{}: {:?}", haystack, m.score());
                assert!(m.score() < Score::exact_match());
                std::cmp::Reverse(m.score())
            });
            haystacks
        };

        let expected_order = haystacks.to_vec();

        assert_eq!(actual_order, expected_order);
    }

    #[track_caller]
    fn assert_equal(pat: &str, haystacks: &[&str]) {
        assert!(haystacks.len() > 1);

        let pat = Pattern::new(pat).unwrap();
        let mut memory = Memory::default();

        let actual_scores = haystacks
            .iter()
            .map(|haystack| {
                println!();
                let m =
                    find_fuzzy(&Haystack::parse(&haystack).as_ref(), &pat, &mut memory).unwrap();
                println!("{}: {:?}", haystack, m.score());
                assert!(m.score() < Score::exact_match());
                m.score()
            })
            .collect::<Vec<_>>();

        let expected_scores = vec![*actual_scores.first().unwrap(); haystacks.len()];

        assert_eq!(actual_scores, expected_scores);
    }

    #[track_caller]
    fn assert_filter(pat: &str, haystacks: &[&str], expected_matches: &[&str]) {
        let pat = Pattern::new(pat).unwrap();
        let mut memory = Memory::default();

        let actual_matches = haystacks
            .iter()
            .filter(|haystack| {
                println!();
                if let Some(m) = find_fuzzy(&Haystack::parse(&haystack).as_ref(), &pat, &mut memory)
                {
                    assert!(m.score() < Score::exact_match());
                    true
                } else {
                    false
                }
            })
            .copied()
            .collect::<Vec<_>>();

        assert_eq!(&actual_matches[..], expected_matches);
    }

    #[track_caller]
    fn assert_accepts(pat: &str, haystack: &str) {
        assert_filter(pat, &[haystack], &[haystack]);
    }

    #[track_caller]
    fn assert_rejects(pat: &str, haystack: &str) {
        assert_filter(pat, &[haystack], &[]);
    }

    #[track_caller]
    fn assert_ranges(pat: &str, haystack: &str, expected_ranges: &[Range<usize>]) {
        let pat = Pattern::new(pat).unwrap();
        let mut memory = Memory::default();

        println!();
        let m = find_fuzzy(&Haystack::parse(&haystack).as_ref(), &pat, &mut memory).unwrap();
        assert!(m.score() < Score::exact_match());

        let actual_ranges = m.ranges().collect::<Vec<_>>();

        assert_eq!(&actual_ranges, expected_ranges);
    }

    #[track_caller]
    fn assert_exact(
        pat: &str,
        (anchor_start, anchor_end): (bool, bool),
        haystack: &str,
        expected_ranges: Option<&[Range<usize>]>,
    ) {
        let pat = Pattern::new(pat).unwrap();
        let mut memory = Memory::default();

        println!();
        let m = find_exact(
            &Haystack::parse(&haystack).as_ref(),
            &pat,
            anchor_start,
            anchor_end,
            &mut memory,
        );

        if let Some(expected_ranges) = expected_ranges {
            assert!(m.is_some());
            let m = m.unwrap();

            let actual_ranges = m.ranges().collect::<Vec<_>>();

            assert_eq!(&actual_ranges, expected_ranges);
        } else {
            assert!(m.is_none());
        }
    }

    #[test]
    fn empty_haystack() {
        let pat = Pattern::new("x").unwrap();
        let mut memory = Memory::default();
        assert!(find_fuzzy(&Haystack::parse("").as_ref(), &pat, &mut memory).is_none());
        assert!(find_fuzzy(&Haystack::parse("x").as_ref(), &pat, &mut memory).is_some());
    }

    #[test]
    fn order() {
        assert_equal("a", &["A", "a"]);
        assert_order("a", &["axxxxxxxx", "x/a", "x/x/a", "a_x", "x/a_x", "a/x"]);
        assert_order("a", &["xA", "xa"]);
        assert_order("ab", &["ab  x", " ab x"]);
        assert_order("abc", &["/AbxxxxxxC", "/AxbC"]);
        assert_order("abc", &["AxBcxxxxxx", "axBxXc"]);
        assert_order("a", &["A", "x/x/A"]);
        assert_order("b", &["xbxxxxxx.bx.x", "xbxxxxxx.x"]);
        assert_order("b", &["xbxxxxxx.b", "xbxxxxxx.bx.b"]);
        assert_order("ab", &["a/ax/b", "a/xa/b"]);
        assert_order("abc", &["abcxxx", "abc/xx"]);
        assert_order("a", &["a/ax", "a/aa_x"]);
        assert_order("a", &["ab", "ba"]);
        assert_order("abc", &["./abc", "./ab-c"]);
        assert_order("a", &["a", "_a"]);
        assert_order("abcd", &["a_b_c/d", "a/xab_c_d"]);
        assert_order("abc", &["ax_bx/cx.x", "ax_bx/bc/x"]);
        assert_order("abc", &["ax_bx/cx.x", "ab.xc"]);
        assert_order("ab", &["x\ta\tb", "x\tx\tab"]);
        assert_order("ab", &["a\tb", "x\tab"]);
        assert_order("ab", &["a\tb", "\tab"]);
        assert_order("a1", &["a1", "a_1"]);
        assert_equal("a1", &["a_1", "a10"]);
        assert_equal("1a", &["1_a", "10a"]);
        assert_order("a1", &["\ta1\t", "\ta\t1"]);
        assert_equal("1", &["1", "01", "001", "0001"]);
        assert_equal("9", &["9", "09", "009", "0009"]);
        assert_order("1", &["01", "_1"]);
        assert_order("1", &["1", "10", "101", "21"]);
        assert_order("12", &["012", "120", "102", "1002"]);
        assert_order("123", &["X01X02/X01X23", "X1/X02X03", "X01X02/X01X03"]);
        assert_equal("ab", &["a00b", "a01b"]);
        assert_order("a", &["xxxxxxxx/a/x", "a_x/x"]);
        assert_order("a", &["a_x/x", "a_x_x/x"]);
        assert_order("a", &["xxx xxx xxx a", "x/a"]);
        assert_order("a", &["xxx\tax", "xa\tx"]);
        assert_order("a", &["xa\txx", "xx\txa"]);
        assert_order("abcd", &["ab abcd/cd", "ab abcd/xx"]);
        assert_order("b", &["a\tbc", "ab\tc"]);
        assert_equal(
            "ab",
            &[
                "a\u{e000}\u{f8ff} \u{f0000}\u{ffffd} \u{100000}\u{10FFFD}\u{a0}\u{202f}b",
                "ab",
            ],
        );
        assert_equal("1a", &["1\u{e000}a", "1a"]);
        assert_order("abc", &["abc", "a    bc", "a b c"]);
        assert_order(
            "ab",
            &[
                &format!("a{x} b{x}", x = "x".repeat(10)),
                "a-b",
                &format!("a{x} b{x}", x = "x".repeat(50)),
                &format!("a{x} b{x}", x = "x".repeat(100)),
            ],
        );
        assert_equal("ab", &["a\x1bmb", "ab"]);
        assert_equal("ab", &["Ax\x1b[mB", "AxB"]);
        assert_equal("10", &["1\x1bm0", "10"]);
        assert_equal("1a", &["1\x1bma", "1a"]);
        assert_order("1", &["1", "x 1", "1 x", "1_x", "x_1"]);
        assert_equal("a", &["1a", "1_a"]);
        assert_equal("a", &["a.b", "a.bx", "a.bxx"]);
        assert_order("ab", &["a.b", "a.bx", "a.bxx"]);
        assert_equal("ab", &["a-b", "A-B", "a_b", "A_B", "AxB"]);
        assert_equal("ab", &["-a-b", "xAxB"]);
        assert_equal("a", &["aAaA", "áÁáÁ"]);
        assert_order("bc", &["ab/b/c", "ab/x_c"]);
        assert_order("abc", &["ab/x_c", "ab/b/c"]);
        assert_order("abc", &["aBxxBc", "aXbc"]);
        assert_order("abcd", &["AbBcd", "AbBcD", "AbCxd"]);
        for c in (0..=127).map(char::from).filter(char::is_ascii_punctuation) {
            assert_order("a", &[&format!("xx{c}a"), "xa"]);
        }
    }

    #[test]
    fn filter() {
        for i in 1..32 {
            let a = "a".repeat(i - 1);
            let ax = "a".repeat(i - 1) + "x";
            let aa = "a".repeat(i);
            let aaa = "a".repeat(i + 1);
            let axaa = format!("{ax}{aa}");
            assert_filter(&aa, &[&a, &ax, &aa, &aaa, &axaa], &[&aa, &aaa, &axaa]);
        }

        assert_accepts("a", "a");
        assert_accepts("a", "A");
        assert_accepts("A", "A");
        assert_rejects("A", "a");
        assert_accepts("/", "/");
        assert_accepts("@", "@");
        assert_accepts("b", "abc");
        assert_rejects("aa", "a");
        assert_accepts("ab", "ab");
        assert_rejects("ab", "a");
        assert_rejects("ab", "b");
        assert_rejects("ab", "ba");
        assert_accepts("o", "ó");
        assert_accepts("o", "Ó");
        assert_accepts("oo", "oó");
        assert_accepts("ó", "ó");
        assert_accepts("oó", "oó");
        assert_accepts("óo", "óó");
        assert_rejects("m", "\x1b[m\x1b[m");
        assert_rejects("0", "\x1b[0");
        assert_rejects("0", "\x1b[0m");
        assert_accepts("ab", "a\x1b[mb");
        assert_accepts("a", "\x1b[ma\x1b[m");
        assert_accepts("a", &"a".repeat(1000));
        assert_accepts("/", &"/".repeat(1000));
        assert_accepts("a", &"Aa".repeat(1000));
        assert_accepts("0", "0");
        assert_accepts("0", "00");
        assert_accepts("1", "01");
        assert_rejects("00", "00");
        assert_rejects("01", "01");
        assert_accepts("10001", "10001");
    }

    #[test]
    fn ranges() {
        assert_ranges("a", "a", &[0..1]);
        assert_ranges("a", "a/a", &[2..3]);
        assert_ranges("a", "á", &[0..2]);
        assert_ranges("aa", "aá", &[0..1, 1..3]);
        assert_ranges("á", "á", &[0..2]);
        assert_ranges("aá", "aá", &[0..1, 1..3]);
        assert_ranges("ab", "xa-ab", &[3..4, 4..5]);
        assert_ranges("ab", "xaxab", &[1..2, 4..5]);
    }

    #[test]
    fn exact() {
        let any = (false, false);
        let starts = (true, false);
        let ends = (false, true);
        let full = (true, true);

        assert_exact("a", any, "", None);
        assert_exact("a", any, "a", Some(&[0..1]));
        assert_exact("a", full, "A", Some(&[0..1]));
        assert_exact("o", full, "ó", Some(&[0..2]));
        assert_exact("aab", any, "aaaab", Some(&[2..3, 3..4, 4..5]));
        assert_exact("a", full, "a", Some(&[0..1]));
        assert_exact("abc", full, "abc", Some(&[0..1, 1..2, 2..3]));
        for i in 0..32 {
            assert_exact(
                "ab",
                any,
                &format!("{}ab", "x".repeat(i)),
                Some(&[i..i + 1, i + 1..i + 2]),
            );
        }
        assert_exact("a", full, "\x1b[ma\x1b[m", Some(&[3..4]));
        assert_exact("ab", full, "\x1b[ma\x1b[mb\x1b[m", Some(&[3..4, 7..8]));
        assert_exact("b", any, "\t/ a-aA/b", Some(&[8..9]));
        assert_exact("a", starts, "xa", None);
        assert_exact("ab", starts, "a", None);
        assert_exact("ab", starts, "b", None);
        assert_exact("ab", starts, "ax", None);
        assert_exact("ab", starts, "bx", None);
        assert_exact("ab", starts, "axb", None);
        assert_exact("ab", starts, "ba", None);
        assert_exact("a", ends, "ax", None);
        assert_exact("ab", ends, "a", None);
        assert_exact("ab", ends, "b", None);
        assert_exact("ab", ends, "xa", None);
        assert_exact("ab", ends, "xb", None);
        assert_exact("ab", ends, "axb", None);
        assert_exact("ab", ends, "ba", None);
    }
}
