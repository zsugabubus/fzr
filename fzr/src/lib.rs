//! This crate implements a fuzzy matching algorithm.
//!
//! # Example
//!
//! ```
//! use fzr::*;
//!
//! let mut memory = Memory::new();
//!
//! let needle = Pattern::new("o").unwrap();
//!
//! let haystack = Haystack::parse("fóo", Scheme::ShellHistory);
//!
//! let m = find_fuzzy(&haystack.as_ref(), &needle, &mut memory).unwrap();
//! assert_eq!(m.ranges_len(), 1);
//! assert_eq!(m.range(0), 1..3);
//! ```
use std::{
    marker::PhantomData,
    num::{NonZeroU64, Saturating},
    ops::{Add, Range},
};

mod bits;
mod pattern;
mod unicode;

pub use pattern::*;

use crate::bits::BitsExt;

#[derive(Clone, Copy, Debug)]
pub enum Scheme {
    /// Shell history.
    ///
    /// # Example
    ///
    /// ```txt
    /// cargo build --release && echo | cat /etc/passwd
    /// ```
    ShellHistory,
    /// Path.
    ///
    /// The only difference to [`ShellHistory`][Self::ShellHistory] is that `" "` (space) is not treated as
    /// a word delimiter.
    ///
    /// # Example
    ///
    /// ```txt
    /// /path/to/my file.txt
    /// ```
    Path,
}

impl Scheme {
    fn is_shell_history(self) -> bool {
        matches!(self, Self::ShellHistory)
    }
}

/// Represents match score.
///
/// It can be retreived from the [`score`][Match::score] method on a [`Match`].
///
/// Higher score means better match. Exact match has the highest score.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[repr(transparent)]
pub struct Score(NonZeroU64);

impl Score {
    /// Returns the unit score for an exact match.
    #[inline]
    #[must_use]
    pub fn exact_match() -> Self {
        Self(NonZeroU64::new(1 << 40).unwrap())
    }

    /// Returns the higher score.
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
    StartWord {
        subwords: Saturating<u8>,
        letters: Saturating<u8>,
    },
    StartSlashWord {
        subwords: Saturating<u8>,
        letters: Saturating<u8>,
    },
    StartSubword {
        letters: Saturating<u8>,
    },
    StartPath {
        slashes: Saturating<u8>,
    },
    StartSkip,
    EndSkip,
    End,
}

/// Opaque object emitted by [`parse_haystack`].
#[derive(Clone, Copy, Debug)]
pub struct Token {
    pos: u32,
    kind: TokenKind,
}

impl Token {
    fn start_skip(pos: u32) -> Self {
        Self {
            pos,
            kind: TokenKind::StartSkip,
        }
    }

    fn end_skip(pos: u32) -> Self {
        Self {
            pos,
            kind: TokenKind::EndSkip,
        }
    }

    fn start_subword(pos: u32, letters: u8) -> Self {
        Self {
            pos,
            kind: TokenKind::StartSubword {
                letters: Saturating(letters),
            },
        }
    }

    fn start_path(pos: u32, slashes: u8) -> Self {
        Self {
            pos,
            kind: TokenKind::StartPath {
                slashes: Saturating(slashes),
            },
        }
    }

    fn start_slash_word(pos: u32) -> Self {
        Self {
            pos,
            kind: TokenKind::StartSlashWord {
                subwords: Saturating(0),
                letters: Saturating(0),
            },
        }
    }

    fn end(pos: u32) -> Self {
        Self {
            pos,
            kind: TokenKind::End,
        }
    }

    fn start_field(pos: u32) -> Self {
        Self {
            pos,
            kind: TokenKind::StartField,
        }
    }

    fn start_word(pos: u32) -> Self {
        Self {
            pos,
            kind: TokenKind::StartWord {
                subwords: Saturating(0),
                letters: Saturating(0),
            },
        }
    }

    fn slashes_mut(&mut self) -> Option<&mut Saturating<u8>> {
        if let Self {
            kind: TokenKind::StartPath { ref mut slashes },
            ..
        } = self
        {
            Some(slashes)
        } else {
            None
        }
    }

    fn subwords_mut(&mut self) -> Option<&mut Saturating<u8>> {
        if let Self {
            kind:
                TokenKind::StartWord {
                    ref mut subwords, ..
                }
                | TokenKind::StartSlashWord {
                    ref mut subwords, ..
                },
            ..
        } = self
        {
            Some(subwords)
        } else {
            None
        }
    }

    fn letters_mut(&mut self) -> Option<&mut Saturating<u8>> {
        if let Self {
            kind:
                TokenKind::StartWord {
                    ref mut letters, ..
                }
                | TokenKind::StartSlashWord {
                    ref mut letters, ..
                }
                | TokenKind::StartSubword {
                    ref mut letters, ..
                },
            ..
        } = self
        {
            Some(letters)
        } else {
            None
        }
    }
}

/// Reusable working memory for [`find_fuzzy`] and [`find_exact`].
#[derive(Default)]
pub struct Memory {
    y_ranges: [[Range<u32>; 32]; 32],
}

impl Memory {
    /// Returns some memory.
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

impl<'a, S: AsRef<str>> Haystack<'a, S, Box<[Token]>> {
    /// Returns the preprocessed haystack.
    ///
    /// This function is a safe wrapper around [`parse_haystack`] however it needs to allocate an
    /// auxiliary [`Vec`] on every call. See [`Self::from_parts`] for a more efficient but unsafe
    /// alternative.
    pub fn parse(value: S, scheme: Scheme) -> Self {
        let mut tokens = Vec::new();
        parse_haystack(value.as_ref(), scheme, &mut tokens);
        let tokens = tokens.into_boxed_slice();

        // SAFETY: `tokens` is derived from `value`.
        unsafe { Self::from_parts(value, tokens) }
    }
}

impl<'a, S, T> Haystack<'a, S, T> {
    /// Creates a [`Haystack`] from parts.
    ///
    /// # Safety
    ///
    /// `tokens` must be produced by [`parse_haystack`] from the given `value`.
    #[inline]
    pub unsafe fn from_parts(value: S, tokens: T) -> Self {
        Self {
            value,
            tokens,
            _phantom: &PhantomData,
        }
    }

    #[inline]
    pub fn value(&self) -> &S {
        &self.value
    }
}

impl<'a, S: AsRef<str>, T: AsRef<[Token]>> Haystack<'a, S, T> {
    #[inline]
    pub fn as_ref(&self) -> Haystack {
        // SAFETY: Taking reference should not affect safety.
        unsafe { Haystack::from_parts(self.value.as_ref(), self.tokens.as_ref()) }
    }
}

/// Preprocesses haystack according to the given scheme.
///
/// # Panics
///
/// Panics if `s.len()` >= [`u32::MAX`].
pub fn parse_haystack(s: &str, scheme: Scheme, tokens: &mut Vec<Token>) {
    assert!(s.len() < u32::MAX as usize);

    macro_rules! c {
        (not_digit) => {
            '\0'..='/' | '@'..=char::MAX
        };
        (digit) => {
            '0'..='9'
        };
    }

    let start = tokens.len();

    tokens.push(Token::start_word(0));

    let mut start_path: (usize, u32) = (start, 0);
    let mut word_index = start;
    let mut subword_index = start;

    let mut iter = s.char_indices().peekable();
    let mut prev_c = '\0';

    while let Some((i, c)) = iter.next() {
        let i = i as u32;

        let skipping = tokens
            .last()
            .is_some_and(|x| matches!(x.kind, TokenKind::EndSkip) && x.pos == i);

        match (prev_c, c) {
            (_, '\t') => {
                start_path = (tokens.len(), i);
                tokens.push(Token::start_field(i + 1));
                prev_c = c;
            }
            // Ignore whitespace after skip.
            (_, ' ' | '\u{A0}' | '\u{202F}') if skipping => {
                tokens.pop();
                tokens.push(Token::end_skip(i + c.encode_utf8(&mut [0; 4]).len() as u32));
            }
            // Collapse spaces.
            (' ', ' ') => {
                if skipping {
                    tokens.pop();
                } else {
                    tokens.push(Token::start_skip(i));
                }
                tokens.push(Token::end_skip(i + 1));
            }
            (_, ' ' | '=' | '|' | '&' | '<' | '>' | '?' | '"' | '\'')
                if scheme.is_shell_history() =>
            {
                start_path = (tokens.len(), i);
                word_index = tokens.len();
                subword_index = tokens.len();
                tokens.push(Token::start_word(i + 1));
                prev_c = c;
            }
            (_, '/') => {
                let (index, pos) = start_path;
                if let Some(slashes) = tokens[index].slashes_mut() {
                    *slashes += 1;
                } else {
                    tokens.insert(index, Token::start_path(pos, 1));
                }
                word_index = tokens.len();
                subword_index = tokens.len();
                tokens.push(Token::start_slash_word(i + 1));
                prev_c = c;
            }
            (_, '-' | '_' | '.') => {
                *tokens[word_index].subwords_mut().unwrap() += 1;
                *tokens[subword_index].letters_mut().unwrap() += 1;
                subword_index = tokens.len();
                tokens.push(Token::start_subword(i + 1, 0));
                prev_c = c;
            }
            // Ignore leading zeros.
            (c!(not_digit), '0') if matches!(iter.peek(), Some((_, c!(digit)))) => {
                if skipping {
                    tokens.pop();
                } else {
                    tokens.push(Token::start_skip(i));
                }
                let mut to = i;
                while let Some((i, _)) = iter.next_if(|&(_, c)| c == '0') {
                    to = i as u32;
                }
                let i = if let Some((i, _)) = iter.next_if(|&(_, c)| c.is_ascii_digit()) {
                    i as u32
                } else {
                    to
                };
                tokens.push(Token::end_skip(i));

                *tokens[word_index].subwords_mut().unwrap() += 1;
                subword_index = tokens.len();
                tokens.push(Token::start_subword(i, 1));
                prev_c = c;
            }
            ('a'..='z', 'A'..='Z') | (c!(not_digit), c!(digit)) | (c!(digit), c!(not_digit)) => {
                *tokens[word_index].subwords_mut().unwrap() += 1;
                subword_index = tokens.len();
                tokens.push(Token::start_subword(i, 1));
                prev_c = c;
            }
            // Ignore characters that (likely) has no textual representation.
            //
            // https://en.wikipedia.org/wiki/Unicode_block
            (
                _,
                '\u{E000}'..='\u{F8FF}'
                | '\u{F0000}'..='\u{FFFFD}'
                | '\u{100000}'..='\u{10FFFD}'
                | '\u{1F000}'..='\u{1F0FF}'
                | '\u{1F300}'..='\u{1FBFF}',
            ) => {
                if skipping {
                    tokens.pop();
                } else {
                    tokens.push(Token::start_skip(i));
                }
                tokens.push(Token::end_skip(i + c.encode_utf8(&mut [0; 4]).len() as u32));
            }
            // Ignore SGR escape sequences.
            (_, '\x1b') => {
                if skipping {
                    tokens.pop();
                } else {
                    tokens.push(Token::start_skip(i));
                }
                let to = if let Some((i, _)) = iter.find(|(_, c)| *c == 'm') {
                    i as u32 + 1
                } else {
                    s.len() as u32
                };
                tokens.push(Token::end_skip(to));
            }
            _ => {
                *tokens[subword_index].letters_mut().unwrap() += 1;
                prev_c = c;
            }
        }
    }

    tokens.push(Token::end(s.len() as u32));

    #[cfg(test)]
    println!("{:?}", tokens);
}

/// Finds needle in haystack using a fuzzy matching algorithm.
///
/// Returns [`Some`] if matched, [`None`] otherwise.
pub fn find_fuzzy<'m>(
    haystack: &Haystack,
    pat: &Pattern,
    memory: &'m mut Memory,
) -> Option<Match<'m>> {
    let Haystack { value, tokens, .. } = haystack;

    debug_assert!(matches!(tokens.last().unwrap().kind, TokenKind::End));
    debug_assert!(tokens.last().unwrap().pos as usize == haystack.value().len());

    let mut cur_token = 0;
    // SAFETY: `tokens` is terminated by `TokenKind::End` thus has at least one item.
    let mut next_pos = unsafe { tokens.get_unchecked(cur_token) }.pos as usize;

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

    let mut y_max = [0_u64; 32];

    let mut i = 0;

    'outer: loop {
        while i >= next_pos {
            // SAFETY: `tokens` has already been indexed by `cur_token`.
            let token = unsafe { tokens.get_unchecked(cur_token) };
            cur_token += 1;

            match token.kind {
                TokenKind::StartField => {
                    field += 1;
                    word = 0;
                    subword = 0;
                    letter = 0;
                }
                TokenKind::StartWord {
                    subwords: x,
                    letters: y,
                } => {
                    word += 1;
                    subword = 0;
                    subwords = x;
                    letter = 0;
                    letters = y;
                }
                TokenKind::StartSubword { letters: y } => {
                    subword += 1;
                    subwords -= 1;
                    letter = 0;
                    letters = y;
                }
                TokenKind::StartPath { slashes: x } => {
                    slash = x;
                    slashes = slash;
                }
                TokenKind::StartSlashWord {
                    subwords: x,
                    letters: y,
                } => {
                    slash -= 1;
                    subword = 0;
                    subwords = x;
                    letter = 0;
                    letters = y;
                }
                TokenKind::StartSkip => {
                    // SAFETY: Always followed by `TokenKind::EndSkip`.
                    let end_skip = unsafe { tokens.get_unchecked(cur_token) };
                    debug_assert!(matches!(end_skip.kind, TokenKind::EndSkip));
                    i = end_skip.pos as usize;
                    cur_token += 1;
                }
                TokenKind::EndSkip => {
                    // Skipped by `TokenKind::StartSkip`.
                    unreachable!();
                }
                TokenKind::End => {
                    break 'outer;
                }
            }

            // SAFETY: `tokens` is terminated by `TokenKind::End`. When `TokenKind::End` seen loop
            // is terminated.
            next_pos = unsafe { tokens.get_unchecked(cur_token) }.pos as usize;
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

        for y in matches.one_bits_rev() {
            let y = y as usize;

            let prefix_score = if y > 0 { y_max[y - 1] } else { 0 };

            let cont = (old_prev_matches & (1 << y)) != 0;
            let must_cont = letter > 1;
            let head = cont || !must_cont;

            let match_score = (7_u64.saturating_sub(if y == 0 {
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
            if total_score <= y_max[y] {
                continue;
            }

            y_max[y] = total_score;
            matches_mask |= (4_u32 << y).wrapping_sub(1);

            #[allow(clippy::assigning_clones)]
            if y > 0 {
                memory.y_ranges[y] = memory.y_ranges[y - 1].clone();
            }
            memory.y_ranges[y][y] = (i as u32)..(i + utf8_len) as u32;
        }

        i += utf8_len;
    }

    if let Some(score) = NonZeroU64::new(y_max[pat.len.get() - 1]) {
        Some(Match {
            score: Score(score),
            ranges: &memory.y_ranges[pat.len.get() - 1][..pat.len.get()],
        })
    } else {
        None
    }
}

/// Finds needle in haystack using an exact matching algorithm.
///
/// Returns [`Some`] if matched, [`None`] otherwise.
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

    let mut cur_token = 0;
    // SAFETY: `tokens` is terminated by `TokenKind::End` thus has at least one item.
    let mut next_pos = unsafe { tokens.get_unchecked(cur_token) }.pos as usize;

    let mut prev_matches = 0_u32;
    let pat_match = 1 << (pat.len.get() - 1);

    let mut did_match = false;

    let mut i = 0;
    let mut k = 0;

    'outer: loop {
        while i >= next_pos {
            // SAFETY: `tokens` has already been indexed by `cur_token`.
            let token = unsafe { tokens.get_unchecked(cur_token) };
            cur_token += 1;

            match token.kind {
                TokenKind::StartSkip => {
                    // SAFETY: Always followed by `TokenKind::EndSkip`.
                    let end_skip = unsafe { tokens.get_unchecked(cur_token) };
                    debug_assert!(matches!(end_skip.kind, TokenKind::EndSkip));
                    i = end_skip.pos as usize;
                    cur_token += 1;
                }
                TokenKind::End => {
                    break 'outer;
                }
                _ => {}
            }

            // SAFETY: `tokens` is terminated by `TokenKind::End`. When `TokenKind::End` seen loop
            // is terminated.
            next_pos = unsafe { tokens.get_unchecked(cur_token) }.pos as usize;
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

impl<'m> Match<'m> {
    /// Returns match score.
    pub fn score(&self) -> Score {
        self.score
    }

    /// Returns number of matching ranges.
    #[inline]
    pub fn ranges_len(&self) -> usize {
        self.ranges.len()
    }

    /// Returns byte positions for range.
    ///
    /// # Panics
    ///
    /// Panics if `index` is outside `0..Self::ranges_len()`.
    #[inline]
    pub fn range(&self, index: usize) -> Range<usize> {
        let range = &self.ranges[index];
        range.start as usize..range.end as usize
    }

    /// Returns first matching byte position.
    #[inline]
    pub fn start(&self) -> usize {
        self.ranges.first().unwrap().start as usize
    }

    /// Returns last matching byte position.
    #[inline]
    pub fn end(&self) -> usize {
        self.ranges.last().unwrap().end as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn assert_order(needle: &str, haystacks: &[&str]) {
        assert!(haystacks.len() > 1);

        let pat = Pattern::new(needle).unwrap();
        let mut memory = Memory::default();

        let actual_order = {
            let mut haystacks = haystacks.to_vec();
            haystacks.reverse();
            haystacks.sort_by_cached_key(|haystack| {
                println!();
                let m = find_fuzzy(
                    &Haystack::parse(&haystack, Scheme::ShellHistory).as_ref(),
                    &pat,
                    &mut memory,
                )
                .unwrap();
                println!("{}: {:?} {:?}", haystack, m.score(), m.start());
                std::cmp::Reverse(m.score())
            });
            haystacks
        };

        let expected_order = haystacks.to_vec();

        assert_eq!(actual_order, expected_order);
    }

    #[track_caller]
    fn assert_equal(needle: &str, haystacks: &[&str]) {
        assert!(haystacks.len() > 1);

        let pat = Pattern::new(needle).unwrap();
        let mut memory = Memory::default();

        let actual_scores = haystacks
            .iter()
            .map(|haystack| {
                println!();
                let m = find_fuzzy(
                    &Haystack::parse(&haystack, Scheme::ShellHistory).as_ref(),
                    &pat,
                    &mut memory,
                )
                .unwrap();
                println!("{}: {:?} {:?}", haystack, m.score(), m.start());
                m.score()
            })
            .collect::<Vec<_>>();

        let expected_scores = vec![*actual_scores.first().unwrap(); haystacks.len()];

        assert_eq!(actual_scores, expected_scores);
    }

    #[track_caller]
    fn assert_filter(needle: &str, haystacks: &[&str], expected_matches: &[&str]) {
        let pat = Pattern::new(needle).unwrap();
        let mut memory = Memory::default();

        let actual_matches = haystacks
            .iter()
            .filter(|haystack| {
                println!();
                find_fuzzy(
                    &Haystack::parse(&haystack, Scheme::ShellHistory).as_ref(),
                    &pat,
                    &mut memory,
                )
                .is_some()
            })
            .copied()
            .collect::<Vec<_>>();

        assert_eq!(&actual_matches[..], expected_matches);
    }

    #[track_caller]
    fn assert_accepts(needle: &str, haystack: &str) {
        assert_filter(needle, &[haystack], &[haystack]);
    }

    #[track_caller]
    fn assert_rejects(needle: &str, haystack: &str) {
        assert_filter(needle, &[haystack], &[]);
    }

    #[track_caller]
    fn assert_ranges(needle: &str, haystack: &str, expected_ranges: &[Range<usize>]) {
        let pat = Pattern::new(needle).unwrap();
        let mut memory = Memory::default();

        println!();
        let m = find_fuzzy(
            &Haystack::parse(&haystack, Scheme::ShellHistory).as_ref(),
            &pat,
            &mut memory,
        )
        .unwrap();

        let actual_ranges = (0..m.ranges_len()).map(|i| m.range(i)).collect::<Vec<_>>();

        assert_eq!(&actual_ranges, expected_ranges);
    }

    #[track_caller]
    fn assert_exact(
        needle: &str,
        (anchor_start, anchor_end): (bool, bool),
        haystack: &str,
        expected_ranges: Option<&[Range<usize>]>,
    ) {
        let pat = Pattern::new(needle).unwrap();
        let mut memory = Memory::default();

        println!();
        let m = find_exact(
            &Haystack::parse(&haystack, Scheme::ShellHistory).as_ref(),
            &pat,
            anchor_start,
            anchor_end,
            &mut memory,
        );

        if let Some(expected_ranges) = expected_ranges {
            assert!(m.is_some());
            let m = m.unwrap();

            let actual_ranges = (0..m.ranges_len()).map(|i| m.range(i)).collect::<Vec<_>>();

            assert_eq!(&actual_ranges, expected_ranges);
        } else {
            assert!(m.is_none());
        }
    }

    #[test]
    fn huge_pattern() {
        assert!(Pattern::new(('a'..='z').collect::<String>().as_str()).is_ok());
        assert!(Pattern::new(('A'..='Z').collect::<String>().as_str()).is_ok());
    }

    #[test]
    fn empty_haystack() {
        let pat = Pattern::new("x").unwrap();
        let mut memory = Memory::default();
        assert!(find_fuzzy(
            &Haystack::parse("", Scheme::ShellHistory).as_ref(),
            &pat,
            &mut memory
        )
        .is_none());
        assert!(find_fuzzy(
            &Haystack::parse("x", Scheme::ShellHistory).as_ref(),
            &pat,
            &mut memory
        )
        .is_some());
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
        assert_order("a1", &["\ta1\t", "\tA\t1"]);
        assert_equal("1", &["001", "1"]);
        assert_order("1", &["01", "_1"]);
        assert_order("1", &["1", "10", "101", "21"]);
        assert_order("12", &["012", "120", "102", "1002"]);
        assert_order("123", &["X01X02/X01X23", "X1/X02X03", "X01X02/X01X03"]);
        assert_equal("ab", &["a00b", "a01b"]);
        assert_order("a", &["xxxxxxxx/a/x", "a_x/x"]);
        assert_order("a", &["a_x/x", "a_x_x/x"]);
        assert_order("a", &["xxx xxx xxx a", "x/a"]);
        assert_order("a", &["xxx\tax", "xa\tx"]);
        assert_order("abcd", &["ab abcd/cd", "ab abcd/xx"]);
        assert_order("b", &["a\tbc", "ab\tc"]);
        assert_equal(
            "ab",
            &[
                "a\u{e000}\u{f8ff} \u{f0000}\u{ffffd} \u{100000}\u{10FFFD}\u{a0}\u{202f}b",
                "ab",
            ],
        );
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
        assert_equal("ab", &["Ax\x1b[mB", "AxB"]);
        assert_equal("a", &["a.b", "a.bx", "a.bxx"]);
        assert_order("ab", &["a.b", "a.bx", "a.bxx"]);
        assert_equal("ab", &["a-b", "A-B", "a_b", "A_B", "AxB"]);
        assert_equal("ab", &["-a-b", "xAxB"]);
        assert_order("bc", &["ab/b/c", "ab/x_c"]);
        assert_order("abc", &["ab/x_c", "ab/b/c"]);
        assert_order("abc", &["aBxxBc", "aXbc"]);
        assert_order("abcd", &["AbBcd", "AbBcD", "AbCxd"]);
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
