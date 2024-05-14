use crate::unicode::get_unicode_variations;
use std::num::NonZeroUsize;
use utf8_map::{Utf8Map, Utf8MapBuilder};

/// An error returned from [`PatternBuilder::build`].
#[derive(Debug, PartialEq)]
pub enum PatternError {
    /// Pattern contains no characters.
    Empty,
    /// Pattern contains too many characters that cannot not be represented.
    TooLong,
}

/// Case sensitivity.
#[non_exhaustive]
pub enum CaseSensitivity {
    /// A character is equivalent to itself only.
    Sensitive,
    /// Uppercase and lowercase ASCII characters are treated as equivalent.
    AsciiInsensitive,
    /// Lowercase ASCII characters are treated equivalent to their uppercase counterpart.
    ///
    /// This is the most user-friendly choice.
    LowerAsciiInsensitive,
}

/// Builder for [`Pattern`].
pub struct PatternBuilder {
    case_sensitivity: CaseSensitivity,
    romanize_unicode: bool,
}

impl PatternBuilder {
    /// Constructs a new [`PatternBuilder`] with defaults.
    pub fn new() -> Self {
        Self {
            case_sensitivity: CaseSensitivity::LowerAsciiInsensitive,
            romanize_unicode: true,
        }
    }

    /// Sets case sensitivity.
    pub fn case_sensitivity(&mut self, case_sensitivity: CaseSensitivity) -> &mut Self {
        self.case_sensitivity = case_sensitivity;
        self
    }

    /// Sets whether Latin characters can match similar-looking Unicode characters.
    ///
    /// When set to [`true`], `"R"` matches `"Ř"`, `"ʁ"`, `"ℝ"`, `"🄬"`, `"🆁"`… etc.
    pub fn romanize_unicode(&mut self, yes: bool) -> &mut Self {
        self.romanize_unicode = yes;
        self
    }

    fn insert(&self, map: &mut Utf8MapBuilder, c: char, bits: u32) {
        match self.case_sensitivity {
            CaseSensitivity::AsciiInsensitive if c.is_ascii_alphabetic() => {
                self.insert2(map, c.to_ascii_lowercase(), bits);
                self.insert2(map, c.to_ascii_uppercase(), bits);
            }
            CaseSensitivity::LowerAsciiInsensitive if c.is_ascii_lowercase() => {
                self.insert2(map, c, bits);
                self.insert2(map, c.to_ascii_uppercase(), bits);
            }
            _ => {
                self.insert2(map, c, bits);
            }
        }
    }

    fn insert2(&self, map: &mut Utf8MapBuilder, c: char, bits: u32) {
        map.insert(c, bits);

        if self.romanize_unicode {
            map.extend(get_unicode_variations(c).iter().map(|&c| (c, bits)));
        }
    }

    /// Builds the [`Pattern`].
    pub fn build(&self, pat: &str) -> Result<Pattern, PatternError> {
        let mut map = Utf8MapBuilder::new();
        let mut len = 0;

        for (i, c) in pat.chars().enumerate() {
            len += 1;
            if len > 31 {
                return Err(PatternError::TooLong);
            }

            self.insert(&mut map, c, 1 << i);
        }

        Ok(Pattern {
            len: NonZeroUsize::new(len).ok_or(PatternError::Empty)?,
            map: map.build(),
        })
    }
}

impl Default for PatternBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub(crate) map: Utf8Map,
    pub(crate) len: NonZeroUsize,
}

impl Pattern {
    /// Creates a new [`Pattern`] using the default [`PatternBuilder`].
    pub fn new<T: AsRef<str>>(pat: T) -> Result<Self, PatternError> {
        PatternBuilder::new().build(pat.as_ref())
    }
}

#[cfg(test)]
#[allow(non_upper_case_globals)]
mod tests {
    use super::*;

    const MAX_LEN: usize = 31;

    const a: u32 = 1 << 0;
    const A: u32 = 1 << 1;
    const aacute: u32 = 1 << 2;
    const Aacute: u32 = 1 << 3;

    #[test]
    fn empty() {
        assert_eq!(Pattern::new("").unwrap_err(), PatternError::Empty);
    }

    #[test]
    fn too_long() {
        assert!(Pattern::new("a".repeat(MAX_LEN)).is_ok());
        assert_eq!(
            Pattern::new("a".repeat(MAX_LEN + 1)).unwrap_err(),
            PatternError::TooLong
        );
    }

    #[test]
    fn all_match_ascii() {
        let pat = Pattern::new("a".repeat(MAX_LEN)).unwrap();
        assert_eq!(pat.map.parse_str("a"), Some((1, (1 << MAX_LEN) - 1)));
    }

    #[test]
    fn all_match_unicode() {
        let pat = Pattern::new("á".repeat(MAX_LEN)).unwrap();
        assert_eq!(pat.map.parse_str("á"), Some((2, (1 << MAX_LEN) - 1)));
    }

    fn case_matches(case_sensitivity: CaseSensitivity) -> (u32, u32, u32, u32) {
        let pat = PatternBuilder::new()
            .case_sensitivity(case_sensitivity)
            .romanize_unicode(true)
            .build("aAáÁ")
            .unwrap();
        (
            pat.map.parse_str("a").unwrap().1,
            pat.map.parse_str("A").unwrap().1,
            pat.map.parse_str("á").unwrap().1,
            pat.map.parse_str("Á").unwrap().1,
        )
    }

    #[test]
    fn case_sensitive() {
        assert_eq!(
            case_matches(CaseSensitivity::Sensitive),
            (a, A, a | aacute, A | Aacute)
        );
    }

    #[test]
    fn case_ascii_insensitive() {
        assert_eq!(
            case_matches(CaseSensitivity::AsciiInsensitive),
            (a | A, a | A, a | A | aacute, a | A | Aacute)
        );
    }

    #[test]
    fn case_lower_ascii_insensitive() {
        assert_eq!(
            case_matches(CaseSensitivity::LowerAsciiInsensitive),
            (a, a | A, a | aacute, a | A | Aacute)
        );
    }
}
