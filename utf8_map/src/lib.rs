use std::{collections::HashMap, fmt, mem::size_of_val};

const CONT_MASK: u8 = 0x3f;

type StateIndex = u8;

#[derive(Clone, Copy)]
#[repr(transparent)]
struct ValueStateIndex(StateIndex);

#[derive(Clone, Copy)]
#[repr(transparent)]
struct ContStateIndex(StateIndex);

/// Builder for [`Utf8Map`].
pub struct Utf8MapBuilder {
    head: [u32; 256],
    cont: Vec<[StateIndex; 64]>,
    values: Vec<[u32; 64]>,
    len: usize,
}

impl Utf8MapBuilder {
    /// Constructs a new [`Utf8MapBuilder`].
    pub fn new() -> Self {
        // Start with one `cont` and one `values` state so unallocated entries will return `0`.
        Self {
            head: [0; 256],
            values: vec![[0; 64]],
            cont: vec![[0; 64]],
            len: 0,
        }
    }

    fn new_cont_state(&mut self) {
        self.cont.push([0; 64]);
    }

    fn new_value_state(&mut self) {
        self.values.push([0; 64]);
    }

    fn insert_head_value_state(&mut self, input: u8) -> ValueStateIndex {
        let state = &mut self.head[usize::from(input)];
        if *state == 0 {
            let index: StateIndex = self.values.len().try_into().unwrap();
            *state = u32::from(index);
            self.new_value_state();
            ValueStateIndex(index)
        } else {
            ValueStateIndex((*state).try_into().unwrap())
        }
    }

    fn insert_head_cont_state(&mut self, input: u8) -> ContStateIndex {
        let state = &mut self.head[usize::from(input)];
        if *state == 0 {
            let index: StateIndex = self.cont.len().try_into().unwrap();
            *state = u32::from(index);
            self.new_cont_state();
            ContStateIndex(index)
        } else {
            ContStateIndex((*state).try_into().unwrap())
        }
    }

    fn insert_cont_state(&mut self, state: ContStateIndex, input: u8) -> ContStateIndex {
        let index: StateIndex = self.cont.len().try_into().unwrap();
        let state = &mut self.cont[state.0 as usize][usize::from(input & CONT_MASK)];
        if *state == 0 {
            *state = index;
            self.new_cont_state();
            ContStateIndex(index)
        } else {
            ContStateIndex(*state)
        }
    }

    fn insert_final_cont_state(&mut self, state: ContStateIndex, input: u8) -> ValueStateIndex {
        let state = &mut self.cont[state.0 as usize][usize::from(input & CONT_MASK)];
        if *state == 0 {
            let index: StateIndex = self.values.len().try_into().unwrap();
            *state = index;
            self.new_value_state();
            ValueStateIndex(index)
        } else {
            ValueStateIndex(*state)
        }
    }

    fn insert_head_value(&mut self, input: u8, bits: u32) {
        self.head[usize::from(input)] |= bits;
    }

    fn insert_value(&mut self, state: ValueStateIndex, input: u8, bits: u32) {
        self.values[state.0 as usize][usize::from(input & CONT_MASK)] |= bits;
    }

    /// Associates bits with character.
    ///
    /// # Examples
    ///
    /// ```
    /// use utf8_map::Utf8MapBuilder;
    ///
    /// let mut builder = Utf8MapBuilder::new();
    ///
    /// builder.insert('a', 0b01);
    /// builder.insert('a', 0b10);
    ///
    /// assert_eq!(builder.build().parse_str("a"), Some((1, 0b11)));
    /// ```
    pub fn insert(&mut self, c: char, bits: u32) {
        self.len += 1;

        match *c.encode_utf8(&mut [0; 4]).as_bytes() {
            [a] => {
                self.insert_head_value(a, bits);
            }
            [a, b] => {
                let state = self.insert_head_value_state(a);
                self.insert_value(state, b, bits);
            }
            [a, b, c] => {
                let state = self.insert_head_cont_state(a);
                let state = self.insert_final_cont_state(state, b);
                self.insert_value(state, c, bits);
            }
            [a, b, c, d] => {
                let state = self.insert_head_cont_state(a);
                let state = self.insert_cont_state(state, b);
                let state = self.insert_final_cont_state(state, c);
                self.insert_value(state, d, bits);
            }
            _ => unreachable!(),
        }
    }

    /// Builds the [`Utf8Map`].
    pub fn build(self) -> Utf8Map {
        Utf8Map {
            head: self.head,
            cont: self.cont.into_boxed_slice(),
            values: self.values.into_boxed_slice(),
        }
    }
}

impl Extend<(char, u32)> for Utf8MapBuilder {
    fn extend<T: IntoIterator<Item = (char, u32)>>(&mut self, iter: T) {
        iter.into_iter().for_each(|(c, bits)| self.insert(c, bits));
    }
}

impl fmt::Debug for Utf8MapBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Utf8MapBuilder")
            .field(
                "head",
                &self
                    .head
                    .iter()
                    .take(128)
                    .copied()
                    .enumerate()
                    .filter(|(_, bits)| *bits != 0)
                    .map(|(c, bits)| (char::from(c as u8), bits))
                    .collect::<HashMap<_, _>>(),
            )
            .field("value_states", &self.values.len())
            .field("intermediate_states", &self.cont.len())
            .field(
                "allocated_bytes",
                &(size_of_val(&self.head) + size_of_val(&*self.cont) + size_of_val(&*self.values)),
            )
            .field("len", &self.len)
            .finish()
    }
}

impl Default for Utf8MapBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// UTF-8 to [`u32`] map.
///
// ```plain
// head
// +---+
// |  0|
// |...|
// |127|->value
// |   |     +---+
// |   |-----|  0|
// |   |--\  |  1|-> value
// +---+  |  |...|
//        |  +---+
//        |
//        |  +---+   +---+
//        \--|   |---|...|
//           |   |   |   |-> value
//           |   |   +---+
//           |   |
//           |   |---+---+
//           +---+   |...|
//                   |   |
//                   +---+-> value
// ```
#[derive(Clone)]
pub struct Utf8Map {
    head: [u32; 256],
    cont: Box<[[StateIndex; 64]]>,
    values: Box<[[u32; 64]]>,
}

impl Utf8Map {
    /// Returns a default [`Utf8MapBuilder`].
    #[inline]
    pub fn builder() -> Utf8MapBuilder {
        Utf8MapBuilder::default()
    }

    /// Returns value corresponding to the first character of the input.
    ///
    /// Returns `(UTF-8 sequence length, value)` tuple wrapped in [`Some`] if string is not empty,
    /// otherwise returns [`None`].
    #[inline(always)]
    pub fn parse_str(&self, s: &str) -> Option<(usize, u32)> {
        if s.is_empty() {
            None
        } else {
            // SAFETY: We just checked that it is not empty.
            Some(unsafe { self.parse_str_unchecked(s) })
        }
    }

    /// Optimized version of [`parse_str`](Self::parse_str).
    ///
    /// # Safety
    ///
    /// The caller must ensure that `s` is not empty.
    // NOTE: It would be nice if we would not need this function but
    // `parse_str(...).unwrap_unchecked()` is "optimized" in a way that makes it even slower than
    // `parse_str(...).unwrap()`.
    #[inline(always)]
    pub unsafe fn parse_str_unchecked(&self, s: &str) -> (usize, u32) {
        // SAFETY: `get_unchecked`s are valid by construction.
        unsafe {
            match *s.as_bytes() {
                [a, ..] if a <= 0b0111_1111 => (1, self.head[a as usize]),
                [a, b, ..] if a <= 0b1101_1111 => {
                    let state = self.head[a as usize];
                    (
                        2,
                        self.values.get_unchecked(state as usize)[(b & CONT_MASK) as usize],
                    )
                }
                [a, b, c, ..] if a <= 0b1110_1111 => {
                    let state = self.head[a as usize];
                    let state = self.cont.get_unchecked(state as usize)[(b & CONT_MASK) as usize];
                    (
                        3,
                        self.values.get_unchecked(state as usize)[(c & CONT_MASK) as usize],
                    )
                }
                [a, b, c, d, ..] if a <= 0b1111_0111 => {
                    let state = self.head[a as usize];
                    let state = self.cont.get_unchecked(state as usize)[(b & CONT_MASK) as usize];
                    let state = self.cont.get_unchecked(state as usize)[(c & CONT_MASK) as usize];
                    (
                        4,
                        self.values.get_unchecked(state as usize)[(d & CONT_MASK) as usize],
                    )
                }
                _ => {
                    // All valid UTF-8 sequences are covered above and caller ensures that the
                    // string is not empty.
                    std::hint::unreachable_unchecked()
                }
            }
        }
    }
}

impl fmt::Debug for Utf8Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Utf8Map").finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_str() {
        fn check(input: char) {
            let s = format!("{input}");

            let builder = Utf8MapBuilder::new();
            assert_eq!(builder.build().parse_str(&s), Some((input.len_utf8(), 0)));

            let mut builder = Utf8MapBuilder::new();
            builder.insert(input, 1);
            builder.insert(input, 2);
            assert_eq!(
                builder.build().parse_str(&s),
                Some((input.len_utf8(), 1 | 2))
            );
        }

        assert_eq!(Utf8MapBuilder::new().build().parse_str(""), None);

        check('\u{0}');
        check('\u{7F}');

        check('\u{80}');
        check('\u{7FF}');

        check('\u{800}');
        check('\u{FFFF}');

        check('\u{10000}');
        check('\u{10FFFF}');
    }
}
