pub struct OneBitsRev(u32);

impl Iterator for OneBitsRev {
    type Item = u32;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            let index = 31 - self.0.leading_zeros();
            self.0 &= !(1 << index);
            Some(index)
        } else {
            None
        }
    }
}

pub trait BitsExt {
    fn one_bits_rev(self) -> OneBitsRev;
}

impl BitsExt for u32 {
    fn one_bits_rev(self) -> OneBitsRev {
        OneBitsRev(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_one_bits_rev() {
        fn check(input: u32, expected: &[u32]) {
            assert_eq!(input.one_bits_rev().collect::<Box<_>>().as_ref(), expected);
        }

        check(u32::MIN, &[]);
        check(1, &[0]);
        check(1 << 31, &[31]);
        check((1 << 31) | 1, &[31, 0]);
        check(u32::MAX, &(0..32).rev().collect::<Box<_>>());
    }
}
