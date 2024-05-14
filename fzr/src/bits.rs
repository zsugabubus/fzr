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
    fn one_bits_rev() {
        assert_eq!(&1.one_bits_rev().collect::<Vec<_>>()[..], &[0]);
        assert_eq!(&(1 << 31).one_bits_rev().collect::<Vec<_>>()[..], &[31]);
        assert_eq!(u32::MAX.one_bits_rev().count(), 32);
        assert_eq!(
            &((1 << 31) | 1).one_bits_rev().collect::<Vec<_>>()[..],
            &[31, 0]
        );
    }
}
