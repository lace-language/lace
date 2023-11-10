use std::num::NonZeroU16;

/// Represents a constant binary value. This can be a float, integer
/// char or boolean. Anything really. Typecheck information on nodes
/// can disambiguate what operation to apply to values of this type.
#[derive(Debug, PartialEq, Eq)]
pub enum BinaryValue {
    /// if values are less than 64 bits we can easily store it inline
    Smol { data: u64, bits: NonZeroU16 },
    /// however, less common big values we cannot and we store these
    /// as a pointer instead. TODO: this may turn out to be a mistake
    BigBoi { data: Box<u128>, bits: NonZeroU16 },
}

impl BinaryValue {
    pub fn new(value: u128, bits: NonZeroU16) -> Option<Self> {
        let mask = match bits.get() {
            i @ 0..=127 => (1u128 << i) - 1,
            128 => u128::MAX,
            _ => return None,
        };

        let truncated_value = value & mask;
        if truncated_value != value {
            return None;
        }

        Some(if bits.get() <= 64 {
            Self::Smol {
                data: truncated_value as u64,
                bits,
            }
        } else {
            Self::BigBoi {
                data: Box::new(truncated_value),
                bits,
            }
        })
    }
}

macro_rules! from {
    ($ty: ty, $bits: literal) => {
        impl From<$ty> for BinaryValue {
            fn from(value: $ty) -> Self {
                Self::Smol {
                    data: value as u64,
                    bits: NonZeroU16::new($bits).unwrap(),
                }
            }
        }
    };
}

from!(u64, 64);
from!(u32, 32);
from!(u16, 16);
from!(u8, 8);

from!(i64, 64);
from!(i32, 32);
from!(i16, 16);
from!(i8, 8);

impl From<i128> for BinaryValue {
    fn from(value: i128) -> Self {
        Self::BigBoi {
            data: Box::new(value as u128),
            bits: NonZeroU16::new(128).unwrap(),
        }
    }
}

impl From<u128> for BinaryValue {
    fn from(value: u128) -> Self {
        Self::BigBoi {
            data: Box::new(value),
            bits: NonZeroU16::new(128).unwrap(),
        }
    }
}

impl From<f64> for BinaryValue {
    fn from(value: f64) -> Self {
        Self::Smol {
            data: value.to_bits(),
            bits: NonZeroU16::new(64).unwrap(),
        }
    }
}

impl From<f32> for BinaryValue {
    fn from(value: f32) -> Self {
        Self::Smol {
            data: value.to_bits() as u64,
            bits: NonZeroU16::new(32).unwrap(),
        }
    }
}

impl From<bool> for BinaryValue {
    fn from(value: bool) -> Self {
        Self::Smol {
            data: if value { 1 } else { 0 },
            bits: NonZeroU16::new(1).unwrap(),
        }
    }
}

mod tests {
    use crate::lowering::lir::int::BinaryValue;

    #[test]
    pub fn int_size() {
        assert_eq!(std::mem::size_of::<BinaryValue>(), 16);
    }

    #[test]
    fn test_new() {
        assert_eq!(
            BinaryValue::new(14, 16.try_into().unwrap()),
            Some(BinaryValue::Smol {
                data: 14,
                bits: 16.try_into().unwrap()
            })
        );
        assert_eq!(
            BinaryValue::new(1u128 << 120, 128.try_into().unwrap()),
            Some(BinaryValue::BigBoi {
                data: Box::new(1 << 120),
                bits: 128.try_into().unwrap()
            })
        );
        assert_eq!(BinaryValue::new(14, 2.try_into().unwrap()), None);
        assert_matches!(BinaryValue::new(15, 4.try_into().unwrap()), Some(_));
        assert_eq!(BinaryValue::new(16, 4.try_into().unwrap()), None);
        assert_matches!(BinaryValue::new(16, 128.try_into().unwrap()), Some(_));
        assert_matches!(BinaryValue::new(16, 129.try_into().unwrap()), None);
    }
}
