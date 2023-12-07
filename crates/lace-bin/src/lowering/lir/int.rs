use bnum::types::U512;

use std::num::NonZeroU16;
use std::str::FromStr;
use thiserror::Error;

/// Represents a constant binary value. This can be a float, integer
/// char or boolean. Anything really. Typecheck information on nodes
/// can disambiguate what operation to apply to values of this type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryValue {
    /// if values are less than 64 bits we can easily store it inline
    Smol {
        data: u64,
        bits: NonZeroU16,
        signed: bool,
    },
    /// however, less common big values we cannot and we store these
    /// as a pointer instead. TODO: this may turn out to be a mistake
    BigBoi {
        data: Box<U512>,
        bits: NonZeroU16,
        signed: bool,
    },
}

impl BinaryValue {
    // TODO: const unwrap is nightly. this is nicer for now than
    //       a match and a const panic, but be this needs to be fixed in the future
    pub const TRUE: Self = BinaryValue::Smol {
        data: 1,
        bits: unsafe { NonZeroU16::new_unchecked(1) },
        signed: false,
    };
    pub const FALSE: Self = BinaryValue::Smol {
        data: 0,
        bits: unsafe { NonZeroU16::new_unchecked(1) },
        signed: false,
    };

    pub fn new(value: impl Into<U512>, bits: NonZeroU16, signed: bool) -> Option<Self> {
        // TODO: is masking different when signed?
        let value = value.into();

        let mask = match bits.get() {
            i @ 0..=127 => (U512::ONE << i) - U512::ONE,
            128 => U512::MAX,
            _ => return None,
        };

        let truncated_value = value & mask;
        if truncated_value != value {
            return None;
        }

        Some(if bits.get() <= 64 {
            Self::Smol {
                data: truncated_value
                    .try_into()
                    .unwrap_or_else(|e| lice!("too many bits, somehow?: {e:?}")),
                bits,
                signed,
            }
        } else {
            Self::BigBoi {
                data: Box::new(truncated_value),
                bits,
                signed,
            }
        })
    }

    pub fn from_str(s: &str, bits: NonZeroU16, signed: bool) -> Result<Self, ParseIntError> {
        let num = U512::from_str(s).map_err(ParseIntError)?;

        Ok(match Self::new(num, bits, signed) {
            Some(i) => i,
            None => todo!(), // throw a nice compile error
        })
    }
}

macro_rules! from {
    ($ty: ty, $bits: literal, $signed: literal) => {
        impl From<$ty> for BinaryValue {
            fn from(value: $ty) -> Self {
                Self::Smol {
                    data: value as u64,
                    bits: NonZeroU16::new($bits).unwrap(),
                    signed: $signed,
                }
            }
        }
    };
}

from!(u64, 64, false);
from!(u32, 32, false);
from!(u16, 16, false);
from!(u8, 8, false);

from!(i64, 64, true);
from!(i32, 32, true);
from!(i16, 16, true);
from!(i8, 8, true);

#[derive(Debug, Error)]
#[error("parse int: {0:?}")]
pub struct ParseIntError(bnum::errors::ParseIntError);

impl From<i128> for BinaryValue {
    fn from(value: i128) -> Self {
        Self::BigBoi {
            data: Box::new((value as u128).into()),
            bits: NonZeroU16::new(128).unwrap(),
            signed: true,
        }
    }
}

impl From<u128> for BinaryValue {
    fn from(value: u128) -> Self {
        Self::BigBoi {
            data: Box::new(value.into()),
            bits: NonZeroU16::new(128).unwrap(),
            signed: false,
        }
    }
}

impl From<f64> for BinaryValue {
    fn from(value: f64) -> Self {
        Self::Smol {
            data: value.to_bits(),
            bits: NonZeroU16::new(64).unwrap(),
            signed: false,
        }
    }
}

impl From<f32> for BinaryValue {
    fn from(value: f32) -> Self {
        Self::Smol {
            data: value.to_bits() as u64,
            bits: NonZeroU16::new(32).unwrap(),
            signed: false,
        }
    }
}

impl From<bool> for BinaryValue {
    fn from(value: bool) -> Self {
        Self::Smol {
            data: if value { 1 } else { 0 },
            bits: NonZeroU16::new(1).unwrap(),
            signed: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lowering::lir::int::BinaryValue;
    use bnum::types::U512;

    #[test]
    pub fn int_size() {
        assert_eq!(std::mem::size_of::<BinaryValue>(), 16);
    }

    #[test]
    fn test_new() {
        assert_eq!(
            BinaryValue::new(14u32, 16.try_into().unwrap(), false),
            Some(BinaryValue::Smol {
                data: 14,
                bits: 16.try_into().unwrap(),
                signed: false,
            })
        );
        assert_eq!(
            BinaryValue::new(1u128 << 120, 128.try_into().unwrap(), false),
            Some(BinaryValue::BigBoi {
                data: Box::new(U512::ONE << 120),
                bits: 128.try_into().unwrap(),
                signed: false,
            })
        );
        assert_eq!(BinaryValue::new(14u32, 2.try_into().unwrap(), false), None);
        assert_matches!(
            BinaryValue::new(15u32, 4.try_into().unwrap(), false),
            Some(_)
        );
        assert_eq!(BinaryValue::new(16u32, 4.try_into().unwrap(), false), None);
        assert_matches!(
            BinaryValue::new(16u32, 128.try_into().unwrap(), false),
            Some(_)
        );
        assert_matches!(
            BinaryValue::new(16u32, 129.try_into().unwrap(), false),
            None
        );
    }
}
