// Runtime value type for the pattern language evaluator

use std::fmt;
use std::sync::Arc;

use crate::error::EvalError;
use crate::parser::ast::{BuiltinType, Endianness};

/// Runtime value during evaluation
#[derive(Debug, Clone)]
pub enum Value {
    Unsigned(u128),
    Signed(i128),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Null,
    /// Array of values (from initializer lists)
    Array(Vec<Value>),
    /// Reference to a sized pattern node (struct, union, array, bitfield)
    SizedRef {
        offset: u64,
        size: u64,
    },
    /// Bulk data reference for large fixed-size builtin arrays.
    /// Carries the raw buffer so index access can decode directly.
    BulkRef {
        offset: u64,
        size: u64,
        data: Arc<Vec<u8>>,
        elem_type: BuiltinType,
        endian: Endianness,
    },
}

impl Value {
    /// Convert to unsigned integer
    #[inline]
    pub fn to_unsigned(&self) -> Result<u128, EvalError> {
        match self {
            Value::Unsigned(v) => Ok(*v),
            Value::Signed(v) => Ok(*v as u128),
            Value::Float(v) => Ok(*v as u128),
            Value::Bool(v) => Ok(if *v { 1 } else { 0 }),
            Value::Char(v) => Ok(*v as u128),
            Value::SizedRef { offset, .. } | Value::BulkRef { offset, .. } => Ok(*offset as u128),
            Value::Null => Ok(0),
            // Convert String to unsigned: treat raw bytes as little-endian integer (FourCC-style)
            Value::String(s) if !s.is_empty() && s.len() <= 16 => {
                let mut v: u128 = 0;
                for (i, b) in s.bytes().enumerate() {
                    v |= (b as u128) << (i * 8);
                }
                Ok(v)
            }
            _ => Err(EvalError::new(format!(
                "cannot convert {:?} to unsigned",
                self
            ))),
        }
    }

    /// Convert to signed integer
    #[inline]
    pub fn to_signed(&self) -> Result<i128, EvalError> {
        match self {
            Value::Unsigned(v) => Ok(*v as i128),
            Value::Signed(v) => Ok(*v),
            Value::Float(v) => Ok(*v as i128),
            Value::Bool(v) => Ok(if *v { 1 } else { 0 }),
            Value::Char(v) => Ok(*v as i128),
            Value::SizedRef { offset, .. } | Value::BulkRef { offset, .. } => Ok(*offset as i128),
            Value::Null => Ok(0),
            _ => Err(EvalError::new(format!(
                "cannot convert {:?} to signed",
                self
            ))),
        }
    }

    /// Convert to float
    pub fn to_float(&self) -> Result<f64, EvalError> {
        match self {
            Value::Unsigned(v) => Ok(*v as f64),
            Value::Signed(v) => Ok(*v as f64),
            Value::Float(v) => Ok(*v),
            Value::Bool(v) => Ok(if *v { 1.0 } else { 0.0 }),
            _ => Err(EvalError::new(format!(
                "cannot convert {:?} to float",
                self
            ))),
        }
    }

    /// Convert to boolean
    #[inline]
    pub fn to_bool(&self) -> Result<bool, EvalError> {
        match self {
            Value::Unsigned(v) => Ok(*v != 0),
            Value::Signed(v) => Ok(*v != 0),
            Value::Float(v) => Ok(*v != 0.0),
            Value::Bool(v) => Ok(*v),
            Value::Char(v) => Ok(*v != '\0'),
            Value::String(s) => Ok(!s.is_empty()),
            Value::Null => Ok(false),
            Value::Array(items) => Ok(!items.is_empty()),
            Value::SizedRef { .. } | Value::BulkRef { .. } => Ok(true),
        }
    }

    /// Convert to string representation
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Unsigned(v) => format!("{}", v),
            Value::Signed(v) => format!("{}", v),
            Value::Float(v) => format!("{}", v),
            Value::Bool(v) => format!("{}", v),
            Value::Char(v) => format!("{}", v),
            Value::String(v) => v.clone(),
            Value::Null => "null".to_string(),
            Value::Array(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.to_display_string()).collect();
                format!("[{}]", inner.join(", "))
            }
            Value::SizedRef { offset, size } => format!("<ref@{:#x} size={}>", offset, size),
            Value::BulkRef { offset, size, .. } => {
                format!("<bulk@{:#x} size={}>", offset, size)
            }
        }
    }

    /// Check if this is a numeric type
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Value::Unsigned(_) | Value::Signed(_) | Value::Float(_)
        )
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unsigned(a), Value::Unsigned(b)) => a == b,
            (Value::Signed(a), Value::Signed(b)) => a == b,
            (Value::Unsigned(a), Value::Signed(b)) => (*a as i128) == *b,
            (Value::Signed(a), Value::Unsigned(b)) => *a == (*b as i128),
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Array(a), Value::Array(b)) => a == b,
            (
                Value::SizedRef {
                    offset: o1,
                    size: s1,
                },
                Value::SizedRef {
                    offset: o2,
                    size: s2,
                },
            ) => o1 == o2 && s1 == s2,
            (
                Value::BulkRef {
                    offset: o1,
                    size: s1,
                    ..
                },
                Value::BulkRef {
                    offset: o2,
                    size: s2,
                    ..
                },
            ) => o1 == o2 && s1 == s2,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unsigned_to_bool() {
        assert!(Value::Unsigned(1).to_bool().unwrap());
        assert!(!Value::Unsigned(0).to_bool().unwrap());
    }

    #[test]
    fn test_signed_to_unsigned() {
        assert_eq!(Value::Signed(42).to_unsigned().unwrap(), 42);
    }

    #[test]
    fn test_float_to_signed() {
        assert_eq!(Value::Float(3.7).to_signed().unwrap(), 3);
    }

    #[test]
    fn test_value_equality() {
        assert_eq!(Value::Unsigned(5), Value::Unsigned(5));
        assert_eq!(Value::Unsigned(5), Value::Signed(5));
        assert_ne!(Value::Unsigned(5), Value::Unsigned(6));
    }

    #[test]
    fn test_null_to_bool() {
        assert!(!Value::Null.to_bool().unwrap());
    }

    #[test]
    fn test_null_to_unsigned() {
        assert_eq!(Value::Null.to_unsigned().unwrap(), 0);
    }

    #[test]
    fn test_null_to_signed() {
        assert_eq!(Value::Null.to_signed().unwrap(), 0);
    }

    #[test]
    fn test_string_to_unsigned_fourcc() {
        // FourCC-style: "RIFF" → 0x46464952 (little-endian byte order)
        let val = Value::String("RIFF".to_string());
        let result = val.to_unsigned().unwrap();
        assert_eq!(result, 0x46464952);
    }

    #[test]
    fn test_string_to_unsigned_single_char() {
        let val = Value::String("A".to_string());
        assert_eq!(val.to_unsigned().unwrap(), 0x41);
    }

    #[test]
    fn test_string_to_unsigned_empty_fails() {
        let val = Value::String(String::new());
        assert!(val.to_unsigned().is_err());
    }

    #[test]
    fn test_string_to_unsigned_too_long_fails() {
        let val = Value::String("a".repeat(17));
        assert!(val.to_unsigned().is_err());
    }
}
