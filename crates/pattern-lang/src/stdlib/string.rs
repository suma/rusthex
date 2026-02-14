// std::string - string manipulation functions

use crate::eval::scope::Scope;
use crate::eval::value::Value;
use crate::name::StringInterner;

pub fn register(scope: &mut Scope, interner: &mut StringInterner) {
    scope.register_builtin(interner.intern("builtin::std::string::length"), |args| {
        let s = match args.first() {
            Some(Value::String(s)) => s.clone(),
            _ => return Err("length requires a string argument".into()),
        };
        Ok(Value::Unsigned(s.len() as u128))
    });

    scope.register_builtin(interner.intern("builtin::std::string::at"), |args| {
        if args.len() < 2 {
            return Err("at requires 2 arguments (string, index)".into());
        }
        let s = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err("at requires a string as first argument".into()),
        };
        let idx = args[1].to_unsigned().map_err(|e| e.to_string())? as usize;
        match s.chars().nth(idx) {
            Some(c) => Ok(Value::Char(c)),
            None => Err(format!(
                "string index {} out of bounds (length {})",
                idx,
                s.len()
            )),
        }
    });

    scope.register_builtin(interner.intern("builtin::std::string::substr"), |args| {
        if args.len() < 3 {
            return Err("substr requires 3 arguments (string, start, length)".into());
        }
        let s = match &args[0] {
            Value::String(s) => s.clone(),
            // Accept other types by converting to display string
            other => other.to_display_string(),
        };
        let start = args[1].to_unsigned().map_err(|e| e.to_string())? as usize;
        let len = args[2].to_unsigned().map_err(|e| e.to_string())? as usize;
        let result: String = s.chars().skip(start).take(len).collect();
        Ok(Value::String(result))
    });

    scope.register_builtin(interner.intern("builtin::std::string::contains"), |args| {
        if args.len() < 2 {
            return Err("contains requires 2 arguments".into());
        }
        let haystack = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err("contains requires strings".into()),
        };
        let needle = match &args[1] {
            Value::String(s) => s.clone(),
            _ => return Err("contains requires strings".into()),
        };
        Ok(Value::Bool(haystack.contains(&needle)))
    });

    scope.register_builtin(
        interner.intern("builtin::std::string::starts_with"),
        |args| {
            if args.len() < 2 {
                return Err("starts_with requires 2 arguments".into());
            }
            let s = match &args[0] {
                Value::String(s) => s.clone(),
                _ => return Err("starts_with requires strings".into()),
            };
            let prefix = match &args[1] {
                Value::String(s) => s.clone(),
                _ => return Err("starts_with requires strings".into()),
            };
            Ok(Value::Bool(s.starts_with(&prefix)))
        },
    );

    scope.register_builtin(interner.intern("builtin::std::string::ends_with"), |args| {
        if args.len() < 2 {
            return Err("ends_with requires 2 arguments".into());
        }
        let s = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err("ends_with requires strings".into()),
        };
        let suffix = match &args[1] {
            Value::String(s) => s.clone(),
            _ => return Err("ends_with requires strings".into()),
        };
        Ok(Value::Bool(s.ends_with(&suffix)))
    });

    scope.register_builtin(interner.intern("builtin::std::string::to_string"), |args| {
        let v = args.first().ok_or("to_string requires 1 argument")?;
        Ok(Value::String(v.to_display_string()))
    });

    scope.register_builtin(interner.intern("builtin::std::string::parse_int"), |args| {
        let s = match args.first() {
            Some(Value::String(s)) => s.clone(),
            // Accept numeric values directly (convert to string for parsing)
            Some(v) if v.is_numeric() => v.to_display_string(),
            Some(Value::Char(c)) => c.to_string(),
            // SizedRef from char[] arrays that couldn't be converted to string
            Some(Value::SizedRef { .. }) => String::new(),
            // Array values: try to convert chars to string
            Some(Value::Array(items)) => items
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    Value::Unsigned(n) => Some(*n as u8 as char),
                    _ => None,
                })
                .take_while(|c| *c != '\0')
                .collect(),
            Some(Value::Null) => String::new(),
            _ => return Err("parse_int requires a string argument".into()),
        };
        let base = if args.len() > 1 {
            args[1].to_unsigned().map_err(|e| e.to_string())? as u32
        } else {
            10
        };
        // Strip null terminators and whitespace (char[] arrays often include trailing \0)
        let cleaned: String = s.chars().take_while(|c| *c != '\0').collect();
        let trimmed = cleaned.trim();
        // Empty string (e.g. all-null CPIO fields) → 0
        if trimmed.is_empty() {
            return Ok(Value::Signed(0));
        }
        let val =
            i128::from_str_radix(trimmed, base).map_err(|e| format!("parse_int failed: {}", e))?;
        Ok(Value::Signed(val))
    });

    scope.register_builtin(
        interner.intern("builtin::std::string::parse_float"),
        |args| {
            let s = match args.first() {
                Some(Value::String(s)) => s.clone(),
                _ => return Err("parse_float requires a string argument".into()),
            };
            let val: f64 = s
                .trim()
                .parse()
                .map_err(|e: std::num::ParseFloatError| format!("parse_float failed: {}", e))?;
            Ok(Value::Float(val))
        },
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_length() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::length");
        let result = scope
            .call_builtin(name, &[Value::String("hello".into())])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Unsigned(5));
    }

    #[test]
    fn test_string_contains() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::contains");
        let result = scope
            .call_builtin(
                name,
                &[
                    Value::String("hello world".into()),
                    Value::String("world".into()),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_string_substr() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::substr");
        let result = scope
            .call_builtin(
                name,
                &[
                    Value::String("hello".into()),
                    Value::Unsigned(1),
                    Value::Unsigned(3),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::String("ell".into()));
    }

    #[test]
    fn test_string_parse_int() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::String("42".into())])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(42));
    }

    #[test]
    fn test_string_at() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::at");
        let result = scope
            .call_builtin(name, &[Value::String("hello".into()), Value::Unsigned(1)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Char('e'));
    }

    #[test]
    fn test_string_starts_with() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::starts_with");
        let result = scope
            .call_builtin(
                name,
                &[
                    Value::String("hello world".into()),
                    Value::String("hello".into()),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Bool(true));

        let result = scope
            .call_builtin(
                name,
                &[
                    Value::String("hello world".into()),
                    Value::String("world".into()),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_string_ends_with() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::ends_with");
        let result = scope
            .call_builtin(
                name,
                &[
                    Value::String("hello world".into()),
                    Value::String("world".into()),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Bool(true));

        let result = scope
            .call_builtin(
                name,
                &[
                    Value::String("hello world".into()),
                    Value::String("hello".into()),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_string_to_string() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::to_string");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(42)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::String("42".into()));

        let result = scope
            .call_builtin(name, &[Value::Bool(true)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::String("true".into()));
    }

    #[test]
    fn test_string_parse_float() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_float");
        let result = scope
            .call_builtin(name, &[Value::String("3.14".into())])
            .unwrap()
            .unwrap();
        match result {
            Value::Float(f) => assert!((f - 3.14).abs() < 0.001),
            other => panic!("expected Float, got {:?}", other),
        }
    }

    #[test]
    fn test_string_parse_int_empty_returns_zero() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::String("".into()), Value::Unsigned(16)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(0));
    }

    #[test]
    fn test_string_parse_int_null_terminated() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::String("FF\0\0".into()), Value::Unsigned(16)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(255));
    }

    #[test]
    fn test_string_parse_int_whitespace_only_returns_zero() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::String("   ".into()), Value::Unsigned(10)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(0));
    }

    #[test]
    fn test_string_parse_int_numeric_argument() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(42), Value::Unsigned(10)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(42));
    }

    #[test]
    fn test_string_parse_int_hex_with_base() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::String("1A".into()), Value::Unsigned(16)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(26));
    }

    #[test]
    fn test_string_parse_int_octal() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::String("77".into()), Value::Unsigned(8)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(63));
    }

    #[test]
    fn test_string_parse_int_sized_ref_returns_zero() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(
                name,
                &[Value::SizedRef { offset: 0, size: 8 }, Value::Unsigned(16)],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(0));
    }

    #[test]
    fn test_string_parse_int_array_of_chars() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(
                name,
                &[
                    Value::Array(vec![Value::Char('F'), Value::Char('F'), Value::Char('\0')]),
                    Value::Unsigned(16),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(255));
    }

    #[test]
    fn test_string_parse_int_null_returns_zero() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::string::parse_int");
        let result = scope
            .call_builtin(name, &[Value::Null, Value::Unsigned(10)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(0));
    }
}
