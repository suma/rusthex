// std::io - I/O functions (print, format)

use crate::eval::scope::Scope;
use crate::eval::value::Value;
use crate::name::StringInterner;

pub fn register(scope: &mut Scope, interner: &mut StringInterner) {
    // std::io::print - print values to output
    // Note: actual output is handled by the evaluator's print_output buffer
    scope.register_builtin(interner.intern("builtin::std::io::print"), |args| {
        let _msg: String = args
            .iter()
            .map(|v| v.to_display_string())
            .collect::<Vec<_>>()
            .join("");
        // The evaluator will intercept "print" calls and add to print_output
        Ok(Value::Null)
    });

    // std::io::format - format a string with arguments
    scope.register_builtin(interner.intern("builtin::std::io::format"), |args| {
        if args.is_empty() {
            return Err("format requires at least 1 argument".into());
        }
        let fmt = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err("format requires a format string".into()),
        };

        // Simple format: replace {} with arguments in order
        let mut result = String::new();
        let mut arg_idx = 1;
        let mut chars = fmt.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '{' && chars.peek() == Some(&'}') {
                chars.next(); // consume '}'
                if arg_idx < args.len() {
                    result.push_str(&args[arg_idx].to_display_string());
                    arg_idx += 1;
                } else {
                    result.push_str("{}");
                }
            } else {
                result.push(c);
            }
        }

        Ok(Value::String(result))
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_io_format() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::io::format");
        let result = scope
            .call_builtin(
                name,
                &[
                    Value::String("Hello {}! Count: {}".into()),
                    Value::String("World".into()),
                    Value::Unsigned(42),
                ],
            )
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::String("Hello World! Count: 42".into()));
    }

    #[test]
    fn test_io_format_no_args() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::io::format");
        let result = scope
            .call_builtin(name, &[Value::String("plain text".into())])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::String("plain text".into()));
    }

    #[test]
    fn test_io_print() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::io::print");
        let result = scope
            .call_builtin(name, &[Value::String("hello".into()), Value::Unsigned(42)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Null);
    }
}
