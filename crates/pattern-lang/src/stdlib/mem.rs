// std::mem - memory utility functions

use crate::eval::scope::Scope;
use crate::eval::value::Value;
use crate::name::StringInterner;

pub fn register(scope: &mut Scope, interner: &mut StringInterner, data_size: u64) {
    let size = data_size;
    scope.register_builtin(interner.intern("builtin::std::mem::size"), move |_args| {
        Ok(Value::Unsigned(size as u128))
    });

    scope.register_builtin(
        interner.intern("builtin::std::mem::base_address"),
        |_args| Ok(Value::Unsigned(0)),
    );

    scope.register_builtin(interner.intern("builtin::std::mem::eof"), |_args| {
        // Placeholder: proper implementation needs current offset context
        Ok(Value::Bool(false))
    });

    scope.register_builtin(
        interner.intern("builtin::std::mem::read_unsigned"),
        |args| {
            if args.len() < 2 {
                return Err("std::mem::read_unsigned requires 2 arguments (offset, size)".into());
            }
            let _offset = args[0].to_unsigned().map_err(|e| e.to_string())?;
            let _size = args[1].to_unsigned().map_err(|e| e.to_string())?;
            // This would need access to the DataSource; placeholder
            Ok(Value::Unsigned(0))
        },
    );

    scope.register_builtin(interner.intern("builtin::std::mem::read_signed"), |args| {
        if args.len() < 2 {
            return Err("std::mem::read_signed requires 2 arguments (offset, size)".into());
        }
        Ok(Value::Signed(0))
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mem_size() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner, 1024);
        let name = interner.intern("builtin::std::mem::size");
        let result = scope.call_builtin(name, &[]).unwrap().unwrap();
        assert_eq!(result, Value::Unsigned(1024));
    }

    #[test]
    fn test_mem_base_address() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner, 0);
        let name = interner.intern("builtin::std::mem::base_address");
        let result = scope.call_builtin(name, &[]).unwrap().unwrap();
        assert_eq!(result, Value::Unsigned(0));
    }

    #[test]
    fn test_mem_eof() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner, 100);
        let name = interner.intern("builtin::std::mem::eof");
        let result = scope.call_builtin(name, &[]).unwrap().unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_mem_read_unsigned() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner, 16);
        let name = interner.intern("builtin::std::mem::read_unsigned");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(0), Value::Unsigned(1)])
            .unwrap()
            .unwrap();
        // Placeholder returns 0
        assert_eq!(result, Value::Unsigned(0));
    }

    #[test]
    fn test_mem_read_signed() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner, 16);
        let name = interner.intern("builtin::std::mem::read_signed");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(0), Value::Unsigned(1)])
            .unwrap()
            .unwrap();
        // Placeholder returns 0
        assert_eq!(result, Value::Signed(0));
    }
}
