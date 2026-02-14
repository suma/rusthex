// std::bit - bit manipulation functions

use crate::eval::scope::Scope;
use crate::eval::value::Value;
use crate::name::StringInterner;

pub fn register(scope: &mut Scope, interner: &mut StringInterner) {
    scope.register_builtin(interner.intern("builtin::std::bit::popcount"), |args| {
        let v = args
            .first()
            .ok_or("popcount requires 1 argument")?
            .to_unsigned()
            .map_err(|e| e.to_string())?;
        Ok(Value::Unsigned(v.count_ones() as u128))
    });

    scope.register_builtin(interner.intern("builtin::std::bit::has_bit"), |args| {
        if args.len() < 2 {
            return Err("has_bit requires 2 arguments (value, bit)".into());
        }
        let v = args[0].to_unsigned().map_err(|e| e.to_string())?;
        let bit = args[1].to_unsigned().map_err(|e| e.to_string())?;
        Ok(Value::Bool((v >> bit) & 1 == 1))
    });

    scope.register_builtin(interner.intern("builtin::std::bit::set_bit"), |args| {
        if args.len() < 2 {
            return Err("set_bit requires 2 arguments (value, bit)".into());
        }
        let v = args[0].to_unsigned().map_err(|e| e.to_string())?;
        let bit = args[1].to_unsigned().map_err(|e| e.to_string())?;
        Ok(Value::Unsigned(v | (1 << bit)))
    });

    scope.register_builtin(interner.intern("builtin::std::bit::clear_bit"), |args| {
        if args.len() < 2 {
            return Err("clear_bit requires 2 arguments (value, bit)".into());
        }
        let v = args[0].to_unsigned().map_err(|e| e.to_string())?;
        let bit = args[1].to_unsigned().map_err(|e| e.to_string())?;
        Ok(Value::Unsigned(v & !(1 << bit)))
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_popcount() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::bit::popcount");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(0b1010_1010)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Unsigned(4));
    }

    #[test]
    fn test_has_bit() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::bit::has_bit");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(0b1010), Value::Unsigned(1)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Bool(true));
        let result = scope
            .call_builtin(name, &[Value::Unsigned(0b1010), Value::Unsigned(0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_set_bit() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::bit::set_bit");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(0), Value::Unsigned(3)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Unsigned(8));
    }

    #[test]
    fn test_clear_bit() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::bit::clear_bit");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(0xFF), Value::Unsigned(0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Unsigned(0xFE));
    }
}
