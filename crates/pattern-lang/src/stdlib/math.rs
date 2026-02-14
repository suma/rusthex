// std::math - mathematical functions

use crate::eval::scope::Scope;
use crate::eval::value::Value;
use crate::name::StringInterner;

pub fn register(scope: &mut Scope, interner: &mut StringInterner) {
    scope.register_builtin(interner.intern("builtin::std::math::floor"), |args| {
        let v = args
            .first()
            .ok_or("floor requires 1 argument")?
            .to_float()
            .map_err(|e| e.to_string())?;
        Ok(Value::Float(v.floor()))
    });

    scope.register_builtin(interner.intern("builtin::std::math::ceil"), |args| {
        let v = args
            .first()
            .ok_or("ceil requires 1 argument")?
            .to_float()
            .map_err(|e| e.to_string())?;
        Ok(Value::Float(v.ceil()))
    });

    scope.register_builtin(interner.intern("builtin::std::math::round"), |args| {
        let v = args
            .first()
            .ok_or("round requires 1 argument")?
            .to_float()
            .map_err(|e| e.to_string())?;
        Ok(Value::Float(v.round()))
    });

    scope.register_builtin(interner.intern("builtin::std::math::abs"), |args| {
        let v = args.first().ok_or("abs requires 1 argument")?;
        match v {
            Value::Float(f) => Ok(Value::Float(f.abs())),
            Value::Signed(i) => Ok(Value::Signed(i.abs())),
            Value::Unsigned(u) => Ok(Value::Unsigned(*u)),
            _ => Err("abs requires a numeric argument".into()),
        }
    });

    scope.register_builtin(interner.intern("builtin::std::math::min"), |args| {
        if args.len() < 2 {
            return Err("min requires 2 arguments".into());
        }
        let a = args[0].to_float().map_err(|e| e.to_string())?;
        let b = args[1].to_float().map_err(|e| e.to_string())?;
        Ok(Value::Float(a.min(b)))
    });

    scope.register_builtin(interner.intern("builtin::std::math::max"), |args| {
        if args.len() < 2 {
            return Err("max requires 2 arguments".into());
        }
        let a = args[0].to_float().map_err(|e| e.to_string())?;
        let b = args[1].to_float().map_err(|e| e.to_string())?;
        Ok(Value::Float(a.max(b)))
    });

    scope.register_builtin(interner.intern("builtin::std::math::sqrt"), |args| {
        let v = args
            .first()
            .ok_or("sqrt requires 1 argument")?
            .to_float()
            .map_err(|e| e.to_string())?;
        Ok(Value::Float(v.sqrt()))
    });

    scope.register_builtin(interner.intern("builtin::std::math::pow"), |args| {
        if args.len() < 2 {
            return Err("pow requires 2 arguments".into());
        }
        let base = args[0].to_float().map_err(|e| e.to_string())?;
        let exp = args[1].to_float().map_err(|e| e.to_string())?;
        Ok(Value::Float(base.powf(exp)))
    });

    scope.register_builtin(interner.intern("builtin::std::math::log2"), |args| {
        let v = args
            .first()
            .ok_or("log2 requires 1 argument")?
            .to_float()
            .map_err(|e| e.to_string())?;
        Ok(Value::Float(v.log2()))
    });

    scope.register_builtin(interner.intern("builtin::std::math::log10"), |args| {
        let v = args
            .first()
            .ok_or("log10 requires 1 argument")?
            .to_float()
            .map_err(|e| e.to_string())?;
        Ok(Value::Float(v.log10()))
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_math_floor() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::floor");
        let result = scope
            .call_builtin(name, &[Value::Float(3.7)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(3.0));
    }

    #[test]
    fn test_math_ceil() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::ceil");
        let result = scope
            .call_builtin(name, &[Value::Float(3.2)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(4.0));
    }

    #[test]
    fn test_math_sqrt() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::sqrt");
        let result = scope
            .call_builtin(name, &[Value::Float(16.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(4.0));
    }

    #[test]
    fn test_math_pow() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::pow");
        let result = scope
            .call_builtin(name, &[Value::Float(2.0), Value::Float(10.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(1024.0));
    }

    #[test]
    fn test_math_abs_signed() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::abs");
        let result = scope
            .call_builtin(name, &[Value::Signed(-42)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Signed(42));
    }

    #[test]
    fn test_math_round() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::round");
        let result = scope
            .call_builtin(name, &[Value::Float(3.5)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(4.0));

        let result = scope
            .call_builtin(name, &[Value::Float(2.3)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(2.0));
    }

    #[test]
    fn test_math_min() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::min");
        let result = scope
            .call_builtin(name, &[Value::Float(3.0), Value::Float(7.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(3.0));

        let result = scope
            .call_builtin(name, &[Value::Float(-1.0), Value::Float(1.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(-1.0));
    }

    #[test]
    fn test_math_max() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::max");
        let result = scope
            .call_builtin(name, &[Value::Float(3.0), Value::Float(7.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(7.0));

        let result = scope
            .call_builtin(name, &[Value::Float(-1.0), Value::Float(1.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(1.0));
    }

    #[test]
    fn test_math_log2() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::log2");
        let result = scope
            .call_builtin(name, &[Value::Float(8.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(3.0));

        let result = scope
            .call_builtin(name, &[Value::Float(1.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(0.0));
    }

    #[test]
    fn test_math_log10() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::math::log10");
        let result = scope
            .call_builtin(name, &[Value::Float(1000.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(3.0));

        let result = scope
            .call_builtin(name, &[Value::Float(1.0)])
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Float(0.0));
    }
}
