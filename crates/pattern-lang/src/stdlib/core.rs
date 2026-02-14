// std::core - core utility functions

use crate::eval::scope::Scope;
use crate::eval::value::Value;
use crate::name::StringInterner;

pub fn register(scope: &mut Scope, interner: &mut StringInterner) {
    // std::core::align_to(alignment) - returns padding bytes needed
    scope.register_builtin(interner.intern("builtin::std::core::align_to"), |args| {
        if args.is_empty() {
            return Err("std::core::align_to requires 1 argument".into());
        }
        let _alignment = args[0].to_unsigned().map_err(|e| e.to_string())?;
        // This would need the current offset to compute padding;
        // for now return 0 as a placeholder
        Ok(Value::Unsigned(0))
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_core_align_to() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        register(&mut scope, &mut interner);
        let name = interner.intern("builtin::std::core::align_to");
        let result = scope
            .call_builtin(name, &[Value::Unsigned(4)])
            .unwrap()
            .unwrap();
        // Placeholder returns 0
        assert_eq!(result, Value::Unsigned(0));
    }
}
