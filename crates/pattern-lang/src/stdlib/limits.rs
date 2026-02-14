// std::limits - type min/max constants

use crate::eval::scope::Scope;
use crate::eval::value::Value;
use crate::name::StringInterner;

macro_rules! register_limit {
    ($scope:expr, $interner:expr, $name:literal, $variant:ident, $value:expr) => {
        $scope.register_builtin(
            $interner.intern(concat!("builtin::std::limits::", $name)),
            |_| Ok(Value::$variant($value)),
        );
    };
}

pub fn register(scope: &mut Scope, interner: &mut StringInterner) {
    // Unsigned max values
    register_limit!(scope, interner, "u8_max", Unsigned, u8::MAX as u128);
    register_limit!(scope, interner, "u16_max", Unsigned, u16::MAX as u128);
    register_limit!(scope, interner, "u32_max", Unsigned, u32::MAX as u128);
    register_limit!(scope, interner, "u64_max", Unsigned, u64::MAX as u128);
    register_limit!(scope, interner, "u128_max", Unsigned, u128::MAX);

    // Unsigned min values (always 0)
    register_limit!(scope, interner, "u8_min", Unsigned, 0);
    register_limit!(scope, interner, "u16_min", Unsigned, 0);
    register_limit!(scope, interner, "u32_min", Unsigned, 0);
    register_limit!(scope, interner, "u64_min", Unsigned, 0);
    register_limit!(scope, interner, "u128_min", Unsigned, 0);

    // Signed max values
    register_limit!(scope, interner, "s8_max", Signed, i8::MAX as i128);
    register_limit!(scope, interner, "s16_max", Signed, i16::MAX as i128);
    register_limit!(scope, interner, "s32_max", Signed, i32::MAX as i128);
    register_limit!(scope, interner, "s64_max", Signed, i64::MAX as i128);
    register_limit!(scope, interner, "s128_max", Signed, i128::MAX);

    // Signed min values
    register_limit!(scope, interner, "s8_min", Signed, i8::MIN as i128);
    register_limit!(scope, interner, "s16_min", Signed, i16::MIN as i128);
    register_limit!(scope, interner, "s32_min", Signed, i32::MIN as i128);
    register_limit!(scope, interner, "s64_min", Signed, i64::MIN as i128);
    register_limit!(scope, interner, "s128_min", Signed, i128::MIN);
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_limit {
        ($test_name:ident, $limit_name:literal, $expected:expr) => {
            #[test]
            fn $test_name() {
                let mut interner = StringInterner::new();
                let mut scope = Scope::new();
                register(&mut scope, &mut interner);
                let name = interner.intern(concat!("builtin::std::limits::", $limit_name));
                let result = scope.call_builtin(name, &[]).unwrap().unwrap();
                assert_eq!(result, $expected);
            }
        };
    }

    test_limit!(test_u8_max, "u8_max", Value::Unsigned(u8::MAX as u128));
    test_limit!(test_u16_max, "u16_max", Value::Unsigned(u16::MAX as u128));
    test_limit!(test_u32_max, "u32_max", Value::Unsigned(u32::MAX as u128));
    test_limit!(test_u64_max, "u64_max", Value::Unsigned(u64::MAX as u128));
    test_limit!(test_u128_max, "u128_max", Value::Unsigned(u128::MAX));

    test_limit!(test_u8_min, "u8_min", Value::Unsigned(0));
    test_limit!(test_u16_min, "u16_min", Value::Unsigned(0));
    test_limit!(test_u32_min, "u32_min", Value::Unsigned(0));
    test_limit!(test_u64_min, "u64_min", Value::Unsigned(0));
    test_limit!(test_u128_min, "u128_min", Value::Unsigned(0));

    test_limit!(test_s8_max, "s8_max", Value::Signed(i8::MAX as i128));
    test_limit!(test_s16_max, "s16_max", Value::Signed(i16::MAX as i128));
    test_limit!(test_s32_max, "s32_max", Value::Signed(i32::MAX as i128));
    test_limit!(test_s64_max, "s64_max", Value::Signed(i64::MAX as i128));
    test_limit!(test_s128_max, "s128_max", Value::Signed(i128::MAX));

    test_limit!(test_s8_min, "s8_min", Value::Signed(i8::MIN as i128));
    test_limit!(test_s16_min, "s16_min", Value::Signed(i16::MIN as i128));
    test_limit!(test_s32_min, "s32_min", Value::Signed(i32::MIN as i128));
    test_limit!(test_s64_min, "s64_min", Value::Signed(i64::MIN as i128));
    test_limit!(test_s128_min, "s128_min", Value::Signed(i128::MIN));
}
