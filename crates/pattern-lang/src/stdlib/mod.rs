// Standard library for the pattern language

pub mod bit;
pub mod core;
pub mod io;
pub mod limits;
pub mod math;
pub mod mem;
pub mod string;

use crate::eval::scope::Scope;
use crate::name::StringInterner;

/// Register all standard library functions into a scope
pub fn register_all(scope: &mut Scope, interner: &mut StringInterner, data_size: u64) {
    core::register(scope, interner);
    mem::register(scope, interner, data_size);
    math::register(scope, interner);
    string::register(scope, interner);
    io::register(scope, interner);
    bit::register(scope, interner);
    limits::register(scope, interner);
}
