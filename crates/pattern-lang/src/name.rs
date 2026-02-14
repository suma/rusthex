// Interned name type for identifiers
//
// Uses u32 index into a StringInterner for O(1) clone (Copy), O(1) equality,
// and O(1) hashing. String resolution requires the interner.

use rustc_hash::FxHashMap;
use std::fmt;
use std::sync::Arc;

/// An interned identifier name. Copy semantics — equality is O(1) integer comparison.
/// To get the underlying string, use `StringInterner::resolve(name)`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(u32);

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name({})", self.0)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name({})", self.0)
    }
}

/// String interner that maps strings to unique Name indices.
/// Deduplicates strings so identical strings always produce the same Name.
pub struct StringInterner {
    strings: Vec<Arc<str>>,
    lookup: FxHashMap<Arc<str>, u32>,
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            lookup: FxHashMap::default(),
        }
    }

    /// Intern a string and return its unique Name.
    /// If the string was already interned, returns the existing Name.
    #[inline]
    pub fn intern(&mut self, s: &str) -> Name {
        if let Some(&idx) = self.lookup.get(s) {
            return Name(idx);
        }
        let idx = self.strings.len() as u32;
        let arc: Arc<str> = Arc::from(s);
        self.strings.push(arc.clone());
        self.lookup.insert(arc, idx);
        Name(idx)
    }

    /// Look up a string without interning it. Returns None if not found.
    pub fn lookup(&self, s: &str) -> Option<Name> {
        self.lookup.get(s).map(|&idx| Name(idx))
    }

    /// Resolve a Name back to its string.
    /// Panics if the Name was not created by this interner.
    #[inline]
    pub fn resolve(&self, name: Name) -> &str {
        &self.strings[name.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_name_copy() {
        let mut interner = StringInterner::new();
        let name = interner.intern("hello");
        let copied = name; // Copy, not clone
        assert_eq!(name, copied);
    }

    #[test]
    fn test_name_dedup() {
        let mut interner = StringInterner::new();
        let a = interner.intern("foo");
        let b = interner.intern("foo");
        assert_eq!(a, b);
    }

    #[test]
    fn test_name_different() {
        let mut interner = StringInterner::new();
        let a = interner.intern("foo");
        let b = interner.intern("bar");
        assert_ne!(a, b);
    }

    #[test]
    fn test_name_resolve() {
        let mut interner = StringInterner::new();
        let name = interner.intern("hello");
        assert_eq!(interner.resolve(name), "hello");
    }

    #[test]
    fn test_name_hashmap() {
        let mut interner = StringInterner::new();
        let key = interner.intern("key");
        let mut map = HashMap::new();
        map.insert(key, 42);
        assert_eq!(map.get(&key), Some(&42));
    }
}
