// Scope and variable environment for the evaluator

use rustc_hash::FxHashMap;
use std::rc::Rc;

use crate::name::{Name, StringInterner};
use crate::parser::ast::{Attribute, Expr, Stmt, TypeExpr};

use crate::error::EvalError;

use super::value::Value;

/// A variable in scope
#[derive(Debug, Clone)]
pub struct Variable {
    pub value: Value,
    pub is_const: bool,
}

/// A user-defined type stored in scope
#[derive(Debug, Clone)]
pub enum TypeDef {
    Struct {
        name: Name,
        parent: Option<(Name, Vec<crate::parser::ast::TemplateArg>)>,
        template_params: Vec<Name>,
        body: Vec<Stmt>,
        attrs: Vec<crate::parser::ast::Attribute>,
    },
    Union {
        name: Name,
        template_params: Vec<Name>,
        body: Vec<Stmt>,
        attrs: Vec<crate::parser::ast::Attribute>,
    },
    Enum {
        name: Name,
        underlying: TypeExpr,
        members: Vec<(Name, Option<Value>)>,
    },
    Bitfield {
        name: Name,
        template_params: Vec<Name>,
        body: Vec<Stmt>,
    },
    Alias {
        name: Name,
        template_params: Vec<Name>,
        ty: Option<TypeExpr>,
        attrs: Vec<Attribute>,
    },
}

/// Function stored in scope
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: Name,
    pub params: Vec<(Name, TypeExpr, Option<Expr>, bool)>, // (name, type, default, is_variadic)
    pub return_ty: Option<TypeExpr>,
    pub body: Vec<Stmt>,
}

/// Built-in function signature
pub type BuiltinFn = Box<dyn Fn(&[Value]) -> Result<Value, String>>;

/// A single scope frame
#[derive(Default)]
struct ScopeFrame {
    variables: FxHashMap<Name, Variable>,
    types: FxHashMap<Name, Rc<TypeDef>>,
    functions: FxHashMap<Name, Rc<FunctionDef>>,
}

/// Scope chain for variable/type/function resolution
pub struct Scope {
    frames: Vec<ScopeFrame>,
    builtins: FxHashMap<Name, BuiltinFn>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            frames: vec![ScopeFrame::default()],
            builtins: FxHashMap::default(),
        }
    }

    /// Push a new scope frame
    pub fn push(&mut self) {
        self.frames.push(ScopeFrame::default());
    }

    /// Pop the current scope frame
    pub fn pop(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }

    /// Define a variable in the current scope
    #[inline]
    pub fn define_var(&mut self, name: Name, value: Value, is_const: bool) {
        let frame = self.frames.last_mut().unwrap();
        frame.variables.insert(name, Variable { value, is_const });
    }

    /// Set a variable's value, searching up the scope chain
    pub fn set_var(
        &mut self,
        name: Name,
        value: Value,
        interner: &StringInterner,
    ) -> Result<(), EvalError> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(var) = frame.variables.get_mut(&name) {
                if var.is_const {
                    return Err(EvalError::new(format!(
                        "cannot assign to const variable '{}'",
                        interner.resolve(name)
                    )));
                }
                var.value = value;
                return Ok(());
            }
        }
        Err(EvalError::new(format!(
            "undefined variable '{}'",
            interner.resolve(name)
        )))
    }

    /// Set an element of an array variable by index
    pub fn set_var_element(
        &mut self,
        name: Name,
        index: usize,
        value: Value,
        interner: &StringInterner,
    ) -> Result<(), EvalError> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(var) = frame.variables.get_mut(&name) {
                if var.is_const {
                    return Err(EvalError::new(format!(
                        "cannot assign to const variable '{}'",
                        interner.resolve(name)
                    )));
                }
                match &mut var.value {
                    Value::Array(items) => {
                        if index >= items.len() {
                            items.resize(index + 1, Value::Null);
                        }
                        items[index] = value;
                        return Ok(());
                    }
                    _ => {
                        // Auto-convert non-array to array for index assignment
                        let mut items = Vec::new();
                        items.resize(index + 1, Value::Null);
                        items[index] = value;
                        var.value = Value::Array(items);
                        return Ok(());
                    }
                }
            }
        }
        Err(EvalError::new(format!(
            "undefined variable '{}'",
            interner.resolve(name)
        )))
    }

    /// Look up a variable by name
    #[inline]
    pub fn get_var(&self, name: Name) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(var) = frame.variables.get(&name) {
                return Some(&var.value);
            }
        }
        None
    }

    /// Return all variables whose name starts with the given prefix followed by '.'
    /// Returns pairs of (suffix_after_dot, value)
    pub fn get_vars_with_prefix(
        &self,
        prefix: &str,
        interner: &StringInterner,
    ) -> Vec<(String, Value)> {
        let dot_prefix = format!("{}.", prefix);
        let mut result = Vec::new();
        let mut seen = rustc_hash::FxHashSet::default();
        for frame in self.frames.iter().rev() {
            for (name, var) in &frame.variables {
                let name_str = interner.resolve(*name);
                if let Some(suffix) = name_str.strip_prefix(&dot_prefix) {
                    if seen.insert(suffix.to_string()) {
                        result.push((suffix.to_string(), var.value.clone()));
                    }
                }
            }
        }
        result
    }

    /// Return all variables in the current (topmost) scope frame
    pub fn current_frame_vars(&self, interner: &StringInterner) -> Vec<(String, Value)> {
        let frame = self.frames.last().unwrap();
        frame
            .variables
            .iter()
            .map(|(k, v)| (interner.resolve(*k).to_string(), v.value.clone()))
            .collect()
    }

    /// Return variables from the enclosing struct context.
    /// Walks up from the current frame to find the nearest frame that has `this`
    /// defined (indicating a struct/union body scope), then collects variables
    /// from that frame plus any intervening frames (match/if/try-catch).
    /// Falls back to current_frame_vars if no struct context is found.
    pub fn enclosing_struct_vars(
        &self,
        this_key: Name,
        interner: &StringInterner,
    ) -> Vec<(String, Value)> {
        // Find the nearest frame with `this` defined (the enclosing struct scope)
        let mut struct_frame_idx = None;
        for (idx, frame) in self.frames.iter().enumerate().rev() {
            if frame.variables.contains_key(&this_key) {
                struct_frame_idx = Some(idx);
                break;
            }
        }

        match struct_frame_idx {
            Some(idx) => {
                // Collect variables from the struct frame and any frames above it
                // (match/if/try-catch frames may have local vars too),
                // with top-frame-wins priority
                let mut seen = rustc_hash::FxHashSet::default();
                let mut result = Vec::new();
                for frame in self.frames[idx..].iter().rev() {
                    for (name, var) in &frame.variables {
                        let name_str = interner.resolve(*name).to_string();
                        if seen.insert(name_str.clone()) {
                            result.push((name_str, var.value.clone()));
                        }
                    }
                }
                result
            }
            None => self.current_frame_vars(interner),
        }
    }

    /// Define a type in the current scope
    pub fn define_type(&mut self, name: Name, typedef: TypeDef) {
        let frame = self.frames.last_mut().unwrap();
        frame.types.insert(name, Rc::new(typedef));
    }

    /// Look up a type by name (returns Rc for cheap cloning)
    #[inline]
    pub fn get_type(&self, name: Name) -> Option<Rc<TypeDef>> {
        for frame in self.frames.iter().rev() {
            if let Some(td) = frame.types.get(&name) {
                return Some(Rc::clone(td));
            }
        }
        None
    }

    /// Define a function in the current scope
    pub fn define_fn(&mut self, name: Name, func: FunctionDef) {
        let frame = self.frames.last_mut().unwrap();
        frame.functions.insert(name, Rc::new(func));
    }

    /// Look up a function by name (returns Rc for cheap cloning)
    #[inline]
    pub fn get_fn(&self, name: Name) -> Option<Rc<FunctionDef>> {
        for frame in self.frames.iter().rev() {
            if let Some(f) = frame.functions.get(&name) {
                return Some(Rc::clone(f));
            }
        }
        None
    }

    /// Register a built-in function
    pub fn register_builtin(
        &mut self,
        name: Name,
        func: impl Fn(&[Value]) -> Result<Value, String> + 'static,
    ) {
        self.builtins.insert(name, Box::new(func));
    }

    /// Look up a built-in function and call it
    pub fn call_builtin(&self, name: Name, args: &[Value]) -> Option<Result<Value, EvalError>> {
        self.builtins
            .get(&name)
            .map(|f| f(args).map_err(EvalError::new))
    }

    /// Check if a built-in function exists
    pub fn has_builtin(&self, name: Name) -> bool {
        self.builtins.contains_key(&name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_var_define_get() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let x = interner.intern("x");
        scope.define_var(x, Value::Unsigned(42), false);
        assert_eq!(scope.get_var(x), Some(&Value::Unsigned(42)));
    }

    #[test]
    fn test_scope_var_shadowing() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let x = interner.intern("x");
        scope.define_var(x, Value::Unsigned(1), false);
        scope.push();
        scope.define_var(x, Value::Unsigned(2), false);
        assert_eq!(scope.get_var(x), Some(&Value::Unsigned(2)));
        scope.pop();
        assert_eq!(scope.get_var(x), Some(&Value::Unsigned(1)));
    }

    #[test]
    fn test_scope_set_var() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let x = interner.intern("x");
        scope.define_var(x, Value::Unsigned(1), false);
        scope.set_var(x, Value::Unsigned(2), &interner).unwrap();
        assert_eq!(scope.get_var(x), Some(&Value::Unsigned(2)));
    }

    #[test]
    fn test_scope_const_var() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let x = interner.intern("x");
        scope.define_var(x, Value::Unsigned(1), true);
        assert!(scope.set_var(x, Value::Unsigned(2), &interner).is_err());
    }

    #[test]
    fn test_scope_undefined_var() {
        let mut interner = StringInterner::new();
        let scope = Scope::new();
        let nope = interner.intern("nope");
        assert!(scope.get_var(nope).is_none());
    }

    #[test]
    fn test_set_var_element_basic() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let arr = interner.intern("arr");
        scope.define_var(
            arr,
            Value::Array(vec![
                Value::Unsigned(10),
                Value::Unsigned(20),
                Value::Unsigned(30),
            ]),
            false,
        );
        scope
            .set_var_element(arr, 1, Value::Unsigned(99), &interner)
            .unwrap();
        match scope.get_var(arr) {
            Some(Value::Array(items)) => {
                assert_eq!(items[0], Value::Unsigned(10));
                assert_eq!(items[1], Value::Unsigned(99));
                assert_eq!(items[2], Value::Unsigned(30));
            }
            other => panic!("expected Array, got {:?}", other),
        }
    }

    #[test]
    fn test_set_var_element_extends_array() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let arr = interner.intern("arr");
        scope.define_var(arr, Value::Array(vec![Value::Unsigned(1)]), false);
        // Index beyond current length should extend with Null
        scope
            .set_var_element(arr, 3, Value::Unsigned(42), &interner)
            .unwrap();
        match scope.get_var(arr) {
            Some(Value::Array(items)) => {
                assert_eq!(items.len(), 4);
                assert_eq!(items[0], Value::Unsigned(1));
                assert_eq!(items[1], Value::Null);
                assert_eq!(items[2], Value::Null);
                assert_eq!(items[3], Value::Unsigned(42));
            }
            other => panic!("expected Array, got {:?}", other),
        }
    }

    #[test]
    fn test_set_var_element_const_error() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let arr = interner.intern("arr");
        scope.define_var(arr, Value::Array(vec![Value::Unsigned(1)]), true);
        assert!(scope
            .set_var_element(arr, 0, Value::Unsigned(2), &interner)
            .is_err());
    }

    #[test]
    fn test_set_var_element_non_array_auto_converts() {
        // Non-array variables are auto-converted to arrays on index assignment
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let x = interner.intern("x");
        scope.define_var(x, Value::Unsigned(42), false);
        scope
            .set_var_element(x, 2, Value::Unsigned(99), &interner)
            .unwrap();
        match scope.get_var(x) {
            Some(Value::Array(items)) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[2], Value::Unsigned(99));
            }
            other => panic!("expected Array, got {:?}", other),
        }
    }

    #[test]
    fn test_set_var_element_undefined_error() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let x = interner.intern("x");
        assert!(scope
            .set_var_element(x, 0, Value::Unsigned(1), &interner)
            .is_err());
    }

    #[test]
    fn test_enclosing_struct_vars_finds_struct_frame() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let this_key = interner.intern("this");
        let field_a = interner.intern("field_a");

        // Simulate struct scope with `this` defined
        scope.push();
        scope.define_var(this_key, Value::Unsigned(0), false);
        scope.define_var(field_a, Value::Unsigned(42), false);

        // Push a match/if scope on top (no `this`)
        scope.push();
        let local = interner.intern("local");
        scope.define_var(local, Value::Unsigned(99), false);

        let vars = scope.enclosing_struct_vars(this_key, &interner);
        let names: Vec<&str> = vars.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"this"));
        assert!(names.contains(&"field_a"));
        assert!(names.contains(&"local"));
    }

    #[test]
    fn test_enclosing_struct_vars_fallback_no_struct() {
        let mut interner = StringInterner::new();
        let mut scope = Scope::new();
        let this_key = interner.intern("this");
        let x = interner.intern("x");

        // No `this` anywhere
        scope.define_var(x, Value::Unsigned(1), false);
        let vars = scope.enclosing_struct_vars(this_key, &interner);
        let names: Vec<&str> = vars.iter().map(|(n, _)| n.as_str()).collect();
        assert!(names.contains(&"x"));
    }
}
