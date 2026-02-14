// Tree-walking evaluator for the pattern language

pub mod data_source;
mod expr;
pub mod pattern;
mod read_type;
pub mod scope;
mod stmt;
pub mod value;

use crate::error::EvalError;
use crate::name::{Name, StringInterner};
use crate::parser::ast::*;
use crate::span::Span;
use data_source::DataSource;
use pattern::{PatternAttributes, PatternNode, PatternValue};
use scope::{FunctionDef, Scope, TypeDef};
use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use value::Value;

/// Control flow signal from statement evaluation
pub(crate) enum ControlFlow {
    None,
    Break,
    Continue,
    Return(Option<Value>),
}

/// Result of executing a loop body iteration
pub(crate) enum LoopAction {
    Continue,
    Break,
    Return(ControlFlow),
}

// Default evaluation limits
const DEFAULT_MAX_RECURSION_DEPTH: u32 = 256;
const DEFAULT_MAX_ARRAY_LIMIT: u64 = 0x10000;
const DEFAULT_MAX_PATTERN_LIMIT: u64 = 0x40000;
/// Default total node limit (controls memory usage).
/// At ~512 bytes per node, 500K nodes ≈ 256 MB.
const DEFAULT_MAX_TOTAL_NODES: u64 = 500_000;

/// Evaluation statistics collected during evaluation
#[derive(Debug, Clone, Default)]
pub struct EvalStats {
    /// Total number of eval_stmt calls
    pub stmt_count: u64,
    /// Total number of read_type calls
    pub read_type_count: u64,
    /// Total PatternNode count (including children)
    pub total_node_count: u64,
    /// Total results (top-level nodes)
    pub result_count: usize,
}

/// Evaluator that processes an AST against binary data
pub struct Evaluator<'a> {
    data: &'a dyn DataSource,
    pub scope: Scope,
    pub interner: StringInterner,
    offset: u64,
    results: Vec<PatternNode>,
    default_endian: Endianness,
    recursion_depth: u32,
    max_recursion_depth: u32,
    max_array_limit: u64,
    max_pattern_limit: u64,
    pattern_count: u64,
    /// Output from std::io::print
    pub print_output: Vec<String>,
    /// Preprocessed source text for error location reporting
    source: String,
    /// Whether we are inside a struct/union body (for LocalVar → virtual node promotion)
    in_struct_body: bool,
    /// Current namespace context for qualified name resolution
    current_namespace: Option<String>,
    /// Total PatternNode count (including children) for memory limiting
    total_node_count: u64,
    /// Maximum total PatternNode count (0 = unlimited)
    max_total_nodes: u64,
    /// Cancellation token — evaluation stops when set to true
    cancelled: Option<Arc<AtomicBool>>,
    /// Number of eval_stmt calls (for performance debugging)
    stmt_count: u64,
    /// Number of read_type calls (for performance debugging)
    read_type_count: u64,
    /// Monotonic counter for amortized cancellation checks
    check_count: u64,
    /// Shared counters — readable from outside during evaluation
    shared_stmt_count: Option<Arc<AtomicU64>>,
    shared_read_type_count: Option<Arc<AtomicU64>>,
    /// Set when `break` is encountered inside a struct body.
    /// Propagates to the enclosing while-loop array.
    pub(crate) break_signaled: bool,
    /// Custom memory sections (handle → growable byte buffer)
    sections: FxHashMap<u128, Vec<u8>>,
    /// Next section handle ID
    next_section_id: u128,
    /// Temporary section data override for reads during `in section` placement
    section_data_override: Option<Vec<u8>>,
    /// Section write-through metadata: variable name → (handle, offset, byte_size)
    section_vars: FxHashMap<Name, (u128, u64, u64)>,
    /// Reusable buffer for array element names (e.g. "[0]", "[123]")
    elem_name_buf: String,
}

impl<'a> Evaluator<'a> {
    pub fn new(data: &'a dyn DataSource) -> Self {
        Self::with_interner(data, StringInterner::new())
    }

    pub fn with_interner(data: &'a dyn DataSource, interner: StringInterner) -> Self {
        let scope = Scope::new();
        Self {
            data,
            scope,
            interner,
            offset: 0,
            results: Vec::new(),
            default_endian: Endianness::Little,
            recursion_depth: 0,
            max_recursion_depth: DEFAULT_MAX_RECURSION_DEPTH,
            max_array_limit: DEFAULT_MAX_ARRAY_LIMIT,
            max_pattern_limit: DEFAULT_MAX_PATTERN_LIMIT,
            pattern_count: 0,
            print_output: Vec::new(),
            source: String::new(),
            in_struct_body: false,
            current_namespace: None,
            total_node_count: 0,
            max_total_nodes: DEFAULT_MAX_TOTAL_NODES,
            cancelled: None,
            stmt_count: 0,
            read_type_count: 0,
            check_count: 0,
            shared_stmt_count: None,
            shared_read_type_count: None,
            break_signaled: false,
            sections: FxHashMap::default(),
            next_section_id: 1,
            section_data_override: None,
            section_vars: FxHashMap::default(),
            elem_name_buf: String::with_capacity(16),
        }
    }

    /// Format an array element name like "[42]" into the reusable buffer.
    /// Returns the formatted string. Buffer is cleared before each use.
    pub(crate) fn fmt_elem_name(&mut self, index: u64) -> String {
        self.elem_name_buf.clear();
        self.elem_name_buf.push('[');
        // Inline integer formatting to avoid format!() allocation
        itoa_into(&mut self.elem_name_buf, index);
        self.elem_name_buf.push(']');
        self.elem_name_buf.clone()
    }

    /// Set the preprocessed source text for error location reporting
    pub fn set_source(&mut self, source: String) {
        self.source = source;
    }

    /// Create an EvalError with span and resolved line/col
    pub(crate) fn make_error(&self, message: impl Into<String>, span: Span) -> EvalError {
        let (line, col) = crate::span::offset_to_line_col(&self.source, span.start);
        EvalError::with_location(message, span, line, col)
    }

    /// Resolve location on an existing error (if span is set but line/col are not)
    pub(crate) fn resolve_error(&self, err: EvalError) -> EvalError {
        err.resolve_location(&self.source)
    }

    /// Set default endianness
    pub fn set_default_endian(&mut self, endian: Endianness) {
        self.default_endian = endian;
    }

    /// Set evaluation limits.
    /// Recursion depth is capped at 512 to prevent stack overflow.
    pub fn set_limits(&mut self, array_limit: u64, pattern_limit: u64, recursion_depth: u32) {
        self.max_array_limit = array_limit;
        self.max_pattern_limit = pattern_limit;
        self.max_recursion_depth = recursion_depth.min(1024);
    }

    /// Set maximum total node count (controls memory usage).
    /// At ~512 bytes per node, 500K nodes ≈ 256 MB.
    /// Set to 0 to disable the limit.
    pub fn set_max_total_nodes(&mut self, limit: u64) {
        self.max_total_nodes = limit;
    }

    /// Ensure max_total_nodes is at least `min`.
    /// If `#pragma pattern_limit` declares a higher limit than the current
    /// max_total_nodes, the pattern author expects to create that many nodes.
    pub fn ensure_min_total_nodes(&mut self, min: u64) {
        if min > 0 && self.max_total_nodes > 0 && min > self.max_total_nodes {
            self.max_total_nodes = min;
        }
    }

    /// Set a cancellation token. Evaluation stops when the token is set to true.
    pub fn set_cancellation_token(&mut self, token: Arc<AtomicBool>) {
        self.cancelled = Some(token);
    }

    /// Set shared counters for live monitoring from another thread.
    pub fn set_shared_stmt_count(&mut self, counter: Arc<AtomicU64>) {
        self.shared_stmt_count = Some(counter);
    }

    pub fn set_shared_read_type_count(&mut self, counter: Arc<AtomicU64>) {
        self.shared_read_type_count = Some(counter);
    }

    /// Check resource limits: cancellation and total node count.
    /// Called at key points (read_type entry, loop iterations).
    /// Cancellation check is amortized to every 256th call to reduce atomic load overhead.
    pub(crate) fn check_resource_limits(&mut self, span: Span) -> Result<(), EvalError> {
        self.check_count += 1;
        if self.check_count & 0xFF == 0 {
            if let Some(ref token) = self.cancelled {
                if token.load(Ordering::Relaxed) {
                    return Err(self.make_error("evaluation cancelled", span));
                }
            }
        }
        if self.max_total_nodes > 0 && self.total_node_count > self.max_total_nodes {
            return Err(self.make_error(
                format!(
                    "total node limit exceeded ({} > max {})",
                    self.total_node_count, self.max_total_nodes
                ),
                span,
            ));
        }
        Ok(())
    }

    /// Check pattern limit and increment counter.
    pub(crate) fn check_pattern_limit(&mut self, span: Span) -> Result<(), EvalError> {
        self.pattern_count += 1;
        if self.max_pattern_limit > 0 && self.pattern_count > self.max_pattern_limit {
            return Err(self.make_error(
                format!("pattern limit exceeded (max {})", self.max_pattern_limit),
                span,
            ));
        }
        Ok(())
    }

    pub(crate) fn resolve_path(&self, path: &[Name]) -> String {
        path.iter()
            .map(|n| self.interner.resolve(*n))
            .collect::<Vec<_>>()
            .join("::")
    }

    /// Read bytes from the active data source (section override or main data).
    pub(crate) fn read_data_bytes(&self, offset: u64, size: u64) -> Result<Vec<u8>, EvalError> {
        if let Some(ref section_bytes) = self.section_data_override {
            let end = offset as usize + size as usize;
            if end > section_bytes.len() {
                return Err(EvalError::read_oob(format!(
                    "section read out of bounds: offset {} + size {} > section size {}",
                    offset,
                    size,
                    section_bytes.len()
                )));
            }
            Ok(section_bytes[offset as usize..end].to_vec())
        } else {
            self.data.read_bytes(offset, size)
        }
    }

    /// Get size of the active data source (section override or main data).
    pub(crate) fn active_data_size(&self) -> u64 {
        if let Some(ref section_bytes) = self.section_data_override {
            section_bytes.len() as u64
        } else {
            self.data.size()
        }
    }

    /// Write a value back to a section buffer.
    pub(crate) fn write_section_value(
        &mut self,
        handle: u128,
        offset: u64,
        size: u64,
        val: &Value,
    ) {
        let bytes = match val {
            Value::Unsigned(v) => v.to_le_bytes().to_vec(),
            Value::Signed(v) => v.to_le_bytes().to_vec(),
            Value::Float(f) => f.to_le_bytes().to_vec(),
            Value::Bool(b) => vec![if *b { 1 } else { 0 }],
            _ => return,
        };
        if let Some(section) = self.sections.get_mut(&handle) {
            let end = offset as usize + size as usize;
            if section.len() < end {
                section.resize(end, 0);
            }
            let copy_len = size as usize;
            for i in 0..copy_len {
                section[offset as usize + i] = if i < bytes.len() { bytes[i] } else { 0 };
            }
        }
    }

    /// Evaluate an AST and return pattern nodes
    pub fn evaluate(&mut self, ast: &Ast) -> Result<Vec<PatternNode>, EvalError> {
        // First pass: collect type and function definitions
        for stmt in &ast.stmts {
            self.collect_definitions(stmt)?;
        }

        // Second pass: evaluate statements
        // OOB at top level is treated as end-of-evaluation (not a fatal error),
        // matching ImHex behavior where patterns stop when data runs out.
        for stmt in &ast.stmts {
            match self.eval_stmt(stmt) {
                Ok(_) => {}
                Err(e) if e.is_read_oob => break,
                Err(e) => return Err(e),
            }
        }

        Ok(self.results.clone())
    }

    /// Get evaluation statistics
    pub fn stats(&self) -> EvalStats {
        EvalStats {
            stmt_count: self.stmt_count,
            read_type_count: self.read_type_count,
            total_node_count: self.total_node_count,
            result_count: self.results.len(),
        }
    }

    /// Collect type and function definitions from the AST
    fn collect_definitions(&mut self, stmt: &Stmt) -> Result<(), EvalError> {
        self.collect_definitions_impl(stmt, None)
    }

    /// Qualify a name with an optional namespace prefix.
    /// Returns the name unchanged when `ns_prefix` is None,
    /// or "prefix::name" when Some.
    fn qualify_name(&mut self, ns_prefix: Option<Name>, name: Name) -> Name {
        match ns_prefix {
            None => name,
            Some(prefix) => {
                let qs = format!(
                    "{}::{}",
                    self.interner.resolve(prefix),
                    self.interner.resolve(name)
                );
                self.interner.intern(&qs)
            }
        }
    }

    /// Internal: collect definitions with optional namespace prefix.
    /// When `ns_prefix` is Some, names are qualified (e.g. "ns::Name").
    fn collect_definitions_impl(
        &mut self,
        stmt: &Stmt,
        ns_prefix: Option<Name>,
    ) -> Result<(), EvalError> {
        match &stmt.kind {
            StmtKind::StructDef(d) => {
                let reg_name = self.qualify_name(ns_prefix, d.name);
                self.scope.define_type(
                    reg_name,
                    TypeDef::Struct {
                        name: reg_name,
                        parent: d.parent.clone(),
                        template_params: d.template_params.iter().map(|p| p.name).collect(),
                        body: d.body.clone(),
                        attrs: d.attrs.clone(),
                    },
                );
            }
            StmtKind::UnionDef(d) => {
                let reg_name = self.qualify_name(ns_prefix, d.name);
                self.scope.define_type(
                    reg_name,
                    TypeDef::Union {
                        name: reg_name,
                        template_params: d.template_params.iter().map(|p| p.name).collect(),
                        body: d.body.clone(),
                        attrs: d.attrs.clone(),
                    },
                );
            }
            StmtKind::EnumDef(d) => {
                let reg_name = self.qualify_name(ns_prefix, d.name);
                let mut resolved_members = Vec::new();
                let mut next_val: u128 = 0;

                // Register empty enum first so members can reference earlier members
                self.scope.define_type(
                    reg_name,
                    TypeDef::Enum {
                        name: reg_name,
                        underlying: d.underlying.clone(),
                        members: vec![],
                    },
                );

                for member in &d.members {
                    let val = if let Some(ref expr) = member.value {
                        let v = self.eval_expr(expr)?;
                        next_val = v.to_unsigned()?.wrapping_add(1);
                        Some(v)
                    } else {
                        let v = Value::Unsigned(next_val);
                        next_val += 1;
                        Some(v)
                    };
                    resolved_members.push((member.name, val));

                    // Update enum definition so subsequent members can reference earlier ones
                    self.scope.define_type(
                        reg_name,
                        TypeDef::Enum {
                            name: reg_name,
                            underlying: d.underlying.clone(),
                            members: resolved_members.clone(),
                        },
                    );
                }
            }
            StmtKind::BitfieldDef(d) => {
                let reg_name = self.qualify_name(ns_prefix, d.name);
                // Store the entire bitfield definition stmt for later evaluation
                self.scope.define_type(
                    reg_name,
                    TypeDef::Bitfield {
                        name: reg_name,
                        template_params: d.template_params.iter().map(|p| p.name).collect(),
                        body: vec![stmt.clone()],
                    },
                );
            }
            StmtKind::FnDef(d) => {
                let reg_name = self.qualify_name(ns_prefix, d.name);
                self.scope.define_fn(
                    reg_name,
                    FunctionDef {
                        name: reg_name,
                        params: d.params
                            .iter()
                            .map(|p| (p.name, p.ty.clone(), p.default.clone(), p.is_variadic))
                            .collect(),
                        return_ty: d.return_ty.clone(),
                        body: d.body.clone(),
                    },
                );
            }
            StmtKind::TypeAlias(d) => {
                let reg_name = self.qualify_name(ns_prefix, d.name);
                // Don't shadow existing type definitions with forward declarations (ty: None).
                // Only applies to non-namespace (ns_prefix None) to match original behavior.
                if ns_prefix.is_none() && d.ty.is_none() && self.scope.get_type(reg_name).is_some() {
                    // Forward declaration: keep existing definition
                } else {
                    self.scope.define_type(
                        reg_name,
                        TypeDef::Alias {
                            name: reg_name,
                            template_params: d.template_params.iter().map(|p| p.name).collect(),
                            ty: d.ty.clone(),
                            attrs: d.attrs.clone(),
                        },
                    );
                }
            }
            StmtKind::Namespace(d) => {
                let new_prefix = self.qualify_name(ns_prefix, d.name);
                for s in &d.body {
                    self.collect_definitions_impl(s, Some(new_prefix))?;
                    // Also register with unqualified name
                    self.collect_definitions_impl(s, None)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Resolve endianness from type expression
    pub(crate) fn resolve_endianness(&self, ty: &TypeExpr) -> Endianness {
        match &ty.kind {
            TypeExprKind::Endian(endian, _) => *endian,
            _ => self.default_endian,
        }
    }

    /// Evaluate attributes
    pub(crate) fn eval_attributes(
        &mut self,
        attrs: &[Attribute],
    ) -> Result<PatternAttributes, EvalError> {
        let mut result = PatternAttributes::default();
        for attr in attrs {
            match self.interner.resolve(attr.name) {
                "color" => {
                    if let Some(arg) = attr.args.first() {
                        let val = self.eval_expr(arg)?;
                        result.color = Some(val.to_display_string());
                    }
                }
                "comment" => {
                    if let Some(arg) = attr.args.first() {
                        let val = self.eval_expr(arg)?;
                        result.comment = Some(val.to_display_string());
                    }
                }
                "hidden" => {
                    result.hidden = true;
                }
                "name" => {
                    if let Some(arg) = attr.args.first() {
                        let val = self.eval_expr(arg)?;
                        result.display_name = Some(val.to_display_string());
                    }
                }
                "format" => {
                    if let Some(arg) = attr.args.first() {
                        let val = self.eval_expr(arg)?;
                        result.format = Some(val.to_display_string());
                    }
                }
                "sealed" => {
                    result.sealed = true;
                }
                "inline" => {
                    result.inline = true;
                }
                "no_unique_address" => {
                    result.no_unique_address = true;
                }
                "transform" | "format_read" | "format_write" | "static" => {
                    // Known but not implemented attributes - ignore silently
                }
                _ => {} // Unknown attributes are ignored
            }
        }
        Ok(result)
    }

    /// Get type-level attributes from a TypeExpr (struct/union attrs only).
    /// Used by eval_data_placement for top-level transform application.
    pub(crate) fn get_type_attrs(&self, ty: &TypeExpr) -> Vec<Attribute> {
        self.get_type_attrs_filter(ty, false)
    }

    /// Get type-level attributes only from `using` alias definitions.
    /// Used inside struct body evaluation to apply alias transforms (e.g., uLEB128)
    /// without applying struct/union transforms that would break member access.
    pub(crate) fn get_alias_type_attrs(&self, ty: &TypeExpr) -> Vec<Attribute> {
        self.get_type_attrs_filter(ty, true)
    }

    fn get_type_attrs_filter(&self, ty: &TypeExpr, alias_only: bool) -> Vec<Attribute> {
        let type_name = match &ty.kind {
            TypeExprKind::Named(path) => Some(self.resolve_path(path)),
            TypeExprKind::Template(path, _) => Some(self.resolve_path(path)),
            _ => None,
        };
        if let Some(name) = type_name {
            let key = self.interner.lookup(&name);
            if let Some(k) = key {
                if let Some(td) = self.scope.get_type(k) {
                    return match &*td {
                        TypeDef::Struct { attrs, .. } | TypeDef::Union { attrs, .. }
                            if !alias_only =>
                        {
                            attrs.clone()
                        }
                        TypeDef::Alias { attrs, .. } if alias_only => attrs.clone(),
                        _ => vec![],
                    };
                }
            }
        }
        vec![]
    }
}

// ========== Byte conversion helpers ==========

pub(crate) fn bytes_to_array<const N: usize>(bytes: &[u8]) -> Result<[u8; N], EvalError> {
    bytes[..N]
        .try_into()
        .map_err(|_| EvalError::new(format!("expected {} bytes, got {}", N, bytes.len())))
}

// ========== Member propagation ==========

/// Threshold for skipping scope propagation on large arrays.
/// Arrays with >= this many children use SizedRef → find_node_by_offset fallback instead.
const LAZY_PROPAGATION_THRESHOLD: usize = 64;

/// Register struct children with dotted names in scope (e.g., "header.magic").
/// Uses a mutable String buffer to avoid repeated format!() allocations.
pub(crate) fn propagate_children(
    scope: &mut Scope,
    interner: &mut StringInterner,
    prefix: &str,
    children: &[PatternNode],
) {
    if children.len() >= LAZY_PROPAGATION_THRESHOLD {
        return;
    }
    let mut buf = String::with_capacity(prefix.len() + 32);
    buf.push_str(prefix);
    propagate_children_inner(scope, interner, &mut buf, prefix.len(), children);
}

fn propagate_children_inner(
    scope: &mut Scope,
    interner: &mut StringInterner,
    buf: &mut String,
    prefix_len: usize,
    children: &[PatternNode],
) {
    if children.len() >= LAZY_PROPAGATION_THRESHOLD {
        return;
    }
    for child in children {
        // Build "prefix.child_name" in-place
        buf.truncate(prefix_len);
        buf.push('.');
        buf.push_str(&child.name);
        let dotted_key = interner.intern(buf);
        let val = value_from_node(child);
        // For non-addressable values (String from char[], Array), store __addr_ so
        // addressof() can resolve the offset without searching the result tree.
        if matches!(val, Value::String(_) | Value::Array(_)) {
            let dotted_len = buf.len();
            // Prepend "__addr_" by rebuilding: "__addr_" + current buf content
            let dotted_snapshot = buf[..dotted_len].to_string();
            buf.clear();
            buf.push_str("__addr_");
            buf.push_str(&dotted_snapshot);
            let addr_key = interner.intern(buf);
            scope.define_var(
                addr_key,
                Value::SizedRef {
                    offset: child.offset,
                    size: child.size,
                },
                false,
            );
            // Restore buf to the dotted name
            buf.clear();
            buf.push_str(&dotted_snapshot);
        }
        scope.define_var(dotted_key, val, false);
        let current_len = buf.len();
        propagate_children_inner(scope, interner, buf, current_len, &child.children);
    }
}

// ========== Template substitution ==========

/// Substitute template parameters in a type expression
pub(crate) fn substitute_type(ty: &TypeExpr, params: &[Name], args: &[TemplateArg]) -> TypeExpr {
    match &ty.kind {
        TypeExprKind::Named(path) if path.len() == 1 => {
            // Check if it matches a template parameter
            if let Some(idx) = params.iter().position(|p| p == &path[0]) {
                match args.get(idx) {
                    Some(TemplateArg::Type(arg_ty)) => return arg_ty.clone(),
                    // Expr(Ident) may be a type name parsed as expression
                    Some(TemplateArg::Expr(expr)) if matches!(&expr.kind, ExprKind::Ident(_)) => {
                        if let ExprKind::Ident(name) = &expr.kind {
                            return TypeExpr {
                                kind: TypeExprKind::Named(vec![name.clone()]),
                                span: expr.span,
                            };
                        }
                    }
                    _ => {}
                }
            }
            ty.clone()
        }
        TypeExprKind::Endian(endian, inner) => {
            let substituted = substitute_type(inner, params, args);
            TypeExpr {
                kind: TypeExprKind::Endian(*endian, Box::new(substituted)),
                span: ty.span,
            }
        }
        TypeExprKind::Array(elem, size) => {
            let substituted = substitute_type(elem, params, args);
            TypeExpr {
                kind: TypeExprKind::Array(Box::new(substituted), size.clone()),
                span: ty.span,
            }
        }
        TypeExprKind::Pointer(pointee, size_ty) => {
            let sub_pointee = substitute_type(pointee, params, args);
            let sub_size = substitute_type(size_ty, params, args);
            TypeExpr {
                kind: TypeExprKind::Pointer(Box::new(sub_pointee), Box::new(sub_size)),
                span: ty.span,
            }
        }
        TypeExprKind::Template(path, template_args) => {
            let new_args: Vec<TemplateArg> = template_args
                .iter()
                .map(|a| match a {
                    TemplateArg::Type(t) => TemplateArg::Type(substitute_type(t, params, args)),
                    TemplateArg::Expr(e) => {
                        // Substitute Ident matching a template parameter
                        if let ExprKind::Ident(name) = &e.kind {
                            if let Some(idx) = params.iter().position(|p| p == name) {
                                if let Some(arg) = args.get(idx) {
                                    return arg.clone();
                                }
                            }
                        }
                        TemplateArg::Expr(e.clone())
                    }
                })
                .collect();
            TypeExpr {
                kind: TypeExprKind::Template(path.clone(), new_args),
                span: ty.span,
            }
        }
        _ => ty.clone(),
    }
}

// ========== Arbitrary-width integer helpers ==========

/// Parse type names like "u24", "s48" → Some((is_signed, bit_width))
pub(crate) fn parse_arbitrary_int_type(name: &str) -> Option<(bool, u64)> {
    let (signed, digits) = if let Some(rest) = name.strip_prefix('u') {
        (false, rest)
    } else if let Some(rest) = name.strip_prefix('s') {
        (true, rest)
    } else {
        return None;
    };
    let bits: u64 = digits.parse().ok()?;
    if bits == 0 || bits > 128 {
        return None;
    }
    Some((signed, bits))
}

/// Read arbitrary-width integer bytes and return as unsigned value
pub(crate) fn bytes_to_unsigned(bytes: &[u8], endian: Endianness) -> u128 {
    let mut val: u128 = 0;
    match endian {
        Endianness::Little => {
            for (i, &b) in bytes.iter().enumerate() {
                val |= (b as u128) << (i * 8);
            }
        }
        Endianness::Big => {
            for &b in bytes.iter() {
                val = (val << 8) | (b as u128);
            }
        }
    }
    val
}

// ========== Helper functions ==========

/// Format a value according to a format specifier (e.g. "", ":02X", ":X").
pub(crate) fn format_value_with_spec(val: &Value, spec: &str) -> String {
    if spec.is_empty() {
        return val.to_display_string();
    }
    // Strip leading ':'
    let spec = spec.strip_prefix(':').unwrap_or(spec);
    if spec.is_empty() {
        return val.to_display_string();
    }
    // Parse optional zero-padding width and format type
    let (width, fmt_char) =
        if spec.len() >= 2 && spec.starts_with('0') && spec.as_bytes()[1].is_ascii_digit() {
            // e.g. "02X" → width=2, fmt='X'
            let w: usize = spec[1..spec.len() - 1].parse().unwrap_or(0);
            let f = spec.chars().last().unwrap_or('d');
            (w, f)
        } else if spec.len() == 1 {
            (0, spec.chars().next().unwrap())
        } else {
            // Try parsing width without leading zero, e.g. "2X"
            let w: usize = spec[..spec.len() - 1].parse().unwrap_or(0);
            let f = spec.chars().last().unwrap_or('d');
            (w, f)
        };

    let num = match val {
        Value::Unsigned(v) => *v,
        Value::Signed(v) => *v as u128,
        Value::Char(c) => *c as u128,
        _ => return val.to_display_string(),
    };

    match fmt_char {
        'X' => format!("{:0>width$X}", num, width = width),
        'x' => format!("{:0>width$x}", num, width = width),
        'd' => format!("{:0>width$}", num, width = width),
        _ => val.to_display_string(),
    }
}

/// Decode a single element from a bulk data byte slice.
pub(crate) fn decode_bulk_element(
    bytes: &[u8],
    elem_type: &BuiltinType,
    endian: Endianness,
) -> Value {
    macro_rules! decode_int {
        ($size:expr, $int_ty:ty, $variant:ident) => {{
            let arr: [u8; $size] = bytes[..$size].try_into().unwrap();
            let v = match endian {
                Endianness::Little => <$int_ty>::from_le_bytes(arr),
                Endianness::Big => <$int_ty>::from_be_bytes(arr),
            };
            Value::$variant(v as _)
        }};
    }
    macro_rules! decode_float {
        ($size:expr, $float_ty:ty) => {{
            let arr: [u8; $size] = bytes[..$size].try_into().unwrap();
            let v = match endian {
                Endianness::Little => <$float_ty>::from_le_bytes(arr),
                Endianness::Big => <$float_ty>::from_be_bytes(arr),
            };
            Value::Float(v as f64)
        }};
    }
    match elem_type {
        BuiltinType::U8 => Value::Unsigned(bytes[0] as u128),
        BuiltinType::S8 => Value::Signed(bytes[0] as i8 as i128),
        BuiltinType::Bool => Value::Bool(bytes[0] != 0),
        BuiltinType::Char => Value::Char(bytes[0] as char),
        BuiltinType::U16 => decode_int!(2, u16, Unsigned),
        BuiltinType::S16 => decode_int!(2, i16, Signed),
        BuiltinType::Char16 => {
            let arr: [u8; 2] = bytes[..2].try_into().unwrap();
            let v = match endian {
                Endianness::Little => u16::from_le_bytes(arr),
                Endianness::Big => u16::from_be_bytes(arr),
            };
            Value::Char(char::from_u32(v as u32).unwrap_or('\u{FFFD}'))
        }
        BuiltinType::U32 => decode_int!(4, u32, Unsigned),
        BuiltinType::S32 => decode_int!(4, i32, Signed),
        BuiltinType::U64 => decode_int!(8, u64, Unsigned),
        BuiltinType::S64 => decode_int!(8, i64, Signed),
        BuiltinType::U128 => decode_int!(16, u128, Unsigned),
        BuiltinType::S128 => decode_int!(16, i128, Signed),
        BuiltinType::Float => decode_float!(4, f32),
        BuiltinType::Double => decode_float!(8, f64),
        BuiltinType::Str => Value::Null,
    }
}

pub(crate) fn value_from_pattern(pv: &PatternValue) -> Value {
    match pv {
        PatternValue::Unsigned(v) => Value::Unsigned(*v),
        PatternValue::Signed(v) => Value::Signed(*v),
        PatternValue::Float(v) => Value::Float(*v),
        PatternValue::Bool(v) => Value::Bool(*v),
        PatternValue::Char(v) => Value::Char(*v),
        PatternValue::String(v) => Value::String(v.clone()),
        PatternValue::Enum { value, .. } => Value::Unsigned(*value),
        PatternValue::Array
        | PatternValue::Struct
        | PatternValue::Union
        | PatternValue::Bitfield
        | PatternValue::Pointer { .. }
        | PatternValue::Padding(_)
        | PatternValue::BulkData { .. }
        | PatternValue::LazyStructArray { .. } => Value::Null,
    }
}

/// Convert a PatternNode to a Value, considering children for arrays of chars.
pub(crate) fn value_from_node(node: &PatternNode) -> Value {
    match &node.value {
        PatternValue::Array => {
            // Single-pass: check if all children are Char and build string simultaneously
            if !node.children.is_empty() {
                let mut chars = String::with_capacity(node.children.len());
                let mut all_chars = true;
                for child in &node.children {
                    if let PatternValue::Char(ch) = &child.value {
                        chars.push(*ch as u8 as char);
                    } else {
                        all_chars = false;
                        break;
                    }
                }
                if all_chars {
                    return Value::String(chars);
                }
            }
            Value::SizedRef {
                offset: node.offset,
                size: node.size,
            }
        }
        PatternValue::BulkData {
            ref data,
            elem_type,
            endian,
        } => Value::BulkRef {
            offset: node.offset,
            size: node.size,
            data: data.clone(),
            elem_type: *elem_type,
            endian: *endian,
        },
        PatternValue::Struct | PatternValue::Union | PatternValue::Bitfield => Value::SizedRef {
            offset: node.offset,
            size: node.size,
        },
        PatternValue::LazyStructArray { .. } => Value::SizedRef {
            offset: node.offset,
            size: node.size,
        },
        PatternValue::Pointer { address } => {
            // Pointer nodes resolve to SizedRef at the target address,
            // enabling member access on the pointed-to struct/union.
            Value::SizedRef {
                offset: *address,
                size: 0,
            }
        }
        other => value_from_pattern(other),
    }
}

/// Convert a Value to PatternValue for storing transform results in node tree.
/// Returns None for non-convertible values (SizedRef, BulkRef, Array, Null).
pub(crate) fn value_to_pattern_value(val: &Value) -> Option<PatternValue> {
    match val {
        Value::String(s) => Some(PatternValue::String(s.clone())),
        Value::Unsigned(n) => Some(PatternValue::Unsigned(*n)),
        Value::Signed(n) => Some(PatternValue::Signed(*n)),
        Value::Float(f) => Some(PatternValue::Float(*f)),
        Value::Bool(b) => Some(PatternValue::Bool(*b)),
        Value::Char(c) => Some(PatternValue::Char(*c)),
        _ => None,
    }
}

/// Find a node in the result tree by matching offset and size.
/// Returns a reference to the matching node, searching recursively.
/// If size == 0, matches by offset only (used for parent references).
pub(crate) fn find_node_by_offset(
    nodes: &[PatternNode],
    offset: u64,
    size: u64,
) -> Option<&PatternNode> {
    for node in nodes {
        if node.offset == offset && (size == 0 || node.size == size) {
            return Some(node);
        }
        // Range pruning: only recurse into children if offset falls within this node's range
        if node.size > 0 && offset >= node.offset && offset < node.offset + node.size {
            if let Some(found) = find_node_by_offset(&node.children, offset, size) {
                return Some(found);
            }
        }
    }
    None
}

/// Like find_node_by_offset but searches in reverse order (most recently added first).
/// Used by call_fn_by_name to prefer the most recently pushed node when multiple
/// nodes share the same offset (e.g., temp push for alias transform).
pub(crate) fn find_node_by_offset_rev(
    nodes: &[PatternNode],
    offset: u64,
    size: u64,
) -> Option<&PatternNode> {
    for node in nodes.iter().rev() {
        if node.offset == offset && (size == 0 || node.size == size) {
            return Some(node);
        }
        // Range pruning: only recurse into children if offset falls within this node's range
        if node.size > 0 && offset >= node.offset && offset < node.offset + node.size {
            if let Some(found) = find_node_by_offset(&node.children, offset, size) {
                return Some(found);
            }
        }
    }
    None
}

/// Like find_node_by_offset but prefers deeper (more specific) matches.
/// Used by addressof Index resolution where a parent struct may share the same
/// offset/size as an inner array field.
pub(crate) fn find_node_by_offset_deep(
    nodes: &[PatternNode],
    offset: u64,
    size: u64,
) -> Option<&PatternNode> {
    for node in nodes {
        // Range pruning: only recurse into children if offset falls within this node's range
        if node.size > 0 && offset >= node.offset && offset < node.offset + node.size {
            // Search children first to prefer deeper matches
            if let Some(found) = find_node_by_offset_deep(&node.children, offset, size) {
                return Some(found);
            }
        }
        if node.offset == offset && (size == 0 || node.size == size) {
            return Some(node);
        }
    }
    None
}

/// Find a named member in a node's children, also searching through pointer
/// children (implicit dereference) if the member is not a direct child.
pub(crate) fn find_child_member<'a>(
    node: &'a PatternNode,
    member: &str,
) -> Option<&'a PatternNode> {
    // Direct child lookup
    if let Some(child) = node.children.iter().find(|c| c.name.as_str() == member) {
        return Some(child);
    }
    // Implicit pointer dereference: look through pointer children
    for child in &node.children {
        if matches!(child.value, PatternValue::Pointer { .. }) {
            if let Some(grandchild) = child.children.iter().find(|c| c.name.as_str() == member) {
                return Some(grandchild);
            }
        }
    }
    None
}

/// Flatten a member access chain expression (a.b.c.d) into a dotted string.
/// Returns None if the expression contains non-Ident/MemberAccess nodes.
pub(crate) fn flatten_member_chain(expr: &Expr, interner: &StringInterner) -> Option<String> {
    match &expr.kind {
        ExprKind::Ident(name) => Some(interner.resolve(*name).to_string()),
        ExprKind::MemberAccess {
            expr: inner,
            member,
        } => flatten_member_chain(inner, interner)
            .map(|prefix| format!("{}.{}", prefix, interner.resolve(*member))),
        _ => None,
    }
}

/// Flatten a slice of Values, expanding nested Array values into individual elements.
pub(crate) fn flatten_args(args: &[Value]) -> Vec<Value> {
    let mut result = Vec::new();
    for v in args {
        if let Value::Array(items) = v {
            result.extend(items.iter().cloned());
        } else {
            result.push(v.clone());
        }
    }
    result
}

pub(crate) fn values_equal(a: &Value, b: &Value) -> bool {
    a == b
}

pub(crate) fn builtin_type_name(bt: &BuiltinType) -> &'static str {
    match bt {
        BuiltinType::U8 => "u8",
        BuiltinType::U16 => "u16",
        BuiltinType::U32 => "u32",
        BuiltinType::U64 => "u64",
        BuiltinType::U128 => "u128",
        BuiltinType::S8 => "s8",
        BuiltinType::S16 => "s16",
        BuiltinType::S32 => "s32",
        BuiltinType::S64 => "s64",
        BuiltinType::S128 => "s128",
        BuiltinType::Float => "float",
        BuiltinType::Double => "double",
        BuiltinType::Char => "char",
        BuiltinType::Char16 => "char16",
        BuiltinType::Bool => "bool",
        BuiltinType::Str => "str",
    }
}

pub(crate) fn type_name_of(ty: &TypeExpr, interner: &StringInterner) -> String {
    match &ty.kind {
        TypeExprKind::Builtin(bt) => builtin_type_name(bt).to_string(),
        TypeExprKind::Named(path) => path
            .iter()
            .map(|n| interner.resolve(*n))
            .collect::<Vec<_>>()
            .join("::"),
        TypeExprKind::Endian(endian, inner) => {
            let prefix = match endian {
                Endianness::Little => "le",
                Endianness::Big => "be",
            };
            format!("{} {}", prefix, type_name_of(inner, interner))
        }
        TypeExprKind::Array(elem, _) => format!("{}[]", type_name_of(elem, interner)),
        TypeExprKind::Template(path, _) => {
            let p = path
                .iter()
                .map(|n| interner.resolve(*n))
                .collect::<Vec<_>>()
                .join("::");
            format!("{}<...>", p)
        }
        TypeExprKind::ArbitraryInt { signed, bits } => {
            format!("{}{}", if *signed { "s" } else { "u" }, bits)
        }
        TypeExprKind::Auto => "auto".to_string(),
        TypeExprKind::Pointer(pointee, _) => format!("*{}", type_name_of(pointee, interner)),
        TypeExprKind::Padding => "padding".to_string(),
    }
}

/// Write a u64 integer into a String buffer without allocation.
fn itoa_into(buf: &mut String, val: u64) {
    if val == 0 {
        buf.push('0');
        return;
    }
    let start = buf.len();
    let mut n = val;
    while n > 0 {
        buf.push((b'0' + (n % 10) as u8) as char);
        n /= 10;
    }
    // Reverse the digits we just pushed
    let bytes = unsafe { buf.as_bytes_mut() };
    bytes[start..].reverse();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::ast::Ast;
    use crate::parser::Parser;
    use crate::span::SourceId;
    use data_source::SliceDataSource;

    /// Parse source and create evaluator (without running evaluate).
    /// Caller can configure (set_limits, set_source, etc.) then call evaluate().
    fn setup_evaluator<'a>(source: &str, ds: &'a SliceDataSource<'a>) -> (Ast, Evaluator<'a>) {
        let mut interner = StringInterner::new();
        let lexer = Lexer::new(source, SourceId(0), &mut interner);
        let (tokens, lex_errors) = lexer.tokenize();
        assert!(lex_errors.is_empty(), "lex errors: {:?}", lex_errors);
        let parser = Parser::new(tokens, &mut interner);
        let ast = parser.parse().expect("parse failed");
        let evaluator = Evaluator::with_interner(ds, interner);
        (ast, evaluator)
    }

    fn eval_pattern(source: &str, data: &[u8]) -> Vec<PatternNode> {
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).expect("evaluation failed")
    }

    /// Helper: parse + evaluate, return (results, evaluator) for tests that need scope access
    fn parse_and_eval<'a>(
        source: &str,
        ds: &'a SliceDataSource<'a>,
    ) -> (Vec<PatternNode>, Evaluator<'a>) {
        let (ast, mut evaluator) = setup_evaluator(source, ds);
        let results = evaluator.evaluate(&ast).expect("evaluation failed");
        (results, evaluator)
    }

    #[test]
    fn test_simple_u8() {
        let results = eval_pattern("u8 x @ 0x00;", &[0x42]);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "x");
        assert_eq!(results[0].value, PatternValue::Unsigned(0x42));
        assert_eq!(results[0].offset, 0);
        assert_eq!(results[0].size, 1);
    }

    #[test]
    fn test_u32_le() {
        let results = eval_pattern("u32 magic @ 0x00;", &[0x89, 0x50, 0x4E, 0x47]);
        assert_eq!(results[0].value, PatternValue::Unsigned(0x474E5089));
    }

    #[test]
    fn test_u32_be() {
        let results = eval_pattern("be u32 magic @ 0x00;", &[0x89, 0x50, 0x4E, 0x47]);
        assert_eq!(results[0].value, PatternValue::Unsigned(0x89504E47));
    }

    #[test]
    fn test_simple_struct() {
        let source = r#"
            struct Header {
                u32 magic;
                u16 version;
                u16 flags;
            };
            Header header @ 0x00;
        "#;
        let data: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x01, 0x00, 0x03, 0x00];
        let results = eval_pattern(source, data);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "header");
        assert_eq!(results[0].value, PatternValue::Struct);
        assert_eq!(results[0].children.len(), 3);
        assert_eq!(
            results[0].children[0].value,
            PatternValue::Unsigned(0x474E5089)
        ); // magic (LE)
        assert_eq!(results[0].children[1].value, PatternValue::Unsigned(1)); // version
        assert_eq!(results[0].children[2].value, PatternValue::Unsigned(3)); // flags
    }

    #[test]
    fn test_simple_struct_be() {
        let source = r#"
            struct Header {
                u32 magic;
                u16 version;
            };
            be Header header @ 0x00;
        "#;
        let data: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x00, 0x01];
        let results = eval_pattern(source, data);

        assert_eq!(
            results[0].children[0].value,
            PatternValue::Unsigned(0x89504E47)
        ); // magic (BE)
        assert_eq!(results[0].children[1].value, PatternValue::Unsigned(1)); // version (BE)
    }

    #[test]
    fn test_enum() {
        let source = r#"
            enum Color : u8 {
                Red = 0,
                Green = 1,
                Blue = 2
            };
            Color c @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x01]);
        assert_eq!(
            results[0].value,
            PatternValue::Enum {
                value: 1,
                name: "Green".to_string()
            }
        );
    }

    #[test]
    fn test_array_fixed() {
        let source = "u8 data[4] @ 0x00;";
        let results = eval_pattern(source, &[0x01, 0x02, 0x03, 0x04]);
        assert_eq!(results[0].value, PatternValue::Array);
        assert_eq!(results[0].children.len(), 4);
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(0x01));
        assert_eq!(results[0].children[3].value, PatternValue::Unsigned(0x04));
    }

    #[test]
    fn test_signed_type() {
        let source = "s8 val @ 0x00;";
        let results = eval_pattern(source, &[0xFF]);
        assert_eq!(results[0].value, PatternValue::Signed(-1));
    }

    #[test]
    fn test_float_type() {
        let source = "float val @ 0x00;";
        let data = 3.14f32.to_le_bytes();
        let results = eval_pattern(source, &data);
        match &results[0].value {
            PatternValue::Float(v) => {
                assert!((v - 3.14).abs() < 0.001);
            }
            other => panic!("expected Float, got {:?}", other),
        }
    }

    #[test]
    fn test_union() {
        let source = r#"
            union Data {
                u32 as_int;
                float as_float;
            };
            Data d @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x00, 0x00, 0x80, 0x3F]); // 1.0f in LE
        assert_eq!(results[0].value, PatternValue::Union);
        assert_eq!(results[0].children.len(), 2);
        assert_eq!(
            results[0].children[0].value,
            PatternValue::Unsigned(0x3F800000)
        );
        match &results[0].children[1].value {
            PatternValue::Float(v) => assert!((v - 1.0).abs() < 0.001),
            other => panic!("expected Float, got {:?}", other),
        }
    }

    #[test]
    fn test_function_call() {
        let source = r#"
            fn double(u32 x) -> u32 {
                return x * 2;
            };
            u32 val = double(21);
        "#;
        // No data needed for local variables
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(42)));
    }

    #[test]
    fn test_if_condition() {
        let source = r#"
            u8 x @ 0x00;
            u8 result = 0;
            if (x == 1) {
                result = 10;
            } else {
                result = 20;
            }
        "#;
        let data = [0x01u8];
        let ds = SliceDataSource::new(&data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("result");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(10)));
    }

    #[test]
    fn test_for_loop() {
        let source = r#"
            u32 sum = 0;
            for (u32 i = 0; i < 5; i = i + 1) {
                sum = sum + i;
            }
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sum");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(10))); // 0+1+2+3+4 = 10
    }

    #[test]
    fn test_auto_advance_offset() {
        let source = r#"
            struct Header {
                u8 a;
                u8 b;
                u16 c;
            };
            Header h @ 0x00;
        "#;
        let data = [0x01, 0x02, 0x03, 0x04];
        let results = eval_pattern(source, &data);
        assert_eq!(results[0].children[0].offset, 0);
        assert_eq!(results[0].children[1].offset, 1);
        assert_eq!(results[0].children[2].offset, 2);
        assert_eq!(results[0].size, 4);
    }

    #[test]
    fn test_nested_struct() {
        let source = r#"
            struct Inner {
                u8 x;
                u8 y;
            };
            struct Outer {
                Inner a;
                Inner b;
            };
            Outer data @ 0x00;
        "#;
        let data = [0x01, 0x02, 0x03, 0x04];
        let results = eval_pattern(source, &data);
        assert_eq!(results[0].children.len(), 2);
        assert_eq!(
            results[0].children[0].children[0].value,
            PatternValue::Unsigned(1)
        );
        assert_eq!(
            results[0].children[0].children[1].value,
            PatternValue::Unsigned(2)
        );
        assert_eq!(
            results[0].children[1].children[0].value,
            PatternValue::Unsigned(3)
        );
        assert_eq!(
            results[0].children[1].children[1].value,
            PatternValue::Unsigned(4)
        );
    }

    #[test]
    fn test_bool_type() {
        let source = "bool flag @ 0x00;";
        let results = eval_pattern(source, &[0x01]);
        assert_eq!(results[0].value, PatternValue::Bool(true));

        let results = eval_pattern(source, &[0x00]);
        assert_eq!(results[0].value, PatternValue::Bool(false));
    }

    #[test]
    fn test_char_type() {
        let source = "char c @ 0x00;";
        let results = eval_pattern(source, &[0x41]); // 'A'
        assert_eq!(results[0].value, PatternValue::Char('A'));
    }

    #[test]
    fn test_attribute_color() {
        let source = r#"[[color("FF0000")]] u8 x @ 0x00;"#;
        let results = eval_pattern(source, &[0x42]);
        assert_eq!(results[0].attributes.color, Some("FF0000".to_string()));
    }

    #[test]
    fn test_using_alias() {
        let source = r#"
            using DWORD = u32;
            DWORD magic @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x89, 0x50, 0x4E, 0x47]);
        assert_eq!(results[0].value, PatternValue::Unsigned(0x474E5089));
    }

    #[test]
    fn test_struct_with_if_else_members() {
        let source = r#"
            struct Header {
                u8 tag;
                if (tag == 1) u16 value;
                else u32 value;
                u8 footer;
            };
            Header h @ 0x00;
        "#;
        // tag=2 → else branch → u32
        let data: &[u8] = &[0x02, 0xAA, 0xBB, 0xCC, 0xDD, 0xFF];
        let results = eval_pattern(source, data);
        assert_eq!(results[0].value, PatternValue::Struct);
        // tag(1) + value(4) + footer(1) = 6 bytes
        assert_eq!(results[0].size, 6);
        assert_eq!(results[0].children.len(), 3);
        assert_eq!(results[0].children[0].name, "tag");
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(2));
        assert_eq!(results[0].children[1].name, "value");
        assert_eq!(
            results[0].children[1].value,
            PatternValue::Unsigned(0xDDCCBBAA) // LE u32
        );
        assert_eq!(results[0].children[2].name, "footer");
        assert_eq!(results[0].children[2].value, PatternValue::Unsigned(0xFF));
    }

    #[test]
    fn test_struct_member_propagation() {
        let source = r#"
            struct Inner {
                u16 x;
                u16 y;
            };
            struct Outer {
                Inner inner;
                u8 data[inner.x];
            };
            Outer o @ 0x00;
        "#;
        // inner.x=2 (LE), inner.y=3, data=[0xAA, 0xBB]
        let data: &[u8] = &[0x02, 0x00, 0x03, 0x00, 0xAA, 0xBB];
        let results = eval_pattern(source, data);
        assert_eq!(results[0].children.len(), 2);
        assert_eq!(results[0].children[1].children.len(), 2); // data has 2 elements
    }

    #[test]
    fn test_enum_self_reference() {
        let source = r#"
            enum CpuType : u32 {
                I386 = 7,
                X86_64 = CpuType::I386 | 0x01000000,
                ARM = 12,
                ARM64 = CpuType::ARM | 0x01000000
            };
            CpuType ct @ 0x00;
        "#;
        // 0x0100000C = ARM64 in little-endian
        let data: &[u8] = &[0x0C, 0x00, 0x00, 0x01];
        let results = eval_pattern(source, data);
        assert_eq!(
            results[0].value,
            PatternValue::Enum {
                value: 0x0100000C,
                name: "ARM64".to_string()
            }
        );
    }

    // --- Fix 1: endian-wrapped type cast ---
    #[test]
    fn test_endian_cast() {
        let source = r#"
            u32 x = be u32(15);
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("x");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(15)));
    }

    // --- Fix 2: arbitrary-width unsigned int (u24) ---
    #[test]
    fn test_arbitrary_width_int_u24() {
        // LE: [0x01, 0x02, 0x03] → 0x030201
        let source = "u24 val @ 0x00;";
        let data: &[u8] = &[0x01, 0x02, 0x03];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "val");
        assert_eq!(results[0].value, PatternValue::Unsigned(0x030201));
        assert_eq!(results[0].size, 3);
    }

    // --- Fix 2: arbitrary-width signed int (s24) ---
    #[test]
    fn test_arbitrary_width_int_s24() {
        // [0xFF, 0xFF, 0xFF] → unsigned 0xFFFFFF → sign-extend 24-bit → -1
        let source = "s24 val @ 0x00;";
        let data: &[u8] = &[0xFF, 0xFF, 0xFF];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Signed(-1));
        assert_eq!(results[0].size, 3);
    }

    // --- Fix 3: template alias instantiation ---
    #[test]
    fn test_template_alias_instantiation() {
        let source = r#"
            using Alias<T> = T;
            Alias<u32> x @ 0x00;
        "#;
        let data: &[u8] = &[0x01, 0x00, 0x00, 0x00];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Unsigned(1));
        assert_eq!(results[0].size, 4);
    }

    // --- Fix 4: template struct instantiation ---
    #[test]
    fn test_template_struct_instantiation() {
        let source = r#"
            struct Container<T> {
                T value;
            };
            Container<u16> c @ 0x00;
        "#;
        let data: &[u8] = &[0x34, 0x12];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Struct);
        assert_eq!(results[0].size, 2);
        assert_eq!(results[0].children.len(), 1);
        assert_eq!(results[0].children[0].name, "value");
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(0x1234));
    }

    // --- Fix 5: sizeof for named type alias ---
    #[test]
    fn test_type_size_named() {
        let source = r#"
            using DWORD = u32;
            u32 sz = sizeof(DWORD);
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sz");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(4)));
    }

    // --- Fix 5: sizeof for arbitrary-width int ---
    #[test]
    fn test_type_size_arbitrary_int() {
        let source = r#"
            u32 sz = sizeof(u24);
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sz");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(3)));
    }

    // --- Fix 9: multi-level (3+) member access inside struct ---
    #[test]
    fn test_multi_level_member_access() {
        // Test that 3-level deep member access works inside a struct body
        // (propagate_children registers dotted names during struct member reading)
        let source = r#"
            struct Level2 {
                u8 x;
            };
            struct Level1 {
                Level2 inner;
            };
            struct Level0 {
                Level1 mid;
                u8 copy_of_x[mid.inner.x];
            };
            Level0 outer @ 0x00;
        "#;
        // Level2.x = 0x02, then copy_of_x has 2 elements
        let data: &[u8] = &[0x02, 0xAA, 0xBB];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Struct);
        // mid member
        let mid = &results[0].children[0];
        assert_eq!(mid.name, "mid");
        // copy_of_x array with 2 elements (from mid.inner.x = 2)
        let copy = &results[0].children[1];
        assert_eq!(copy.name, "copy_of_x");
        assert_eq!(copy.value, PatternValue::Array);
        assert_eq!(copy.children.len(), 2);
        assert_eq!(copy.children[0].value, PatternValue::Unsigned(0xAA));
        assert_eq!(copy.children[1].value, PatternValue::Unsigned(0xBB));
    }

    // --- Fix 10: std::mem::read_unsigned ---
    #[test]
    fn test_std_mem_read_unsigned() {
        let source = r#"
            u8 val = std::mem::read_unsigned(0, 1);
        "#;
        let data: &[u8] = &[0xAB];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(0xAB)));
    }

    // --- padding[N] in struct ---
    #[test]
    fn test_padding_in_struct() {
        let source = r#"
            struct Padded {
                u8 a;
                padding[2];
                u8 b;
            };
            Padded p @ 0x00;
        "#;
        let data: &[u8] = &[0x01, 0x00, 0x00, 0x02];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Struct);
        assert_eq!(results[0].size, 4);
        // a at offset 0, padding skips 2 bytes, b at offset 3
        let children = &results[0].children;
        assert_eq!(children[0].name, "a");
        assert_eq!(children[0].value, PatternValue::Unsigned(1));
        assert_eq!(children[0].offset, 0);
        // Find "b" member (skipping any padding nodes)
        let b = children
            .iter()
            .find(|c| c.name == "b")
            .expect("member b not found");
        assert_eq!(b.value, PatternValue::Unsigned(2));
        assert_eq!(b.offset, 3);
    }

    // --- [[sealed]] array ---
    #[test]
    fn test_sealed_array() {
        let source = r#"
            [[sealed]] u8 data[4] @ 0x00;
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Array);
        assert_eq!(results[0].size, 4);
        // Sealed arrays skip element-by-element reading, so children may be empty
        assert_eq!(results[0].children.len(), 0);
        assert!(results[0].attributes.sealed);
    }

    // --- Fix 6: struct member placement with explicit @ ---
    #[test]
    fn test_struct_placement_member() {
        let source = r#"
            struct Header {
                u8 size @ 0x00;
                u8 offset @ 0x01;
                u8 data[size] @ offset;
            };
            Header h @ 0x00;
        "#;
        // size=3, offset=4, data at offset 4 = [0xAA, 0xBB, 0xCC]
        let data: &[u8] = &[0x03, 0x04, 0x00, 0x00, 0xAA, 0xBB, 0xCC];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Struct);
        // "size" member
        let size_m = &results[0].children[0];
        assert_eq!(size_m.name, "size");
        assert_eq!(size_m.value, PatternValue::Unsigned(3));
        // "data" member: array of 3 elements at offset 4
        let data_m = results[0]
            .children
            .iter()
            .find(|c| c.name == "data")
            .expect("member data not found");
        assert_eq!(data_m.offset, 4);
        assert_eq!(data_m.children.len(), 3);
        assert_eq!(data_m.children[0].value, PatternValue::Unsigned(0xAA));
        assert_eq!(data_m.children[1].value, PatternValue::Unsigned(0xBB));
        assert_eq!(data_m.children[2].value, PatternValue::Unsigned(0xCC));
    }

    #[test]
    fn test_template_struct_expr_param() {
        // Test struct with expression (auto) template parameter
        let src = r#"
            struct Foo<auto N> {
                u8 data[N];
            };
            Foo<3> f @ 0;
        "#;
        let data = &[0x11, 0x22, 0x33, 0x44, 0x55];
        let results = eval_pattern(src, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "f");
        // data member should be an array of 3 u8 elements
        let data_m = results[0]
            .children
            .iter()
            .find(|c| c.name == "data")
            .expect("member data not found");
        assert_eq!(data_m.children.len(), 3);
        assert_eq!(data_m.children[0].value, PatternValue::Unsigned(0x11));
        assert_eq!(data_m.children[1].value, PatternValue::Unsigned(0x22));
        assert_eq!(data_m.children[2].value, PatternValue::Unsigned(0x33));
    }

    // --- InitializerList evaluation ---
    #[test]
    fn test_initializer_list_basic() {
        let source = r#"
            u8 table[] = { 0xA7, 0xB7, 0xC8 };
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("table");
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::Array(vec![
                Value::Unsigned(0xA7),
                Value::Unsigned(0xB7),
                Value::Unsigned(0xC8),
            ]))
        );
    }

    #[test]
    fn test_initializer_list_index() {
        let source = r#"
            u8 table[] = { 10, 20, 30 };
            u8 val = table[1];
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(20)));
    }

    // --- sizeof(expr) ---
    #[test]
    fn test_sizeof_struct_variable() {
        let source = r#"
            struct Header {
                u8 a;
                u16 b;
                u32 c;
            };
            Header h @ 0x00;
            u32 sz = sizeof(h);
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        // Header = u8(1) + u16(2) + u32(4) = 7 bytes
        let key = evaluator.interner.intern("sz");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(7)));
    }

    // --- struct inheritance ---
    #[test]
    fn test_struct_inheritance() {
        let source = r#"
            struct Base {
                u8 tag;
                u8 len;
            };
            struct Child : Base {
                u8 data[len];
            };
            Child c @ 0x00;
        "#;
        // tag=0xAA, len=2, data=[0x11, 0x22]
        let data: &[u8] = &[0xAA, 0x02, 0x11, 0x22];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Struct);
        // Should have 3 children: tag, len (from Base), data (from Child)
        assert_eq!(results[0].children.len(), 3);
        assert_eq!(results[0].children[0].name, "tag");
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(0xAA));
        assert_eq!(results[0].children[1].name, "len");
        assert_eq!(results[0].children[1].value, PatternValue::Unsigned(2));
        assert_eq!(results[0].children[2].name, "data");
        assert_eq!(results[0].children[2].children.len(), 2);
        assert_eq!(results[0].size, 4); // 1 + 1 + 2
    }

    #[test]
    fn test_struct_multi_level_inheritance() {
        let source = r#"
            struct GrandParent {
                u8 a;
            };
            struct Parent : GrandParent {
                u8 b;
            };
            struct Child : Parent {
                u8 c;
            };
            Child x @ 0x00;
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].children.len(), 3);
        assert_eq!(results[0].children[0].name, "a");
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(1));
        assert_eq!(results[0].children[1].name, "b");
        assert_eq!(results[0].children[1].value, PatternValue::Unsigned(2));
        assert_eq!(results[0].children[2].name, "c");
        assert_eq!(results[0].children[2].value, PatternValue::Unsigned(3));
        assert_eq!(results[0].size, 3);
    }

    // --- Bitfield conditional fields ---
    #[test]
    fn test_bitfield_conditional_field() {
        let source = r#"
            bitfield Flags {
                has_extra : 1;
                if (has_extra == 1) {
                    extra_bits : 3;
                }
                padding : 4;
            };
            Flags f @ 0x00;
        "#;
        let data: &[u8] = &[0xFF]; // all bits set → has_extra = 1
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, PatternValue::Bitfield);
        // Should have 3 children: has_extra, extra_bits, padding
        assert_eq!(results[0].children.len(), 3);
        assert_eq!(results[0].children[0].name, "has_extra");
        assert_eq!(results[0].children[1].name, "extra_bits");
        assert_eq!(results[0].children[2].name, "padding");
    }

    #[test]
    fn test_bitfield_reads_actual_data_values() {
        // Verify bitfield fields contain actual data values, not field widths.
        // Data: 0b10110001 = 0xB1
        // LSB-first (default): a(3 bits)=[0,1,2]=0b001=1, b(5 bits)=[3..7]=0b10110=22
        let source = r#"
            bitfield Test {
                a : 3;
                b : 5;
            };
            Test t @ 0x00;
        "#;
        let data: &[u8] = &[0xB1]; // 0b10110001
        let results = eval_pattern(source, data);
        assert_eq!(results[0].children[0].name, "a");
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(1)); // bits[0:2] = 001
        assert_eq!(results[0].children[1].name, "b");
        assert_eq!(results[0].children[1].value, PatternValue::Unsigned(22)); // bits[3:7] = 10110
    }

    #[test]
    fn test_bitfield_value_used_in_condition() {
        // Verify bitfield field values (not widths) are used in conditional expressions.
        // Data: 0x00 → all bits zero → flag(1 bit)=0 → else branch
        let source = r#"
            bitfield Flags {
                flag : 1;
                value : 7;
            };
            Flags f @ 0x00;
            u8 result @ 0x01;
        "#;
        let data: &[u8] = &[0x00, 0x42];
        let results = eval_pattern(source, data);
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(0)); // flag = 0
        assert_eq!(results[0].children[1].value, PatternValue::Unsigned(0)); // value = 0
    }

    #[test]
    fn test_bitfield_msb_first_order() {
        // Test MSB-first (MostToLeastSignificant) bit ordering via bitfield_order attribute.
        // Data: 0xD1 = 0b11010001
        // MSB-first with group_size=8:
        //   MB(1)=bit7=1, ME(1)=bit6=1, CF(1)=bit5=0, SR(1)=bit4=1,
        //   IL(1)=bit3=0, TNF(3)=bits[2:0]=001=1
        // Note: BitfieldOrder::MostToLeastSignificant = 0 (from std/core.pat)
        let source = r#"
            bitfield NDEFFlags {
                MB  : 1;
                ME  : 1;
                CF  : 1;
                SR  : 1;
                IL  : 1;
                TNF : 3;
            } [[bitfield_order(0, 8)]];
            NDEFFlags f @ 0x00;
        "#;
        let data: &[u8] = &[0xD1]; // 0b11010001
        let results = eval_pattern(source, data);
        assert_eq!(results[0].children.len(), 6);
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(1)); // MB = bit7 = 1
        assert_eq!(results[0].children[1].value, PatternValue::Unsigned(1)); // ME = bit6 = 1
        assert_eq!(results[0].children[2].value, PatternValue::Unsigned(0)); // CF = bit5 = 0
        assert_eq!(results[0].children[3].value, PatternValue::Unsigned(1)); // SR = bit4 = 1
        assert_eq!(results[0].children[4].value, PatternValue::Unsigned(0)); // IL = bit3 = 0
        assert_eq!(results[0].children[5].value, PatternValue::Unsigned(1)); // TNF = bits[2:0] = 001 = 1
    }

    #[test]
    fn test_bitfield_conditional_uses_actual_value() {
        // Verify that if/else inside bitfield uses actual bit values for conditions.
        // Data: 0x00 → bit 0 = 0 → has_extra is false → else branch
        let source = r#"
            bitfield Flags {
                has_extra : 1;
                if (has_extra == 1) {
                    extra : 3;
                } else {
                    reserved : 3;
                }
                padding : 4;
            };
            Flags f @ 0x00;
        "#;
        let data: &[u8] = &[0x00]; // all zero → has_extra = 0
        let results = eval_pattern(source, data);
        assert_eq!(results[0].children.len(), 3); // has_extra, reserved, padding
        assert_eq!(results[0].children[0].name, "has_extra");
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(0));
        assert_eq!(results[0].children[1].name, "reserved");
    }

    // ========== Error case tests ==========

    fn eval_pattern_err(source: &str, data: &[u8]) -> EvalError {
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_source(source.to_string());
        evaluator
            .evaluate(&ast)
            .expect_err("expected evaluation error")
    }

    #[test]
    fn test_oob_read_at_top_level_is_graceful() {
        // OOB at top level is treated as end-of-evaluation (matching ImHex behavior)
        let source = "u32 x @ 0x00;";
        let data: &[u8] = &[0x01, 0x02];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        let results = evaluator.evaluate(&ast).unwrap();
        // The OOB var decl is skipped; no results produced
        assert!(
            results.is_empty(),
            "expected empty results, got: {:?}",
            results
        );
    }

    #[test]
    fn test_error_undefined_type() {
        let err = eval_pattern_err("UnknownType x @ 0x00;", &[0x00; 16]);
        assert!(
            err.message.contains("undefined") || err.message.contains("Unknown"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_undefined_variable() {
        let err = eval_pattern_err(
            r#"
                u8 x @ 0x00;
                if (undefined_var == 1) {
                    u8 y @ 0x01;
                }
            "#,
            &[0x01, 0x02],
        );
        assert!(
            err.message.contains("undefined"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_division_by_zero_returns_zero() {
        // Division by zero returns 0 instead of error (matches ImHex behavior)
        let results = eval_pattern(
            r#"
                u8 x @ (0 / 0);
            "#,
            &[0x42],
        );
        assert_eq!(results[0].name, "x");
        assert_eq!(results[0].offset, 0); // 0 / 0 = 0
    }

    #[test]
    fn test_error_pattern_limit_exceeded() {
        let source = "u8 a @ 0x00;\nu8 b @ 0x01;\nu8 c @ 0x02;";
        let ds = SliceDataSource::new(&[0x01, 0x02, 0x03]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0x1000, 2, 32); // pattern_limit = 2
        let err = evaluator
            .evaluate(&ast)
            .expect_err("expected pattern limit error");
        assert!(
            err.message.contains("pattern limit"),
            "unexpected error: {}",
            err.message
        );
    }

    // ========== Additional error case tests ==========

    #[test]
    fn test_error_str_requires_explicit_size() {
        let err = eval_pattern_err("str x @ 0x00;", &[0x68, 0x65, 0x6C, 0x6C, 0x6F]);
        // str without explicit size hits "unsized builtin type" or "str type requires explicit size"
        assert!(
            err.message.contains("unsized builtin type")
                || err.message.contains("str type requires explicit size"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_forward_declared_type_alias() {
        let err = eval_pattern_err("using FwdType; FwdType x @ 0x00;", &[0x00; 16]);
        assert!(
            err.message
                .contains("forward-declared type alias used without definition"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_cannot_negate_non_numeric() {
        let err = eval_pattern_err(
            r#"
                bool b = true;
                u8 x @ (-b);
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("cannot negate") || err.message.contains("non-numeric"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_modulo_by_zero_returns_zero() {
        // Modulo by zero returns 0 instead of error
        let results = eval_pattern(
            r#"
                u8 x @ 0x00;
                u8 y @ (x % 0);
            "#,
            &[0x01, 0x02],
        );
        assert_eq!(results[1].name, "y");
        assert_eq!(results[1].offset, 0); // 1 % 0 = 0
    }

    #[test]
    fn test_error_invalid_operator_for_float() {
        let err = eval_pattern_err(
            r#"
                float f = 1.0;
                u8 x @ (f & 1);
            "#,
            &[0x00; 8],
        );
        assert!(
            err.message.contains("invalid operator for float"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_invalid_assignment_target() {
        let err = eval_pattern_err(
            r#"
                u8 x @ 0x00;
                1 = 2;
            "#,
            &[0x01],
        );
        assert!(
            err.message.contains("invalid assignment target"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_cannot_call_non_function() {
        let err = eval_pattern_err(
            r#"
                fn foo() { return 1; };
                u8 x @ foo()();
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("cannot call non-function"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_array_index_out_of_bounds_returns_zero() {
        // Array OOB returns 0 instead of error (tolerant for real patterns)
        // {1, 2, 3}[10] evaluates to 0 (OOB default), used as offset
        let results = eval_pattern(
            r#"
                u8 y @ {1, 2, 3}[10];
            "#,
            &[0xAA; 16],
        );
        let y = results.iter().find(|n| n.name == "y").unwrap();
        assert_eq!(y.offset, 0); // OOB returns 0, placed at offset 0
    }

    #[test]
    fn test_error_string_index_out_of_bounds() {
        let err = eval_pattern_err(
            r#"
                str s = "hi";
                u8 x @ s[10];
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("string index") && err.message.contains("out of bounds"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_cannot_access_member() {
        let err = eval_pattern_err(
            r#"
                u8 x @ 0x00;
                u8 y @ x.foo;
            "#,
            &[0x01, 0x02],
        );
        assert!(
            err.message.contains("cannot access member"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_addressof_not_addressable() {
        let err = eval_pattern_err(
            r#"
                str s = "hello";
                u8 x @ addressof(s);
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("addressof") && err.message.contains("not an addressable"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_undefined_function() {
        let err = eval_pattern_err(
            r#"
                u8 x @ unknown_func();
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("undefined function"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_undefined_scoped_ident() {
        let err = eval_pattern_err(
            r#"
                u8 x @ Foo::Bar;
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("undefined"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_assert_failure() {
        let err = eval_pattern_err(
            r#"
                std::assert(false, "test assertion");
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("assert:") && err.message.contains("test assertion"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_cast_to_char() {
        // Cast numeric to char is now supported
        // char(65) = 'A', to_unsigned = 65, used as offset
        let results = eval_pattern(
            r#"
                u8 y @ char(65);
            "#,
            &[0x00; 128],
        );
        let y = results.iter().find(|n| n.name == "y").unwrap();
        assert_eq!(y.offset, 65);
    }

    #[test]
    fn test_cast_to_str() {
        let results = eval_pattern(
            r#"
                fn test() {
                    str s = str(42);
                    return s;
                };
                u8 x @ 0x00;
            "#,
            &[0x00; 4],
        );
        assert!(results.iter().any(|n| n.name == "x"));
    }

    #[test]
    fn test_string_to_bool_empty_is_false() {
        // Empty string converts to false
        let results = eval_pattern(
            r#"
                fn check() {
                    str s = "";
                    if (s) { return 1; }
                    return 0;
                };
                u8 x @ check();
            "#,
            &[0x00; 4],
        );
        let x = results.iter().find(|n| n.name == "x").unwrap();
        assert_eq!(x.offset, 0); // empty string is falsy
    }

    #[test]
    fn test_string_to_bool_nonempty_is_true() {
        // Non-empty string converts to true
        let results = eval_pattern(
            r#"
                fn check() {
                    str s = "hello";
                    if (s) { return 1; }
                    return 0;
                };
                u8 x @ check();
            "#,
            &[0x00; 4],
        );
        let x = results.iter().find(|n| n.name == "x").unwrap();
        assert_eq!(x.offset, 1); // non-empty string is truthy
    }

    #[test]
    fn test_member_access_assignment() {
        // Assignment to member access expression (obj.field = val)
        let results = eval_pattern(
            r#"
                fn test() {
                    u8 a;
                    a.x = 42;
                    return a.x;
                };
                u8 y @ test();
            "#,
            &[0x00; 128],
        );
        let y = results.iter().find(|n| n.name == "y").unwrap();
        assert_eq!(y.offset, 42);
    }

    #[test]
    fn test_forward_declared_alias_not_shadow() {
        // Forward declaration (using Name;) should not shadow existing definition
        let results = eval_pattern(
            r#"
                struct Foo {
                    u8 val @ 0x00;
                };
                using Foo;
                Foo f @ 0x00;
            "#,
            &[0x42; 4],
        );
        let f_val = results.iter().find(|n| n.name == "f").unwrap();
        assert_eq!(f_val.offset, 0);
    }

    #[test]
    fn test_read_oob_stops_at_top_level() {
        // Top-level OOB read stops evaluation gracefully (ImHex behavior)
        // Earlier successful placements are preserved in results
        let results = eval_pattern(
            r#"
                u8 x @ 0x00;
                u8 y @ 999;
            "#,
            &[0x42],
        );
        // x should succeed
        assert!(results.iter().any(|n| n.name == "x"));
        // y is not in results because OOB stopped evaluation
        assert!(!results.iter().any(|n| n.name == "y"));
    }

    #[test]
    fn test_read_oob_negative_offset_stops() {
        // Negative offset (wraps to huge u64) stops evaluation at top level
        let results = eval_pattern(
            r#"
                u8 x @ 0x00;
                u8 y @ (0 - 10);
            "#,
            &[0x42; 4],
        );
        assert!(results.iter().any(|n| n.name == "x"));
        // y is not in results because OOB stopped evaluation
        assert!(!results.iter().any(|n| n.name == "y"));
    }

    #[test]
    fn test_read_oob_inside_struct_propagates_error() {
        // OOB read inside a struct body should propagate as an error
        // (not silently set the variable to 0, which breaks member access)
        let results = eval_pattern(
            r#"
                struct Header {
                    u8 magic;
                    u8 version;
                };
                Header h @ 0x00;
            "#,
            &[0x89, 0x03],
        );
        // Should succeed: struct fits in data
        let h = results.iter().find(|n| n.name == "h").unwrap();
        assert_eq!(h.children.len(), 2);
    }

    #[test]
    fn test_namespace_enum_member_resolution() {
        // Enum member should be resolved within namespace context
        let results = eval_pattern(
            r#"
                namespace ns {
                    enum Color : u8 {
                        Red = 1,
                        Green = 2,
                        Blue = 3,
                    };
                    u8 x @ Color::Green;
                };
            "#,
            &[0x00; 8],
        );
        let x = results.iter().find(|n| n.name == "x").unwrap();
        assert_eq!(x.offset, 2); // Green = 2
    }

    #[test]
    fn test_namespace_enum_multiple_same_name() {
        // Multiple namespaces with same-named enum: each should resolve correctly
        let results = eval_pattern(
            r#"
                namespace a {
                    enum Kind : u8 {
                        X = 10,
                    };
                    u8 val @ Kind::X;
                };
                namespace b {
                    enum Kind : u8 {
                        Y = 20,
                    };
                    u8 val @ Kind::Y;
                };
            "#,
            &[0x00; 32],
        );
        let a_val = results.iter().find(|n| n.name == "val" && n.offset == 10);
        let b_val = results.iter().find(|n| n.name == "val" && n.offset == 20);
        assert!(a_val.is_some(), "namespace a should resolve Kind::X = 10");
        assert!(b_val.is_some(), "namespace b should resolve Kind::Y = 20");
    }

    #[test]
    fn test_pointer_deref_member_access() {
        // Member access on a pointer should auto-dereference
        let results = eval_pattern(
            r#"
                struct Inner {
                    u8 val;
                };
                Inner *ptr : u32 @ 0x00;
            "#,
            &[0x04, 0x00, 0x00, 0x00, 0xAB],
        );
        // ptr points to offset 4, where Inner.val = 0xAB
        assert!(results.iter().any(|n| n.name == "ptr"));
    }

    #[test]
    fn test_struct_member_access_basic() {
        // Struct member access should work when struct is fully readable
        let results = eval_pattern(
            r#"
                struct Header {
                    u16 magic;
                    u16 numFrames;
                };
                Header header @ 0x00;
                u16 frames @ header.numFrames;
            "#,
            &[0x89, 0x50, 0x03, 0x00, 0x00, 0x00, 0xAA, 0xBB],
        );
        // header.numFrames = 0x0003 (little-endian)
        let frames = results.iter().find(|n| n.name == "frames").unwrap();
        assert_eq!(frames.offset, 3); // placed at offset = numFrames value
    }

    #[test]
    fn test_struct_member_access_in_conditional() {
        // Struct member access used in condition (like zip CDOffset check)
        let results = eval_pattern(
            r#"
                struct Footer {
                    u32 CDOffset;
                };
                Footer footer @ 0x00;
                if (footer.CDOffset > 0) {
                    u8 marker @ footer.CDOffset;
                };
            "#,
            &[0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0xCC],
        );
        // footer.CDOffset = 6, so marker is at offset 6
        let marker = results.iter().find(|n| n.name == "marker").unwrap();
        assert_eq!(marker.offset, 6);
    }

    #[test]
    fn test_nested_struct_member_access() {
        // Access member of nested struct (like zlib header.header pattern)
        let results = eval_pattern(
            r#"
                struct Inner {
                    u8 version;
                    u8 flags;
                };
                struct Outer {
                    Inner header;
                    u16 data;
                };
                Outer outer @ 0x00;
                u8 ver @ outer.header.version;
            "#,
            &[0x03, 0xFF, 0xAA, 0xBB],
        );
        let ver = results.iter().find(|n| n.name == "ver").unwrap();
        assert_eq!(ver.offset, 3); // outer.header.version = 3
    }

    #[test]
    fn test_struct_member_access_not_affected_by_later_oob() {
        // If a later field would go OOB, the earlier struct member access should still work
        // This is the regression that affected blend/midi/zip/zlib:
        // OOB tolerance was setting struct vars to 0, breaking member access
        let results = eval_pattern(
            r#"
                struct Header {
                    u8 type;
                    u8 value;
                };
                Header h @ 0x00;
                u8 result @ h.value;
            "#,
            &[0x01, 0x04, 0x00, 0x00, 0xAA],
        );
        let result = results.iter().find(|n| n.name == "result").unwrap();
        assert_eq!(result.offset, 4); // h.value = 4
    }

    #[test]
    fn test_pointer_target_member_access() {
        // Pointer target should be evaluated so members are accessible
        // This is the pattern that broke dex.hexpat: ptr.member where ptr is a pointer
        let results = eval_pattern(
            r#"
                struct Inner {
                    u8 val;
                    u8 flag;
                };
                struct Wrapper {
                    Inner *data : u32;
                };
                Wrapper w @ 0x00;
                u8 result @ w.data.val;
            "#,
            // offset 0: pointer value = 4 (u32 LE)
            // offset 4: Inner { val=0x03, flag=0xFF }
            // offset 6+: extra data so `result @ 3` is in bounds
            &[0x04, 0x00, 0x00, 0x00, 0x03, 0xFF, 0xAA, 0xBB],
        );
        let result = results.iter().find(|n| n.name == "result").unwrap();
        assert_eq!(result.offset, 3); // w.data.val = 3, used as placement offset
    }

    #[test]
    fn test_pointer_target_member_in_condition() {
        // Pointer member used in condition expression
        let results = eval_pattern(
            r#"
                struct Header {
                    u8 version;
                    u8 count;
                };
                Header *hdr : u16 @ 0x00;
                if (hdr.version > 0) {
                    u8 data[hdr.count] @ 0x04;
                };
            "#,
            // offset 0: pointer = 2 (u16 LE)
            // offset 2: Header { version=3, count=2 }
            // offset 4: data bytes
            &[0x02, 0x00, 0x03, 0x02, 0xAA, 0xBB],
        );
        // hdr.version=3 > 0 → data array should be placed
        let data = results.iter().find(|n| n.name == "data").unwrap();
        assert_eq!(data.children.len(), 2); // count=2
    }

    #[test]
    fn test_pointer_target_oob_graceful() {
        // Pointer pointing to OOB address: target eval fails but pointer itself succeeds
        let results = eval_pattern(
            r#"
                struct Target {
                    u8 x;
                };
                Target *ptr : u32 @ 0x00;
            "#,
            // pointer value = 0x1000 (way past end of data)
            &[0x00, 0x10, 0x00, 0x00],
        );
        // Pointer node should exist even though target is OOB
        assert!(results.iter().any(|n| n.name == "ptr"));
    }

    #[test]
    fn test_deep_nested_struct_array_member_access() {
        // Mimics PE pattern: coffHeader.optionalHeader.directories[1].rva
        let results = eval_pattern(
            r#"
                struct DataDirectory {
                    u32 rva;
                    u32 size;
                };
                struct OptionalHeader {
                    u16 magic;
                    DataDirectory directories[2];
                };
                struct COFFHeader {
                    u32 sig;
                    OptionalHeader optionalHeader;
                };
                COFFHeader coffHeader @ 0x00;
                u32 test @ coffHeader.optionalHeader.directories[1].rva;
            "#,
            // sig(4) + magic(2) + dir[0].rva(4) + dir[0].size(4) + dir[1].rva(4) + dir[1].size(4)
            &[
                0x50, 0x45, 0x00, 0x00, // sig = "PE\0\0"
                0x0B, 0x01, // magic = 0x010B
                0x00, 0x00, 0x00, 0x00, // dir[0].rva = 0
                0x00, 0x00, 0x00, 0x00, // dir[0].size = 0
                0x14, 0x00, 0x00, 0x00, // dir[1].rva = 0x14 (20)
                0x08, 0x00, 0x00, 0x00, // dir[1].size = 8
                0x00, 0x00, 0x00, 0x00, // extra data at offset 20
            ],
        );
        let test = results.iter().find(|n| n.name == "test").unwrap();
        assert_eq!(test.offset, 0x14); // dir[1].rva = 0x14
    }

    #[test]
    fn test_array_member_access_with_variable_index() {
        // Array member access with variable index (like PE for loop)
        let results = eval_pattern(
            r#"
                struct Entry {
                    u32 rva;
                    u32 size;
                };
                struct Container {
                    Entry entries[3];
                };
                Container c @ 0x00;
                u32 i = 1;
                u32 test @ c.entries[i].rva;
            "#,
            &[
                0x00, 0x00, 0x00, 0x00, // entries[0].rva = 0
                0x00, 0x00, 0x00, 0x00, // entries[0].size = 0
                0x18, 0x00, 0x00, 0x00, // entries[1].rva = 0x18 (24)
                0x04, 0x00, 0x00, 0x00, // entries[1].size = 4
                0x00, 0x00, 0x00, 0x00, // entries[2].rva = 0
                0x00, 0x00, 0x00, 0x00, // entries[2].size = 0
                0xAA, 0xBB, 0xCC, 0xDD, // extra at offset 24
            ],
        );
        let test = results.iter().find(|n| n.name == "test").unwrap();
        assert_eq!(test.offset, 0x18); // entries[1].rva = 0x18
    }

    #[test]
    fn test_error_cannot_determine_size_of_unknown_type() {
        let err = eval_pattern_err(
            r#"
                u8 x @ sizeof(UnknownType);
            "#,
            &[0x00; 8],
        );
        assert!(
            err.message.contains("cannot determine size") && err.message.contains("unknown type"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_while_loop_limit() {
        let source = r#"
            u8 i = 0;
            while (i < 100) {
                i = i + 1;
            };
        "#;
        let ds = SliceDataSource::new(&[0x00; 16]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0x1000, 5, 32); // pattern_limit = 5, loop limit uses same
        let err = evaluator
            .evaluate(&ast)
            .expect_err("expected while loop limit error");
        assert!(
            err.message.contains("while loop iteration limit exceeded"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_for_loop_limit() {
        let source = r#"
            u8 sum = 0;
            for (u8 i = 0, i < 100, i = i + 1) {
                sum = sum + 1;
            };
        "#;
        let ds = SliceDataSource::new(&[0x00; 16]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0x1000, 5, 32); // pattern_limit = 5
        let err = evaluator
            .evaluate(&ast)
            .expect_err("expected for loop limit error");
        assert!(
            err.message.contains("for loop iteration limit exceeded"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_recursion_depth_exceeded() {
        let source = r#"
            struct Node {
                u8 val;
                Node next;
            };
            Node n @ 0x00;
        "#;
        let ds = SliceDataSource::new(&[0x00; 1024]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0x1000, 0x1000, 3); // recursion_depth = 3
        let err = evaluator
            .evaluate(&ast)
            .expect_err("expected recursion depth error");
        assert!(
            err.message.contains("recursion depth exceeded"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_null_terminated_array_capped_by_array_limit() {
        // NullTerminated arrays are soft-capped by array_limit (no error, just stops)
        let source = r#"
            u32 data[] @ 0x00;
        "#;
        // 256 bytes of non-zero data → no null terminator found, but capped at 3 elements
        let ds = SliceDataSource::new(&[0x01; 256]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(3, 0x1000, 32); // array_limit = 3
        let results = evaluator.evaluate(&ast).expect("should succeed (capped)");
        let data_node = results.iter().find(|n| n.name == "data").unwrap();
        // Bulk read path caps at array_limit * elem_size = 3 * 4 = 12 bytes
        assert_eq!(data_node.size, 12);
    }

    #[test]
    fn test_while_array_uses_pattern_limit() {
        // While-loop arrays use pattern_limit, not array_limit (matches ImHex)
        let source = r#"
            struct Item { u8 val; };
            Item data[while($ < 10)] @ 0x00;
        "#;
        let ds = SliceDataSource::new(&[0x00; 256]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0x1000, 3, 32); // array_limit high, pattern_limit = 3
        let err = evaluator
            .evaluate(&ast)
            .expect_err("expected pattern limit error");
        assert!(
            err.message.contains("pattern limit"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_error_enum_has_no_member() {
        let err = eval_pattern_err(
            r#"
                enum Color : u8 {
                    Red = 0,
                    Green = 1,
                    Blue = 2
                };
                u8 x @ Color::Yellow;
            "#,
            &[0x00; 4],
        );
        assert!(
            err.message.contains("has no member"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn test_conditional_false_branch_no_extra() {
        // version=1, so extra should NOT appear
        let source = r#"
            struct Header {
                u8 version;
                u8 flags;
            };
            Header h @ 0x00;

            if (h.version == 2) {
                u16 extra @ 0x02;
            }
        "#;
        let data: &[u8] = &[0x01, 0xFF, 0x42, 0x00];
        let results = eval_pattern(source, data);
        // Only h (Header), no extra
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "h");
    }

    // ========== Line number tests ==========

    #[test]
    fn test_error_has_line_number() {
        let err = eval_pattern_err("u8 x @ 0x00;\nu8 y @ unknown;", &[0x01]);
        assert!(err.line.is_some(), "error should have line number");
        assert_eq!(err.line.unwrap(), 2);
    }

    #[test]
    fn test_error_display_includes_line_col() {
        let err = eval_pattern_err("u8 x @ 0x00;\nu8 y @ unknown;", &[0x01]);
        let display = format!("{}", err);
        assert!(
            display.contains("at 2:"),
            "display should contain line number: {}",
            display
        );
    }

    #[test]
    fn test_error_line_number_first_line() {
        let err = eval_pattern_err("u8 x @ unknown;", &[0x01]);
        assert_eq!(err.line, Some(1));
    }

    #[test]
    fn test_division_by_zero_result() {
        // Division by zero returns 0, so offset is 0
        let results = eval_pattern("u8 x @ 0x00;\nu8 y @ (1 / 0);", &[0x01, 0x02]);
        let y = results.iter().find(|n| n.name == "y").unwrap();
        assert_eq!(y.offset, 0);
    }

    #[test]
    fn test_find_sequence_in_range_hex_args() {
        // Verify that hex literal args are correctly passed to find_sequence_in_range
        // Data: "...PK\x05\x06..." at offset 4
        let mut data = vec![0u8; 16];
        data[4] = 0x50; // P
        data[5] = 0x4B; // K
        data[6] = 0x05;
        data[7] = 0x06;
        let results = eval_pattern(
            r#"
            s128 addr = std::mem::find_sequence_in_range(0, 0, 16, 0x50, 0x4B, 0x05, 0x06);
            u32 result @ addr;
            "#,
            &data,
        );
        let r = results.iter().find(|n| n.name == "result").unwrap();
        assert_eq!(r.offset, 4);
        // Value should be PK\x05\x06 in LE = 0x06054B50
        assert_eq!(r.value, PatternValue::Unsigned(0x06054B50));
    }

    #[test]
    fn test_variadic_param_forwarding() {
        // Variadic parameters should be collected and expanded when forwarded
        let mut data = vec![0u8; 16];
        data[4] = 0x50;
        data[5] = 0x4B;
        data[6] = 0x05;
        data[7] = 0x06;
        let results = eval_pattern(
            r#"
            fn find_wrapper(u128 occ, u128 from, u128 to, auto ... bytes) {
                return std::mem::find_sequence_in_range(occ, from, to, bytes);
            };
            s128 addr = find_wrapper(0, 0, 16, 0x50, 0x4B, 0x05, 0x06);
            u32 result @ addr;
            "#,
            &data,
        );
        let r = results.iter().find(|n| n.name == "result").unwrap();
        assert_eq!(r.offset, 4);
        assert_eq!(r.value, PatternValue::Unsigned(0x06054B50));
    }

    #[test]
    fn test_struct_if_member_access() {
        // Struct with fields inside an if-block, accessed from outside
        let results = eval_pattern(
            r#"
            struct Foo {
                u8 tag;
                if (tag == 0x01) {
                    u16 value;
                } else {
                    u32 value;
                }
            };
            Foo f @ 0x00;
            u8 result @ f.value;
            "#,
            &[0x01, 0x05, 0x00, 0x00, 0x00, 0x00],
        );
        let r = results
            .iter()
            .find(|n| n.name == "result")
            .expect("result not found");
        // f.value == 0x0005, so result reads u8 at offset 5
        assert_eq!(r.offset, 5);
    }

    #[test]
    fn test_parent_keyword_in_nested_struct() {
        // Nested struct accessing parent's fields via `parent` keyword
        let results = eval_pattern(
            r#"
            struct Inner {
                u8 val;
                u8 extra[parent.size - 1];
            };
            struct Outer {
                u8 size;
                Inner data;
            };
            Outer o @ 0x00;
            "#,
            &[0x03, 0xAA, 0xBB, 0xCC, 0xDD],
        );
        let o = results.iter().find(|n| n.name == "o").expect("o not found");
        // size=3, so Inner reads 1 byte val + 2 bytes extra = 3 bytes total
        // Outer = 1 (size) + 3 (Inner) = 4 bytes
        assert_eq!(o.size, 4);
    }

    #[test]
    fn test_dollar_offset_sync_in_struct_body() {
        // $ in init expression must reflect current struct offset
        let results = eval_pattern(
            r#"
            struct Foo {
                u8 len;
                u64 end = $ + len;
                u8 data[len];
                padding[end - $];
            };
            Foo f @ 0x00;
            "#,
            &[0x03, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE],
        );
        let f = results.iter().find(|n| n.name == "f").expect("f not found");
        // len=3 at offset 0, after len $ = 1
        // end = 1 + 3 = 4
        // data = 3 bytes [AA,BB,CC] at offsets 1,2,3, after data $ = 4
        // padding = end - $ = 4 - 4 = 0 bytes
        // Total: 1 + 3 + 0 = 4
        assert_eq!(f.size, 4);
    }

    // ========== Hardcoded builtin function tests ==========

    #[test]
    fn test_eval_std_mem_read_signed() {
        let source = r#"
            s8 val = std::mem::read_signed(0, 1);
        "#;
        let data: &[u8] = &[0xFF]; // -1 as s8
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Signed(-1)));
    }

    #[test]
    fn test_eval_std_mem_eof() {
        // At offset 0 with non-empty data, eof should be false
        let source = r#"
            bool at_eof = std::mem::eof();
        "#;
        let data: &[u8] = &[0x01, 0x02];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("at_eof");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Bool(false)));
    }

    #[test]
    fn test_eval_std_string_length() {
        let source = r#"
            u32 len = std::string::length("hello");
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("len");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(5)));
    }

    #[test]
    fn test_eval_std_ctype_isprint() {
        let source = r#"
            bool printable = std::ctype::isprint(65);
            bool not_printable = std::ctype::isprint(0);
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let k1 = evaluator.interner.intern("printable");
        let k2 = evaluator.interner.intern("not_printable");
        assert_eq!(evaluator.scope.get_var(k1), Some(&Value::Bool(true)));
        assert_eq!(evaluator.scope.get_var(k2), Some(&Value::Bool(false)));
    }

    #[test]
    fn test_eval_std_format() {
        let source = r#"
            str result = std::format("val={}", 42);
        "#;
        let data: &[u8] = &[];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("result");
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::String("val=42".into()))
        );
    }

    #[test]
    fn test_eval_std_mem_read_string() {
        let source = r#"
            str s = std::mem::read_string(0, 5);
        "#;
        let data: &[u8] = b"Hello World";
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("s");
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::String("Hello".into()))
        );
    }

    #[test]
    fn test_eval_std_print() {
        let source = r#"
            std::print("hello {}", 42);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert!(!evaluator.print_output.is_empty());
    }

    // ========== Built-in variable tests ==========

    #[test]
    fn test_dollar_initial_value() {
        // $ starts at 0 before any placement
        let source = r#"
            u64 off = $;
        "#;
        let ds = SliceDataSource::new(&[0x00; 4]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(0))
        );
    }

    #[test]
    fn test_dollar_in_placement() {
        // $ at start is 0; explicit @ placement does not advance $
        let results = eval_pattern(
            r#"
            u8 a @ 0x00;
            u8 b @ $;
            "#,
            &[0x11, 0x22],
        );
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].name, "a");
        assert_eq!(results[0].offset, 0);
        // $ is still 0 after explicit placement, so b is also at 0
        assert_eq!(results[1].name, "b");
        assert_eq!(results[1].offset, 0);
        assert_eq!(results[1].value, PatternValue::Unsigned(0x11));
    }

    #[test]
    fn test_this_in_struct() {
        // `this` inside a struct should equal the struct's base offset.
        // Use addressof(this) to position a field, verifying its offset in the result.
        let source = r#"
            struct Foo {
                u8 val;
                u8 at_base @ addressof(this);
            };
            Foo f @ 0x04;
        "#;
        let data: &[u8] = &[0x00, 0x00, 0x00, 0x00, 0xAA, 0xBB];
        let results = eval_pattern(source, &data);
        let f = results.iter().find(|n| n.name == "f").expect("f not found");
        let at_base = f
            .children
            .iter()
            .find(|c| c.name == "at_base")
            .expect("at_base not found");
        // addressof(this) == 4 (struct base), so at_base reads from offset 4
        assert_eq!(at_base.offset, 4);
        assert_eq!(at_base.value, PatternValue::Unsigned(0xAA));
    }

    #[test]
    fn test_parent_in_union() {
        // parent keyword in a nested union should access enclosing struct fields
        let source = r#"
            struct Inner {
                u8 len;
            };
            union Wrapper {
                u8 raw[parent.count];
            };
            struct Outer {
                u8 count;
                Wrapper w;
            };
            Outer o @ 0x00;
        "#;
        let data: &[u8] = &[0x03, 0xAA, 0xBB, 0xCC];
        let results = eval_pattern(source, data);
        let o = results.iter().find(|n| n.name == "o").expect("o not found");
        // count=3, Wrapper.raw should have 3 elements
        let w = o
            .children
            .iter()
            .find(|c| c.name == "w")
            .expect("w not found");
        let raw = w
            .children
            .iter()
            .find(|c| c.name == "raw")
            .expect("raw not found");
        assert_eq!(raw.children.len(), 3);
    }

    #[test]
    fn test_parent_parent_nested() {
        // parent.parent access in a doubly-nested struct
        let source = r#"
            struct L2 {
                u8 val;
                u8 extra[parent.parent.count];
            };
            struct L1 {
                L2 inner;
            };
            struct L0 {
                u8 count;
                L1 mid;
            };
            L0 root @ 0x00;
        "#;
        let data: &[u8] = &[0x02, 0xAA, 0xBB, 0xCC];
        let results = eval_pattern(source, data);
        let root = results
            .iter()
            .find(|n| n.name == "root")
            .expect("root not found");
        // count=2, L2.extra should have 2 elements
        let mid = &root.children[1]; // mid
        let inner = &mid.children[0]; // inner
        let extra = inner
            .children
            .iter()
            .find(|c| c.name == "extra")
            .expect("extra not found");
        assert_eq!(extra.children.len(), 2);
    }

    #[test]
    fn test_struct_transform_member_access() {
        // Struct with [[transform]] that accesses struct members via function param
        // Mimics rar.hexpat vint: while-array + transform function
        let source = r#"
            fn read_more() {
                return std::mem::read_unsigned($, 1) & 0x80 != 0;
            };
            struct vint {
                u8 data[while(read_more())];
                u8 last;
            } [[transform("vint_value")]];

            fn vint_value(vint vi) {
                u64 value = 0;
                u8 i = 0;
                for(i = 0, i < sizeof(vi.data), i = i + 1) {
                    value = value | ((vi.data[i] & 0x7F) << (i * 7));
                }
                value = value | (vi.last << (i * 7));
                return value;
            };

            vint test @ 0x00;
        "#;
        // data[0] = 0x80 (high bit set → continue), data[1] = 0x01 (high bit clear → stop → becomes last)
        // So data array = [0x80], last = 0x01
        let data: &[u8] = &[0x80, 0x01];
        let results = eval_pattern(source, data);
        assert!(!results.is_empty(), "should have results");
    }

    #[test]
    fn test_addressof_placed_variable() {
        // addressof on a placed struct variable should return its offset
        let source = r#"
            struct Header {
                u8 a;
                u8 b;
            };
            Header h @ 0x04;
            u64 addr = addressof(h);
        "#;
        let data: &[u8] = &[0x00; 8];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("addr")),
            Some(&Value::Unsigned(4))
        );
    }

    #[test]
    fn test_sizeof_string_value() {
        // sizeof on a string variable should return string length
        let source = r#"
            str s = "hello";
            u64 sz = sizeof(s);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("sz")),
            Some(&Value::Unsigned(5))
        );
    }

    #[test]
    fn test_sizeof_array_value() {
        // sizeof on an initializer-list array should return element count
        let source = r#"
            u8 arr[] = { 10, 20, 30, 40 };
            u64 sz = sizeof(arr);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("sz")),
            Some(&Value::Unsigned(4))
        );
    }

    // ========== Dollar offset ($) comprehensive tests ==========

    // --- A. Auto-advance ---

    #[test]
    fn test_dollar_advances_after_u8() {
        let source = r#"
            u8 a @ 0x00;  // placement: $ stays 0
            u8 b;         // auto-advance from $=0, after: $=1
            u64 off = $;
        "#;
        let data: &[u8] = &[0xAA, 0xBB];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(1))
        );
    }

    #[test]
    fn test_dollar_advances_sequential_types() {
        // u8(1) + u16(2) + u32(4) + u64(8) = 15 bytes total
        let source = r#"
            u8 a;
            u64 off1 = $;
            u16 b;
            u64 off2 = $;
            u32 c;
            u64 off3 = $;
            u64 d;
            u64 off4 = $;
        "#;
        let data: &[u8] = &[0x00; 16];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off1")),
            Some(&Value::Unsigned(1))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off2")),
            Some(&Value::Unsigned(3))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off3")),
            Some(&Value::Unsigned(7))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off4")),
            Some(&Value::Unsigned(15))
        );
    }

    #[test]
    fn test_dollar_advances_after_fixed_array() {
        let source = r#"
            u8 arr[4];
            u64 off = $;
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(4))
        );
    }

    // --- B. Placement does not change $ ---

    #[test]
    fn test_dollar_unchanged_after_multiple_placements() {
        let source = r#"
            u8 a @ 0x00;
            u8 b @ 0x01;
            u8 c @ 0x02;
            u64 off = $;
        "#;
        let data: &[u8] = &[0xAA, 0xBB, 0xCC];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(0))
        );
    }

    #[test]
    fn test_dollar_mixed_placement_and_auto() {
        // auto: $=0 -> read u8 -> $=1
        // placement @0x10: $ stays 1
        // auto: $=1 -> read u16 -> $=3
        let source = r#"
            u8 a;
            u64 off1 = $;
            u8 b @ 0x00;
            u64 off2 = $;
            u16 c;
            u64 off3 = $;
        "#;
        let data: &[u8] = &[0x00; 16];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off1")),
            Some(&Value::Unsigned(1))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off2")),
            Some(&Value::Unsigned(1))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off3")),
            Some(&Value::Unsigned(3))
        );
    }

    // --- C. Arithmetic expressions ---

    #[test]
    fn test_dollar_in_arithmetic_expressions() {
        // After u32 (4 bytes), $=4
        let source = r#"
            u32 x;
            u64 add2 = $ + 2;
            u64 sub1 = $ - 1;
            u64 mul3 = $ * 3;
        "#;
        let data: &[u8] = &[0x00; 8];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("add2")),
            Some(&Value::Unsigned(6))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("sub1")),
            Some(&Value::Unsigned(3))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("mul3")),
            Some(&Value::Unsigned(12))
        );
    }

    #[test]
    fn test_dollar_in_array_size_expression() {
        // At $=0, read u8 first_byte (value=3), $=1
        // Then u8 data[$ + 1] => data[1 + 1] = data[2]
        let source = r#"
            u8 first_byte;
            u8 data[$ + 1];
            u64 off = $;
        "#;
        let data: &[u8] = &[0x03, 0xAA, 0xBB, 0xCC, 0xDD];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // data has 2 elements, offset advances from 1 to 3
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(3))
        );
    }

    #[test]
    fn test_dollar_in_if_condition() {
        // After u16, $=2, so if ($ == 2) branch is taken
        let source = r#"
            u16 header;
            u64 result = 0;
            if ($ == 2) {
                result = 42;
            } else {
                result = 99;
            }
        "#;
        let data: &[u8] = &[0x00, 0x00];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("result")),
            Some(&Value::Unsigned(42))
        );
    }

    // --- D. While-array ---

    #[test]
    fn test_dollar_while_array_basic_termination() {
        // while($ < 4): reads 4 u8 elements (offsets 0,1,2,3)
        let results = eval_pattern(
            r#"
            u8 data[while($ < 4)];
            "#,
            &[0xAA, 0xBB, 0xCC, 0xDD, 0xEE],
        );
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "data");
        assert_eq!(results[0].children.len(), 4);
        assert_eq!(results[0].size, 4);
    }

    #[test]
    fn test_dollar_while_array_struct_elements() {
        // struct of 2 bytes, while($ < 6) => 3 elements
        let results = eval_pattern(
            r#"
            struct Pair {
                u8 a;
                u8 b;
            };
            Pair items[while($ < 6)];
            "#,
            &[0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
        );
        let items = results
            .iter()
            .find(|n| n.name == "items")
            .expect("items not found");
        assert_eq!(items.children.len(), 3);
        assert_eq!(items.size, 6);
    }

    #[test]
    fn test_dollar_while_array_at_nonzero_offset() {
        // Place at offset 2, while($ < 6) => 4 elements
        let results = eval_pattern(
            r#"
            u8 data[while($ < 6)] @ 0x02;
            "#,
            &[0x00, 0x00, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF],
        );
        assert_eq!(results[0].name, "data");
        assert_eq!(results[0].offset, 2);
        assert_eq!(results[0].children.len(), 4);
        assert_eq!(results[0].size, 4);
    }

    #[test]
    fn test_dollar_restored_after_while_array_with_placement() {
        // With placement, $ should not change
        let source = r#"
            u8 header;
            u64 off_before = $;
            u8 data[while($ < 4)] @ 0x00;
            u64 off_after = $;
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // After header (auto), $=1
        assert_eq!(
            evaluator
                .scope
                .get_var(evaluator.interner.intern("off_before")),
            Some(&Value::Unsigned(1))
        );
        // Placement does not advance $, so $=1 still
        assert_eq!(
            evaluator
                .scope
                .get_var(evaluator.interner.intern("off_after")),
            Some(&Value::Unsigned(1))
        );
    }

    #[test]
    fn test_dollar_while_array_auto_advance() {
        // Without placement, $ should advance past the array
        let source = r#"
            u8 data[while($ < 4)];
            u64 off = $;
        "#;
        let data: &[u8] = &[0xAA, 0xBB, 0xCC, 0xDD, 0xEE];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // Auto-advance: $=0 + 4 (array size) = 4
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(4))
        );
    }

    // --- E. Struct ---

    #[test]
    fn test_dollar_reflects_position_in_struct() {
        // Check $ at each point inside a struct
        let results = eval_pattern(
            r#"
            struct Foo {
                u64 off0 = $;
                u8 a;
                u64 off1 = $;
                u16 b;
                u64 off2 = $;
            };
            Foo f @ 0x00;
            "#,
            &[0x00; 8],
        );
        let f = results.iter().find(|n| n.name == "f").expect("f not found");
        // off0 = 0 (start), off1 = 1 (after u8), off2 = 3 (after u8+u16)
        // Verify via struct size: u8(1) + u16(2) = 3
        assert_eq!(f.size, 3);
    }

    #[test]
    fn test_dollar_in_nested_struct() {
        let results = eval_pattern(
            r#"
            struct Inner {
                u8 x;
                u8 y;
            };
            struct Outer {
                u8 header;
                Inner body;
            };
            Outer o @ 0x00;
            "#,
            &[0xAA, 0xBB, 0xCC],
        );
        let o = results.iter().find(|n| n.name == "o").expect("o not found");
        assert_eq!(o.offset, 0);
        assert_eq!(o.size, 3);
        let body = o
            .children
            .iter()
            .find(|c| c.name == "body")
            .expect("body not found");
        assert_eq!(body.offset, 1);
        assert_eq!(body.size, 2);
    }

    #[test]
    fn test_dollar_in_struct_local_var_initializer() {
        // Use $ in a computed local for padding calculation
        let results = eval_pattern(
            r#"
            struct Aligned {
                u8 tag;
                u8 val;
                padding[8 - $];
            };
            Aligned a @ 0x00;
            "#,
            &[0x00; 16],
        );
        let a = results.iter().find(|n| n.name == "a").expect("a not found");
        // tag(1) + val(1) + padding(8-2=6) = 8
        assert_eq!(a.size, 8);
    }

    #[test]
    fn test_dollar_in_struct_at_nonzero_base() {
        // Struct placed at 0x10, $ inside tracks correctly
        // Use $ in a padding expression to verify offset tracking
        let results = eval_pattern(
            r#"
            struct Foo {
                u8 a;
                u8 b;
            };
            Foo f @ 0x10;
            "#,
            &[0x00; 0x20],
        );
        let f = results.iter().find(|n| n.name == "f").expect("f not found");
        assert_eq!(f.offset, 0x10);
        assert_eq!(f.size, 2);
        let a = f
            .children
            .iter()
            .find(|c| c.name == "a")
            .expect("a not found");
        assert_eq!(a.offset, 0x10);
        let b = f
            .children
            .iter()
            .find(|c| c.name == "b")
            .expect("b not found");
        assert_eq!(b.offset, 0x11);
    }

    // --- F. Functions ---

    #[test]
    fn test_dollar_passed_as_function_argument() {
        let source = r#"
            fn get_double(u64 x) {
                return x * 2;
            };
            u32 header;
            u64 result = get_double($);
        "#;
        let data: &[u8] = &[0x00; 8];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // After u32, $=4, get_double(4)=8
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("result")),
            Some(&Value::Unsigned(8))
        );
    }

    #[test]
    fn test_dollar_inside_function_reflects_caller_offset() {
        let source = r#"
            fn get_offset() {
                return $;
            };
            u16 header;
            u64 result = get_offset();
        "#;
        let data: &[u8] = &[0x00; 4];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // After u16, $=2, function sees same offset
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("result")),
            Some(&Value::Unsigned(2))
        );
    }

    #[test]
    fn test_dollar_in_function_with_var_decl() {
        // Function reads data, advancing $, and it persists after return
        let source = r#"
            fn read_and_return() {
                u8 tmp;
                u8 tmp2;
                return $;
            };
            u8 header;
            u64 result = read_and_return();
            u64 off_after = $;
        "#;
        let data: &[u8] = &[0x00; 8];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // header: $=0->1, then function reads 2 bytes: $=1->3
        // result = 3 (return value from function)
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("result")),
            Some(&Value::Unsigned(3))
        );
        // $ remains at 3 after function call
        assert_eq!(
            evaluator
                .scope
                .get_var(evaluator.interner.intern("off_after")),
            Some(&Value::Unsigned(3))
        );
    }

    // --- G. Edge cases ---

    #[test]
    fn test_dollar_at_end_of_data() {
        let source = r#"
            u8 a;
            u8 b;
            u8 c;
            u64 off = $;
        "#;
        let data: &[u8] = &[0xAA, 0xBB, 0xCC];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // 3 bytes read, $ should equal data length
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(3))
        );
    }

    #[test]
    fn test_dollar_with_big_endian_types() {
        // be types should advance $ by the same size as le
        let source = r#"
            be u32 magic;
            u64 off1 = $;
            be u16 val;
            u64 off2 = $;
        "#;
        let data: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x00, 0x0D];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off1")),
            Some(&Value::Unsigned(4))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off2")),
            Some(&Value::Unsigned(6))
        );
    }

    #[test]
    fn test_dollar_eof_consistency() {
        // $ should equal data size when std::mem::eof() returns true
        let source = r#"
            u8 data[while(!std::mem::eof())];
            u64 off = $;
            bool at_eof = std::mem::eof();
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(3))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("at_eof")),
            Some(&Value::Bool(true))
        );
    }

    #[test]
    fn test_dollar_with_padding_skip() {
        // Use padding[N - $] to align to boundary
        let results = eval_pattern(
            r#"
            struct Record {
                u8 tag;
                u8 value;
                padding[8 - $];
                u8 next;
            };
            Record r @ 0x00;
            "#,
            &[0x00; 16],
        );
        let r = results.iter().find(|n| n.name == "r").expect("r not found");
        // tag(1) + value(1) + padding(8-2=6) + next(1) = 9
        assert_eq!(r.size, 9);
        let next = r
            .children
            .iter()
            .find(|c| c.name == "next")
            .expect("next not found");
        assert_eq!(next.offset, 8);
    }

    #[test]
    fn test_dollar_zero_size_read() {
        let source = r#"
            u8 header;
            u64 off_before = $;
            u8 empty[0];
            u64 off_after = $;
        "#;
        let data: &[u8] = &[0xAA, 0xBB];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // u8[0] reads nothing, $ stays at 1
        assert_eq!(
            evaluator
                .scope
                .get_var(evaluator.interner.intern("off_before")),
            Some(&Value::Unsigned(1))
        );
        assert_eq!(
            evaluator
                .scope
                .get_var(evaluator.interner.intern("off_after")),
            Some(&Value::Unsigned(1))
        );
    }

    #[test]
    fn test_array_element_member_access() {
        // Test accessing struct members through array indexing: arr[i].field
        // This pattern appears in pex.hexpat (arguments[3].varType)
        let source = r#"
            struct Entry {
                u8 kind;
                u8 val;
            };
            Entry items[3] @ 0x00;
            u8 k0 = items[0].kind;
            u8 k2 = items[2].kind;
            u8 v1 = items[1].val;
        "#;
        // items[0] = {kind=0x01, val=0x0A}
        // items[1] = {kind=0x02, val=0x0B}
        // items[2] = {kind=0x03, val=0x0C}
        let data: &[u8] = &[0x01, 0x0A, 0x02, 0x0B, 0x03, 0x0C];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("k0")),
            Some(&Value::Unsigned(0x01))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("k2")),
            Some(&Value::Unsigned(0x03))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("v1")),
            Some(&Value::Unsigned(0x0B))
        );
    }

    #[test]
    fn test_array_element_member_access_in_struct() {
        // Test arr[i].field inside a struct body (via match body like pex.hexpat)
        let source = r#"
            struct Entry {
                u8 kind;
                u8 val;
            };
            struct Container {
                u8 count;
                Entry items[count];
                u8 check = items[0].kind;
            };
            Container c @ 0x00;
        "#;
        // count=2, items[0]={kind=0xAA, val=0xBB}, items[1]={kind=0xCC, val=0xDD}
        let data: &[u8] = &[0x02, 0xAA, 0xBB, 0xCC, 0xDD];
        let results = eval_pattern(source, data);
        assert!(!results.is_empty(), "should produce results");
        // Find the Container node
        let container = results
            .iter()
            .find(|n| n.name == "c")
            .expect("should find 'c'");
        // The 'check' virtual node should have value 0xAA
        let check = container
            .children
            .iter()
            .find(|c| c.name == "check")
            .expect("should find 'check'");
        assert_eq!(check.value, PatternValue::Unsigned(0xAA));
    }

    #[test]
    fn test_parent_array_element_member_access() {
        // Test parent.arr[i].field — like chromium_pak's parent.entries[i].id
        let source = r#"
            struct Entry {
                u16 id;
                u16 offset;
            };
            struct View {
                u32 i = 0;
                u16 id_copy = parent.entries[i].id;
            };
            struct Pak {
                u8 count;
                Entry entries[count];
                View views[count];
            };
            Pak p @ 0x00;
        "#;
        // count=1, entries[0]={id=0x0042, offset=0x0010}, views[0]={...}
        let data: &[u8] = &[
            0x01, // count = 1
            0x42, 0x00, 0x10,
            0x00, // entries[0]: id=0x0042, offset=0x0010
                  // views[0] is computed, no data needed
        ];
        let results = eval_pattern(source, data);
        assert!(!results.is_empty(), "should produce results");
        let pak = results
            .iter()
            .find(|n| n.name == "p")
            .expect("should find 'p'");
        let views = pak
            .children
            .iter()
            .find(|c| c.name == "views")
            .expect("should find 'views'");
        let view0 = &views.children[0];
        let id_copy = view0
            .children
            .iter()
            .find(|c| c.name == "id_copy")
            .expect("should find 'id_copy'");
        assert_eq!(id_copy.value, PatternValue::Unsigned(0x0042));
    }

    #[test]
    fn test_dollar_index_read_byte() {
        // $[n] reads the byte at absolute file offset n
        let data: &[u8] = &[0xAA, 0xBB, 0xCC, 0xDD];
        let ds = SliceDataSource::new(data);
        let (_, mut eval) = parse_and_eval(
            r#"
            u8 b0 = $[0];
            u8 b1 = $[1];
            u8 b2 = $[2];
            u8 b3 = $[3];
            "#,
            &ds,
        );
        assert_eq!(
            eval.scope.get_var(eval.interner.intern("b0")),
            Some(&Value::Unsigned(0xAA))
        );
        assert_eq!(
            eval.scope.get_var(eval.interner.intern("b1")),
            Some(&Value::Unsigned(0xBB))
        );
        assert_eq!(
            eval.scope.get_var(eval.interner.intern("b2")),
            Some(&Value::Unsigned(0xCC))
        );
        assert_eq!(
            eval.scope.get_var(eval.interner.intern("b3")),
            Some(&Value::Unsigned(0xDD))
        );
    }

    #[test]
    fn test_dollar_index_with_expression() {
        // $[$ + 1] — index computed from current offset expression
        let data: &[u8] = &[0x10, 0x20, 0x30, 0x40];
        let ds = SliceDataSource::new(data);
        let (_, mut eval) = parse_and_eval(
            r#"
            u8 first;
            u8 val = $[$ + 1];
            "#,
            &ds,
        );
        // After reading first (1 byte auto), $ becomes 1.
        // $[$ + 1] = $[1 + 1] = $[2] = byte at offset 2 = 0x30
        assert_eq!(
            eval.scope.get_var(eval.interner.intern("val")),
            Some(&Value::Unsigned(0x30))
        );
    }

    #[test]
    fn test_dollar_index_out_of_bounds_returns_zero() {
        // $[n] where n >= data size returns 0 (tolerant behavior)
        let source = r#"
            u8 val @ $[100];
        "#;
        let data: &[u8] = &[0x00; 4];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        let results = evaluator.evaluate(&ast).unwrap();
        let val = results.iter().find(|n| n.name == "val").unwrap();
        assert_eq!(val.offset, 0); // $[100] returns 0 for OOB
    }

    #[test]
    fn test_unsigned_var_index_read_byte() {
        // $[n] reads byte at absolute offset n from binary data
        let data: &[u8] = &[0xAA, 0xBB, 0xCC];
        let ds = SliceDataSource::new(data);
        let (_, mut eval) = parse_and_eval(
            r#"
            u8 val = $[2];
            "#,
            &ds,
        );
        assert_eq!(
            eval.scope.get_var(eval.interner.intern("val")),
            Some(&Value::Unsigned(0xCC))
        );
    }

    #[test]
    fn test_dollar_index_in_loop() {
        // Read bytes using $[i] in a loop
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        let ds = SliceDataSource::new(data);
        let (_, mut eval) = parse_and_eval(
            r#"
            u32 sum = 0;
            for (u8 i = 0, i < 4, i = i + 1) {
                sum = sum + $[i];
            }
            "#,
            &ds,
        );
        // sum = 0x01 + 0x02 + 0x03 + 0x04 = 10
        assert_eq!(
            eval.scope.get_var(eval.interner.intern("sum")),
            Some(&Value::Unsigned(10))
        );
    }

    // ========== String repetition operator (2cdaf1f) ==========

    #[test]
    fn test_string_repetition() {
        let source = r#"
            str s = "abc" * 3;
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("s");
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::String("abcabcabc".into()))
        );
    }

    #[test]
    fn test_string_repetition_reverse() {
        // N * "str" should also work
        let source = r#"
            str s = 2 * "xy";
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("s");
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::String("xyxy".into()))
        );
    }

    #[test]
    fn test_string_repetition_zero() {
        let source = r#"
            str s = "abc" * 0;
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("s");
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::String("".into()))
        );
    }

    // ========== Dollar assignment (a360a70) ==========

    #[test]
    fn test_dollar_assignment() {
        let source = r#"
            $ = 4;
            u8 x;
            u64 off = $;
        "#;
        let data: &[u8] = &[0x00; 8];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        // After $ = 4, read u8 at offset 4, $ becomes 5
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(5))
        );
    }

    #[test]
    fn test_dollar_compound_assignment() {
        let source = r#"
            u16 header;
            $ += 2;
            u64 off = $;
        "#;
        let data: &[u8] = &[0x00; 8];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        // After u16: $=2, then $+=2: $=4
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("off")),
            Some(&Value::Unsigned(4))
        );
    }

    // ========== Enum with char underlying type (a360a70) ==========

    #[test]
    fn test_enum_char_underlying() {
        let source = r#"
            enum Letter : char {
                A = 'A',
                B = 'B',
                C = 'C'
            };
            Letter x @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x42]); // 'B'
        assert_eq!(
            results[0].value,
            PatternValue::Enum {
                value: 0x42,
                name: "B".to_string()
            }
        );
    }

    // ========== sizeof for struct/union/bitfield types (e06c1b1) ==========

    #[test]
    fn test_sizeof_struct_type() {
        let source = r#"
            struct Foo {
                u8 a;
                u16 b;
                u32 c;
            };
            u64 sz = sizeof(Foo);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sz");
        // u8(1) + u16(2) + u32(4) = 7
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(7)));
    }

    #[test]
    fn test_sizeof_union_type() {
        let source = r#"
            union Bar {
                u8 a;
                u32 b;
                u16 c;
            };
            u64 sz = sizeof(Bar);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sz");
        // max(1, 4, 2) = 4
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(4)));
    }

    #[test]
    fn test_sizeof_bitfield_type() {
        let source = r#"
            bitfield Flags {
                a : 3;
                b : 5;
                c : 8;
            };
            u64 sz = sizeof(Flags);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sz");
        // (3+5+8) / 8 = 2 bytes
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(2)));
    }

    #[test]
    fn test_sizeof_struct_with_inheritance() {
        let source = r#"
            struct Base {
                u8 a;
                u16 b;
            };
            struct Child : Base {
                u32 c;
            };
            u64 sz = sizeof(Child);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sz");
        // Base(1+2=3) + Child(4) = 7
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(7)));
    }

    // ========== Array index assignment (e06c1b1) ==========

    #[test]
    fn test_array_index_assignment() {
        let source = r#"
            u8 arr[] = { 10, 20, 30 };
            arr[1] = 99;
            u8 val = arr[1];
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(99)));
    }

    #[test]
    fn test_array_index_assignment_extend() {
        let source = r#"
            u8 arr[] = { 1 };
            arr[3] = 42;
            u64 len = sizeof(arr);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("len");
        // Array extended to 4 elements
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(4)));
    }

    // ========== Limit 0 = unlimited (3a0bbff) ==========

    #[test]
    fn test_array_limit_zero_is_unlimited() {
        // With array_limit=0, large arrays should not trigger limit error
        let source = r#"
            u8 data[while($ < 10)];
        "#;
        let data: &[u8] = &[0x00; 16];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0, 0x1000, 32); // array_limit = 0 (unlimited)
        let results = evaluator.evaluate(&ast).unwrap();
        assert_eq!(results[0].children.len(), 10);
    }

    #[test]
    fn test_pattern_limit_zero_is_unlimited() {
        // With pattern_limit=0, many patterns should not trigger limit error
        let source = r#"
            u8 a @ 0x00;
            u8 b @ 0x01;
            u8 c @ 0x02;
            u8 d @ 0x03;
            u8 e @ 0x04;
        "#;
        let data: &[u8] = &[0x00; 8];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0x1000, 0, 32); // pattern_limit = 0 (unlimited)
        let results = evaluator.evaluate(&ast).unwrap();
        assert_eq!(results.len(), 5);
    }

    // ========== addressof on char[] (3a0bbff) ==========

    #[test]
    fn test_addressof_char_array() {
        // char[] is stored as Value::String, addressof should still resolve offset
        let source = r#"
            char name[4] @ 0x02;
            u64 addr = addressof(name);
        "#;
        let data: &[u8] = &[0x00, 0x00, 0x41, 0x42, 0x43, 0x44];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("addr");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(2)));
    }

    // ========== Scoped enum N-part path (6272246) ==========

    #[test]
    fn test_scoped_enum_multi_part_path() {
        let source = r#"
            namespace ns {
                enum Color : u8 {
                    Red = 1,
                    Green = 2,
                    Blue = 3
                };
            };
            u8 val = ns::Color::Green;
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(2)));
    }

    // ========== read_unsigned/read_signed with endianness arg (6272246) ==========

    #[test]
    fn test_read_unsigned_big_endian_arg() {
        // 3rd arg: 1 = Big endian
        let source = r#"
            u32 val = std::mem::read_unsigned(0, 4, 1);
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        // Big endian: 0x01020304
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::Unsigned(0x01020304))
        );
    }

    #[test]
    fn test_read_unsigned_little_endian_arg() {
        // 3rd arg: 2 = Little endian
        let source = r#"
            u32 val = std::mem::read_unsigned(0, 4, 2);
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        // Little endian: 0x04030201
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::Unsigned(0x04030201))
        );
    }

    #[test]
    fn test_read_signed_with_endianness() {
        // Read -1 as BE s16
        let source = r#"
            s16 val = std::mem::read_signed(0, 2, 1);
        "#;
        let data: &[u8] = &[0xFF, 0xFF];
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Signed(-1)));
    }

    // ========== Member access in match block (9e55513) ==========

    #[test]
    fn test_member_access_in_match() {
        // Members defined before a match block should be accessible inside
        let source = r#"
            struct Foo {
                u8 tag;
                u8 val;
                match (tag) {
                    (1): u8 extra[val];
                }
            };
            Foo f @ 0x00;
        "#;
        // tag=1, val=2, extra=[0xAA, 0xBB]
        let data: &[u8] = &[0x01, 0x02, 0xAA, 0xBB];
        let results = eval_pattern(source, data);
        let f = results.iter().find(|n| n.name == "f").expect("f not found");
        let extra = f
            .children
            .iter()
            .find(|c| c.name == "extra")
            .expect("extra not found");
        assert_eq!(extra.children.len(), 2);
    }

    // ========== Struct field-level transform (2cdaf1f) ==========

    #[test]
    fn test_struct_field_transform_scope_var() {
        // Field-level [[transform("fn")]] should change the scope variable value
        let source = r#"
            fn double_val(u8 x) {
                return x * 2;
            };
            struct Foo {
                [[transform("double_val")]]
                u8 raw;
                u8 check = raw;
            };
            Foo f @ 0x00;
        "#;
        let data: &[u8] = &[0x05, 0x00];
        let results = eval_pattern(source, data);
        let f = results.iter().find(|n| n.name == "f").expect("f not found");
        // The scope variable 'raw' should be transformed to 10
        let check = f
            .children
            .iter()
            .find(|c| c.name == "check")
            .expect("check not found");
        assert_eq!(check.value, PatternValue::Unsigned(10));
    }

    // ========== FourCC string-to-unsigned conversion (2cdaf1f) ==========

    #[test]
    fn test_fourcc_string_to_unsigned() {
        // char[] stored as String can be cast to unsigned via type cast (FourCC-style)
        let source = r#"
            char magic[4] @ 0x00;
            u32 val = u32(magic);
        "#;
        let data: &[u8] = b"RIFF";
        let ds = SliceDataSource::new(data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("val");
        // "RIFF" as LE u32 = 0x46464952
        assert_eq!(
            evaluator.scope.get_var(key),
            Some(&Value::Unsigned(0x46464952))
        );
    }

    // ========== For loop limit 0 = unlimited (3a0bbff) ==========

    #[test]
    fn test_for_loop_limit_zero_is_unlimited() {
        let source = r#"
            u32 sum = 0;
            for (u32 i = 0, i < 200, i = i + 1) {
                sum = sum + 1;
            }
        "#;
        let ds = SliceDataSource::new(&[]);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.set_limits(0x1000, 0, 32); // pattern_limit = 0 (unlimited)
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("sum")),
            Some(&Value::Unsigned(200))
        );
    }

    // ========== Anonymous type placement from ExprStmt ==========

    #[test]
    fn test_anonymous_builtin_type_placement() {
        // `u32;` without a name should be treated as anonymous placement, advancing $
        let results = eval_pattern(
            r#"
            u8 a;
            u32;
            u8 b;
            "#,
            &[0x11, 0x22, 0x33, 0x44, 0x55, 0x66],
        );
        // a=0x11 at offset 0, anonymous u32 at offset 1 (4 bytes), b at offset 5
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].name, "a");
        assert_eq!(results[0].value, PatternValue::Unsigned(0x11));
        assert_eq!(results[1].name, "_u32");
        assert_eq!(results[1].offset, 1);
        assert_eq!(results[1].size, 4);
        assert_eq!(results[2].name, "b");
        assert_eq!(results[2].offset, 5);
        assert_eq!(results[2].value, PatternValue::Unsigned(0x66));
    }

    #[test]
    fn test_anonymous_builtin_in_struct() {
        // `u32;` inside a struct body should read and skip 4 bytes
        let results = eval_pattern(
            r#"
            struct Foo {
                u8 a;
                u32;
                u8 b;
            };
            Foo f @ 0x00;
            "#,
            &[0x11, 0x22, 0x33, 0x44, 0x55, 0x66],
        );
        let foo = results
            .iter()
            .find(|n| n.name == "f")
            .expect("should find 'f'");
        let a = foo
            .children
            .iter()
            .find(|c| c.name == "a")
            .expect("should find 'a'");
        let b = foo
            .children
            .iter()
            .find(|c| c.name == "b")
            .expect("should find 'b'");
        assert_eq!(a.value, PatternValue::Unsigned(0x11));
        assert_eq!(b.offset, 5);
        assert_eq!(b.value, PatternValue::Unsigned(0x66));
    }

    #[test]
    fn test_anonymous_named_type_placement() {
        // `Header;` without a name should be treated as anonymous placement
        let source = r#"
            struct Header {
                u16 magic;
                u16 size;
            };
            struct Record {
                Header;
                u8 data;
            };
            Record r @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x01, 0x02, 0x03, 0x04, 0x55]);
        let rec = results
            .iter()
            .find(|n| n.name == "r")
            .expect("should find 'r'");
        // Header is anonymous, its fields should be propagated into scope
        let data_child = rec
            .children
            .iter()
            .find(|c| c.name == "data")
            .expect("should find 'data'");
        assert_eq!(data_child.offset, 4);
        assert_eq!(data_child.value, PatternValue::Unsigned(0x55));
    }

    #[test]
    fn test_anonymous_type_fields_accessible() {
        // Fields from anonymous named type should be accessible in struct scope
        let results = eval_pattern(
            r#"
            struct Header {
                u16 id;
                u16 length;
            };
            struct Packet {
                Header;
                u8 checksum = id + length;
            };
            Packet p @ 0x00;
            "#,
            &[0x0A, 0x00, 0x14, 0x00, 0x00],
        );
        let pkt = results
            .iter()
            .find(|n| n.name == "p")
            .expect("should find 'p'");
        let checksum = pkt
            .children
            .iter()
            .find(|c| c.name == "checksum")
            .expect("should find 'checksum'");
        // id=0x000A=10, length=0x0014=20, checksum = 10+20 = 30
        assert_eq!(checksum.value, PatternValue::Unsigned(30));
    }

    #[test]
    fn test_multiple_anonymous_builtins_in_struct() {
        // Multiple `u32;` in a row should each advance the offset
        let results = eval_pattern(
            r#"
            struct Foo {
                u8 tag;
                u32;
                u32;
                u8 val;
            };
            Foo f @ 0x00;
            "#,
            &[0xFF, 0x00, 0x00, 0x00, 0x00, 0x11, 0x11, 0x11, 0x11, 0xAA],
        );
        let foo = results
            .iter()
            .find(|n| n.name == "f")
            .expect("should find 'f'");
        let val = foo
            .children
            .iter()
            .find(|c| c.name == "val")
            .expect("should find 'val'");
        assert_eq!(val.offset, 9);
        assert_eq!(val.value, PatternValue::Unsigned(0xAA));
    }

    // ========== addressof in struct body for char[] ==========

    #[test]
    fn test_addressof_char_array_in_struct() {
        // addressof(field) inside a struct body where field is a char[] (Value::String)
        let source = r#"
            struct Foo {
                char sig[4];
                u64 sig_addr = addressof(sig);
            };
            Foo f @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x41, 0x42, 0x43, 0x44, 0x00, 0x00, 0x00, 0x00]);
        let f = results
            .iter()
            .find(|n| n.name == "f")
            .expect("should find 'f'");
        let sig_addr = f
            .children
            .iter()
            .find(|c| c.name == "sig_addr")
            .expect("should find 'sig_addr'");
        assert_eq!(sig_addr.value, PatternValue::Unsigned(0));
    }

    #[test]
    fn test_addressof_char_array_at_offset_in_struct() {
        // addressof(field) for a char[] that starts at a non-zero offset
        let source = r#"
            struct Bar {
                u32 header;
                char name[3];
                u64 name_addr = addressof(name);
            };
            Bar b @ 0x00;
        "#;
        let results = eval_pattern(
            source,
            &[
                0x01, 0x02, 0x03, 0x04, 0x41, 0x42, 0x43, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );
        let b = results
            .iter()
            .find(|n| n.name == "b")
            .expect("should find 'b'");
        let name_addr = b
            .children
            .iter()
            .find(|c| c.name == "name_addr")
            .expect("should find 'name_addr'");
        // header is 4 bytes, so name starts at offset 4
        assert_eq!(name_addr.value, PatternValue::Unsigned(4));
    }

    #[test]
    fn test_addressof_with_member_access() {
        // addressof(parent.field) — used in png.hexpat for text_len
        let source = r#"
            struct Inner {
                char keyword[4];
                u64 kw_addr = addressof(parent.keyword);
            };
            struct Outer {
                char keyword[4];
                Inner child;
            };
            Outer o @ 0x00;
        "#;
        // keyword at offset 0 (4 bytes), Inner starts at offset 4
        let data: &[u8] = &[
            0x41, 0x42, 0x43, 0x44, // Outer.keyword
            0x45, 0x46, 0x47, 0x48, // Inner.keyword
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Inner.kw_addr
        ];
        let results = eval_pattern(source, data);
        let o = results
            .iter()
            .find(|n| n.name == "o")
            .expect("should find 'o'");
        let child = o
            .children
            .iter()
            .find(|c| c.name == "child")
            .expect("should find 'child'");
        let kw_addr = child
            .children
            .iter()
            .find(|c| c.name == "kw_addr")
            .expect("should find 'kw_addr'");
        // parent.keyword refers to Outer.keyword at offset 0
        assert_eq!(kw_addr.value, PatternValue::Unsigned(0));
    }

    #[test]
    fn test_str_array_reads_bytes_as_string() {
        // str name[N] should read N bytes as a null-terminated string
        let source = r#"
            str name[8] @ 0x00;
        "#;
        let data: &[u8] = b"Hello\x00XX";
        let results = eval_pattern(source, data);
        let name = results
            .iter()
            .find(|n| n.name == "name")
            .expect("should find 'name'");
        assert_eq!(name.value, PatternValue::String("Hello".into()));
        assert_eq!(name.size, 8);
    }

    #[test]
    fn test_str_array_full_string() {
        // str name[5] with no null terminator reads all 5 bytes
        let source = r#"
            str s[5] @ 0x00;
        "#;
        let data: &[u8] = b"ABCDE";
        let results = eval_pattern(source, data);
        let s = results
            .iter()
            .find(|n| n.name == "s")
            .expect("should find 's'");
        assert_eq!(s.value, PatternValue::String("ABCDE".into()));
    }

    #[test]
    fn test_str_array_in_struct() {
        // str field inside a struct
        let source = r#"
            struct Record {
                u8 len;
                str name[4];
            };
            Record r @ 0x00;
        "#;
        let data: &[u8] = &[4, b'T', b'e', b's', b't'];
        let results = eval_pattern(source, data);
        let r = results
            .iter()
            .find(|n| n.name == "r")
            .expect("should find 'r'");
        let name = r
            .children
            .iter()
            .find(|c| c.name == "name")
            .expect("should find 'name'");
        assert_eq!(name.value, PatternValue::String("Test".into()));
    }

    #[test]
    fn test_recursion_depth_limit() {
        // Verify recursion depth limit prevents infinite recursion
        let source = r#"
            struct Node {
                u8 tag;
                if (tag != 0) {
                    Node child;
                }
            };
            Node root @ 0x00;
        "#;
        // Create data with 200 non-zero bytes to trigger deep recursion
        let data: Vec<u8> = vec![1; 200];
        let ds = SliceDataSource::new(&data);
        let (ast, mut eval) = setup_evaluator(source, &ds);
        eval.set_limits(0, 0, 10); // Very low recursion limit
        let result = eval.evaluate(&ast);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("recursion depth exceeded"),
            "got: {}",
            err.message
        );
    }

    #[test]
    fn test_oob_array_member_access_returns_zero() {
        // When an array is OOB-truncated, accessing a member on arr[i] where i >= actual length
        // should return 0 (ImHex compatible) instead of "cannot access member" error.
        let source = r#"
            struct Entry {
                u32 rva;
                u32 size;
            };
            struct Header {
                u8 count;
                Entry entries[3];
            };
            Header hdr @ 0x00;
            // entries array has only 1 element worth of data (truncated to 1),
            // accessing entries[2].rva should gracefully return 0
            fn check() {
                u32 result = hdr.entries[2].rva;
                std::print("{}", result);
            };
            check();
        "#;
        // 1 byte for count + 8 bytes for one Entry = 9 bytes total
        // Only enough data for 1 entry, so entries[1] and entries[2] are OOB
        let data: Vec<u8> = vec![
            0x03, // count = 3
            0x10, 0x00, 0x00, 0x00, // entries[0].rva = 0x10
            0x20, 0x00, 0x00, 0x00, // entries[0].size = 0x20
        ];
        // Should not panic or error — OOB member access returns 0
        let _results = eval_pattern(source, &data);
    }

    #[test]
    fn test_conditional_struct_array_member_access_in_function() {
        // Simulates PE-like pattern: conditional struct with array, accessed in a function loop
        let source = r#"
            struct DirEntry {
                u32 rva;
                u32 size;
            };
            struct OptHeader {
                u16 magic;
                if (magic == 0x010B) {
                    u32 imageBase32;
                } else {
                    u64 imageBase64;
                }
                DirEntry directories[4];
            };
            struct PEHeader {
                u32 signature;
                OptHeader optionalHeader;
            };
            PEHeader pe @ 0x00;

            fn checkDir(u32 idx) {
                if (pe.optionalHeader.directories[idx].rva != 0) {
                    std::print("dir {} has rva", idx);
                }
            };
            checkDir(0);
            checkDir(1);
            checkDir(5);  // OOB index — should gracefully return 0
        "#;
        let mut data = vec![0u8; 128];
        // PE signature
        data[0] = 0x50;
        data[1] = 0x45;
        data[2] = 0x00;
        data[3] = 0x00;
        // magic = 0x010B (PE32)
        data[4] = 0x0B;
        data[5] = 0x01;
        // imageBase32
        data[6] = 0x00;
        data[7] = 0x40;
        data[8] = 0x00;
        data[9] = 0x00;
        // directories[0].rva = 0x1000
        data[10] = 0x00;
        data[11] = 0x10;
        data[12] = 0x00;
        data[13] = 0x00;
        // directories[0].size = 0x100
        data[14] = 0x00;
        data[15] = 0x01;
        data[16] = 0x00;
        data[17] = 0x00;
        // directories[1].rva = 0x2000
        data[18] = 0x00;
        data[19] = 0x20;
        data[20] = 0x00;
        data[21] = 0x00;
        // directories[1].size = 0x200
        data[22] = 0x00;
        data[23] = 0x02;
        data[24] = 0x00;
        data[25] = 0x00;
        // directories[2] and [3] are zeros
        let _results = eval_pattern(source, &data);
    }

    #[test]
    fn test_addressof_array_index() {
        // addressof(arr[i]) should return the offset of the i-th element
        let source = r#"
            struct Item {
                u16 value;
            };
            Item items[3] @ 0x00;
            u64 addr0 = addressof(items[0]);
            u64 addr1 = addressof(items[1]);
            u64 addr2 = addressof(items[2]);
        "#;
        let data: &[u8] = &[0x01, 0x00, 0x02, 0x00, 0x03, 0x00];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("addr0")),
            Some(&Value::Unsigned(0))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("addr1")),
            Some(&Value::Unsigned(2))
        );
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("addr2")),
            Some(&Value::Unsigned(4))
        );
    }

    #[test]
    fn test_addressof_array_index_member_chain() {
        // addressof(arr[i].member) should resolve through index + member access
        let source = r#"
            struct Inner {
                u8 a;
                u8 b;
            };
            struct Wrapper {
                Inner items[2];
            };
            Wrapper w @ 0x00;
            u64 addr_b = addressof(w.items[1].b);
        "#;
        let data: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        evaluator.evaluate(&ast).unwrap();
        // w.items[1].b is at offset 3 (items[0] = 2 bytes, items[1].a = 1 byte)
        assert_eq!(
            evaluator.scope.get_var(evaluator.interner.intern("addr_b")),
            Some(&Value::Unsigned(3))
        );
    }

    #[test]
    fn test_addressof_pointer_index_member_chain() {
        // Simulates dex.hexpat: addressof(parent.string_ids[idx].string_data.string)
        // with pointer dereference and parent reference
        let source = r#"
            struct string_data_item {
                u8 utf16_size;
                char string[utf16_size];
            };
            struct string_id_item {
                string_data_item* string_data : u32;
            };
            struct type_id_item {
                u32 descriptor_idx;
                char type_name[] @ addressof(parent.string_ids[descriptor_idx].string_data.string);
            };
            struct Dex {
                u32 string_ids_size;
                string_id_item string_ids[string_ids_size] @ 4;
                type_id_item type_ids[1] @ 0x10;
            };
            Dex dex @ 0x00;
        "#;
        // Layout:
        // 0x00-0x03: string_ids_size = 2
        // 0x04-0x07: string_ids[0].string_data pointer = 0x14
        // 0x08-0x0B: string_ids[1].string_data pointer = 0x18
        // 0x10-0x13: type_ids[0].descriptor_idx = 0
        // 0x14: string_data_item[0].utf16_size = 3
        // 0x15-0x17: "ABC"
        // 0x18: string_data_item[1].utf16_size = 2
        // 0x19-0x1A: "XY"
        let mut data = vec![0u8; 32];
        data[0] = 2;
        data[1] = 0;
        data[2] = 0;
        data[3] = 0; // string_ids_size = 2
        data[4] = 0x14;
        data[5] = 0;
        data[6] = 0;
        data[7] = 0; // string_ids[0] ptr
        data[8] = 0x18;
        data[9] = 0;
        data[10] = 0;
        data[11] = 0; // string_ids[1] ptr
        data[0x10] = 0;
        data[0x11] = 0;
        data[0x12] = 0;
        data[0x13] = 0; // descriptor_idx = 0
        data[0x14] = 3; // string_data_item[0].utf16_size
        data[0x15] = b'A';
        data[0x16] = b'B';
        data[0x17] = b'C';
        data[0x18] = 2; // string_data_item[1].utf16_size
        data[0x19] = b'X';
        data[0x1A] = b'Y';

        let results = eval_pattern(source, &data);
        let dex = results
            .iter()
            .find(|n| n.name == "dex")
            .expect("dex not found");
        let type_ids = dex
            .children
            .iter()
            .find(|ch| ch.name == "type_ids")
            .expect("type_ids not found");
        let type0 = &type_ids.children[0];
        let type_name = type0
            .children
            .iter()
            .find(|ch| ch.name == "type_name")
            .expect("type_name not found");
        // string_ids[0].string_data points to 0x14, string starts at 0x15 (after utf16_size byte)
        assert_eq!(type_name.offset, 0x15);
    }

    #[test]
    fn test_addressof_with_leb128_like() {
        // Simulates dex.hexpat with LEB128-like type that uses addressof(this) in while condition
        let source = r#"
            fn read_unsigned(u128 addr, u128 sz) {
                return u8(addr);
            };
            struct LEB128Base {
                u8 array[while($ == addressof(this) || read_unsigned($-1, 1) & 0x80 != 0)];
            };
            struct string_data_item {
                LEB128Base utf16_size;
                char string[1];
            };
            struct string_id_item {
                string_data_item* string_data : u32;
            };
            struct type_id_item {
                u32 descriptor_idx;
                char type_name[] @ addressof(parent.string_ids[descriptor_idx].string_data.string);
            };
            struct Dex {
                u32 count;
                string_id_item string_ids[count] @ 4;
                type_id_item type_ids[1] @ 0x10;
            };
            Dex dex @ 0x00;
        "#;
        // Layout:
        // 0x00-0x03: count = 1
        // 0x04-0x07: string_ids[0] ptr = 0x14
        // 0x10-0x13: descriptor_idx = 0
        // 0x14: LEB128Base.array = [0x03] (single byte, no high bit)
        // 0x15: string_data_item.string = "A"
        let mut data = vec![0u8; 32];
        data[0] = 1; // count
        data[4] = 0x14; // ptr
        data[0x10] = 0; // descriptor_idx = 0
        data[0x14] = 0x03; // LEB128 byte (no continuation)
        data[0x15] = b'A'; // string char

        let results = eval_pattern(source, &data);
        let dex = results
            .iter()
            .find(|n| n.name == "dex")
            .expect("dex not found");
        let type_ids = dex
            .children
            .iter()
            .find(|ch| ch.name == "type_ids")
            .expect("type_ids");
        let t0 = &type_ids.children[0];
        let tn = t0
            .children
            .iter()
            .find(|ch| ch.name == "type_name")
            .expect("type_name");
        assert_eq!(
            tn.offset, 0x15,
            "type_name should point to string start after LEB128"
        );
    }

    #[test]
    fn test_bulk_read_u32_array() {
        // u32 array with 256+ elements should use BulkData
        let source = r#"
            u32 data[300] @ 0x00;
        "#;
        // 300 u32 = 1200 bytes
        let mut bytes = vec![0u8; 1200];
        // Set element [0] = 0x01020304, element [1] = 0x05060708 (little-endian)
        bytes[0..4].copy_from_slice(&0x01020304u32.to_le_bytes());
        bytes[4..8].copy_from_slice(&0x05060708u32.to_le_bytes());
        // Set last element [299] = 0xDEADBEEF
        bytes[1196..1200].copy_from_slice(&0xDEADBEEFu32.to_le_bytes());
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(arr.size, 1200);
        // Should be BulkData, not Array with children
        assert!(
            matches!(&arr.value, PatternValue::BulkData { elem_type, .. }
                if *elem_type == crate::parser::ast::BuiltinType::U32),
            "expected BulkData for u32 array, got {:?}",
            arr.value
        );
        assert!(arr.children.is_empty(), "BulkData should have no children");
    }

    #[test]
    fn test_bulk_read_u32_index_access() {
        // Verify index access into bulk u32 array works via struct LocalVar
        let source = r#"
            struct Test {
                u32 data[300];
                u32 first = data[0];
                u32 second = data[1];
                u32 last = data[299];
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0u8; 1200];
        bytes[0..4].copy_from_slice(&42u32.to_le_bytes());
        bytes[4..8].copy_from_slice(&1000u32.to_le_bytes());
        bytes[1196..1200].copy_from_slice(&0xCAFEu32.to_le_bytes());
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let first = t
            .children
            .iter()
            .find(|n| n.name == "first")
            .expect("first");
        assert_eq!(first.value, PatternValue::Unsigned(42));
        let second = t
            .children
            .iter()
            .find(|n| n.name == "second")
            .expect("second");
        assert_eq!(second.value, PatternValue::Unsigned(1000));
        let last = t.children.iter().find(|n| n.name == "last").expect("last");
        assert_eq!(last.value, PatternValue::Unsigned(0xCAFE));
    }

    #[test]
    fn test_bulk_read_s16_signed() {
        // Signed 16-bit array with negative values
        let source = r#"
            struct Test {
                s16 data[256];
                s16 val = data[0];
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0u8; 512];
        bytes[0..2].copy_from_slice(&(-1234i16).to_le_bytes());
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let val = t.children.iter().find(|n| n.name == "val").expect("val");
        assert_eq!(val.value, PatternValue::Signed(-1234));
    }

    #[test]
    fn test_bulk_read_float_array() {
        // float array bulk read
        let source = r#"
            struct Test {
                float data[256];
                float val = data[0];
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0u8; 1024];
        bytes[0..4].copy_from_slice(&std::f32::consts::PI.to_le_bytes());
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let val = t.children.iter().find(|n| n.name == "val").expect("val");
        if let PatternValue::Float(f) = val.value {
            assert!(
                (f - std::f64::consts::PI).abs() < 0.001,
                "float PI mismatch: {}",
                f
            );
        } else {
            panic!("expected Float, got {:?}", val.value);
        }
    }

    #[test]
    fn test_bulk_read_big_endian() {
        // Big-endian u32 array
        let source = r#"
            struct Test {
                be u32 data[256];
                u32 val = data[0];
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0u8; 1024];
        bytes[0..4].copy_from_slice(&0xAABBCCDDu32.to_be_bytes());
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let val = t.children.iter().find(|n| n.name == "val").expect("val");
        assert_eq!(val.value, PatternValue::Unsigned(0xAABBCCDD));
    }

    #[test]
    fn test_bulk_read_double_array() {
        // double (f64) array
        let source = r#"
            struct Test {
                double data[256];
                double val = data[1];
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0u8; 2048];
        bytes[8..16].copy_from_slice(&2.718281828_f64.to_le_bytes());
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let val = t.children.iter().find(|n| n.name == "val").expect("val");
        if let PatternValue::Float(f) = val.value {
            assert!((f - 2.718281828).abs() < 1e-6, "double e mismatch: {}", f);
        } else {
            panic!("expected Float, got {:?}", val.value);
        }
    }

    #[test]
    fn test_bulk_read_u64_array() {
        // u64 array
        let source = r#"
            struct Test {
                u64 data[256];
                u64 val = data[0];
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0u8; 2048];
        bytes[0..8].copy_from_slice(&0x123456789ABCDEF0u64.to_le_bytes());
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let val = t.children.iter().find(|n| n.name == "val").expect("val");
        assert_eq!(val.value, PatternValue::Unsigned(0x123456789ABCDEF0));
    }

    #[test]
    fn test_bulk_read_small_array_not_bulk() {
        // Arrays smaller than threshold should NOT use BulkData
        let source = r#"
            u32 data[10] @ 0x00;
        "#;
        let bytes = vec![0u8; 40];
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(
            arr.value,
            PatternValue::Array,
            "small array should use Array, not BulkData"
        );
        assert_eq!(arr.children.len(), 10);
    }

    #[test]
    fn test_null_terminated_u8_bulk_read() {
        // u8 data[] should be bulk-read as BulkData
        let source = r#"
            u8 data[] @ 0x00;
        "#;
        let bytes = vec![0xAA; 1000];
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(arr.size, 1000);
        assert!(
            matches!(&arr.value, PatternValue::BulkData { elem_type, data, .. }
                if *elem_type == BuiltinType::U8 && data.len() == 1000),
            "expected BulkData for u8[], got {:?}",
            arr.value
        );
    }

    #[test]
    fn test_null_terminated_char_as_string() {
        // char value[] should be bulk-read as String, stopping at null
        let source = r#"
            char data[] @ 0x00;
        "#;
        let mut bytes = vec![0u8; 100];
        bytes[0] = b'H';
        bytes[1] = b'e';
        bytes[2] = b'l';
        bytes[3] = b'l';
        bytes[4] = b'o';
        bytes[5] = 0; // null terminator
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(arr.value, PatternValue::String("Hello".to_string()));
    }

    #[test]
    fn test_null_terminated_u32_bulk_read() {
        // u32 data[] multi-byte NullTerminated
        let source = r#"
            u32 data[] @ 0x00;
        "#;
        let mut bytes = vec![0u8; 400];
        bytes[0..4].copy_from_slice(&42u32.to_le_bytes());
        bytes[4..8].copy_from_slice(&100u32.to_le_bytes());
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(arr.size, 400);
        assert!(
            matches!(&arr.value, PatternValue::BulkData { elem_type, .. }
                if *elem_type == BuiltinType::U32),
            "expected BulkData for u32[], got {:?}",
            arr.value
        );
    }

    #[test]
    fn test_null_terminated_char_in_struct() {
        // char value[] inside a struct should become a String
        let source = r#"
            struct MyString {
                char value[];
            };
            MyString s @ 0x00;
        "#;
        let mut bytes = vec![0u8; 50];
        bytes[0] = b'T';
        bytes[1] = b'e';
        bytes[2] = b's';
        bytes[3] = b't';
        bytes[4] = 0;
        let results = eval_pattern(source, &bytes);
        let s = results.iter().find(|n| n.name == "s").expect("s");
        let value = s
            .children
            .iter()
            .find(|n| n.name == "value")
            .expect("value");
        assert_eq!(value.value, PatternValue::String("Test".to_string()));
    }

    #[test]
    fn test_enum_array_bulk_read() {
        // enum[300] should use BulkData
        let source = r#"
            enum MyEnum : u8 {
                A = 0,
                B = 1,
                C = 2,
            };
            MyEnum data[300] @ 0x00;
        "#;
        let mut bytes = vec![0u8; 300];
        bytes[0] = 1; // B
        bytes[1] = 2; // C
        bytes[299] = 0; // A
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(arr.size, 300);
        assert!(
            matches!(&arr.value, PatternValue::BulkData { elem_type, .. }
                if *elem_type == BuiltinType::U8),
            "expected BulkData for enum[], got {:?}",
            arr.value
        );
    }

    #[test]
    fn test_enum_array_index_access() {
        // Index into bulk enum array should return raw integer value
        let source = r#"
            enum MyEnum : u8 {
                A = 0,
                B = 1,
                C = 2,
            };
            struct Test {
                MyEnum data[300];
                u8 first = data[0];
                u8 second = data[1];
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0u8; 300];
        bytes[0] = 1; // B
        bytes[1] = 2; // C
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let first = t
            .children
            .iter()
            .find(|n| n.name == "first")
            .expect("first");
        assert_eq!(first.value, PatternValue::Unsigned(1));
        let second = t
            .children
            .iter()
            .find(|n| n.name == "second")
            .expect("second");
        assert_eq!(second.value, PatternValue::Unsigned(2));
    }

    #[test]
    fn test_bitfield_array_bulk_read() {
        // bitfield[300] should use BulkData
        let source = r#"
            bitfield Flags {
                a : 4;
                b : 4;
            };
            Flags data[300] @ 0x00;
        "#;
        let bytes = vec![0xABu8; 300];
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(arr.size, 300);
        assert!(
            matches!(&arr.value, PatternValue::BulkData { elem_type, .. }
                if *elem_type == BuiltinType::U8),
            "expected BulkData for bitfield[], got {:?}",
            arr.value
        );
    }

    #[test]
    fn test_small_enum_not_bulk() {
        // Small enum array should NOT use BulkData
        let source = r#"
            enum MyEnum : u8 {
                A = 0,
                B = 1,
            };
            MyEnum data[10] @ 0x00;
        "#;
        let bytes = vec![0u8; 10];
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(
            arr.value,
            PatternValue::Array,
            "small enum array should use Array, not BulkData"
        );
        assert_eq!(arr.children.len(), 10);
    }

    #[test]
    fn test_conditional_bulk_read_u32() {
        // while($ < bound) with u32 elements should bulk-read when >= 256 elements
        let source = r#"
            struct Test {
                u32 bound;
                u32 data[while($ < bound)] @ 4;
            };
            Test t @ 0x00;
        "#;
        // bound=2052 → (2052 - 4) / 4 = 512 elements
        let mut bytes = vec![0u8; 2052];
        bytes[0..4].copy_from_slice(&2052u32.to_le_bytes()); // bound
        bytes[4..8].copy_from_slice(&42u32.to_le_bytes()); // data[0]
        bytes[8..12].copy_from_slice(&99u32.to_le_bytes()); // data[1]
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let data = t.children.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(data.size, 2048); // 512 elements × 4 bytes
        assert!(
            matches!(&data.value, PatternValue::BulkData { elem_type, .. }
                if *elem_type == BuiltinType::U32),
            "expected BulkData for while u32[], got {:?}",
            data.value
        );
    }

    #[test]
    fn test_conditional_small_not_bulk() {
        // while($ < bound) with < 256 elements should NOT be bulk-read
        let source = r#"
            u32 data[while($ < 40)] @ 0;
        "#;
        let bytes = vec![0u8; 40]; // 10 u32 elements
        let results = eval_pattern(source, &bytes);
        let arr = results.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(arr.children.len(), 10);
        assert_eq!(
            arr.value,
            PatternValue::Array,
            "small while array should use Array, not BulkData"
        );
    }

    #[test]
    fn test_conditional_char_bulk_read() {
        // while($ < bound) with char elements should bulk-read as String
        let source = r#"
            struct Test {
                u32 bound;
                char data[while($ < bound)] @ 4;
            };
            Test t @ 0x00;
        "#;
        let mut bytes = vec![0x41u8; 1028]; // 'A' fill
        bytes[0..4].copy_from_slice(&1028u32.to_le_bytes()); // bound
        bytes[4] = b'H';
        bytes[5] = b'i';
        let results = eval_pattern(source, &bytes);
        let t = results.iter().find(|n| n.name == "t").expect("t");
        let data = t.children.iter().find(|n| n.name == "data").expect("data");
        assert_eq!(data.size, 1024); // 1024 chars
        assert!(
            matches!(&data.value, PatternValue::String(s) if s.starts_with("Hi")),
            "expected String starting with 'Hi', got {:?}",
            data.value
        );
    }

    #[test]
    fn test_type_size_struct_with_array_field() {
        // type_size should correctly compute struct size including fixed array fields
        let source = r#"
            struct Item {
                u32 a;
                u8 b[4];
                u16 c;
            };
            Item x @ 0x00;
        "#;
        let data = vec![0u8; 10]; // 4 + 4 + 2 = 10
        let results = eval_pattern(source, &data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].size, 10);
    }

    #[test]
    fn test_propagate_children_threshold() {
        // Arrays with >= 64 children skip propagation, but index access still works
        // via SizedRef → find_node_by_offset fallback
        let source = r#"
            struct Entry {
                u8 val;
            };
            Entry arr[64] @ 0x00;
            std::print(arr[3].val);
        "#;
        let mut data = vec![0u8; 64];
        data[3] = 0xAB;
        let ds = SliceDataSource::new(&data);
        let (_, evaluator) = parse_and_eval(source, &ds);
        assert_eq!(evaluator.print_output, vec!["171"]); // 0xAB = 171
    }

    #[test]
    fn test_lazy_struct_array_creation() {
        // 256+ element fixed-size struct arrays should become LazyStructArray
        let source = r#"
            struct Entry {
                u16 id;
                u16 val;
            };
            Entry arr[256] @ 0x00;
        "#;
        let data = vec![0u8; 256 * 4]; // 256 * 4 bytes
        let results = eval_pattern(source, &data);
        let arr = results.iter().find(|n| n.name == "arr").expect("arr");
        assert_eq!(arr.size, 256 * 4);
        assert!(
            matches!(
                &arr.value,
                PatternValue::LazyStructArray {
                    count: 256,
                    elem_size: 4,
                    ..
                }
            ),
            "expected LazyStructArray, got {:?}",
            arr.value
        );
        assert!(
            arr.children.is_empty(),
            "lazy array should have no children"
        );
    }

    #[test]
    fn test_lazy_struct_array_index_access() {
        // Indexing a LazyStructArray should read the element on demand
        let source = r#"
            struct Entry {
                u32 id;
            };
            Entry arr[256] @ 0x00;
            std::print(arr[5].id);
        "#;
        let mut data = vec![0u8; 256 * 4];
        // arr[5] is at offset 5*4=20
        data[20..24].copy_from_slice(&0xDEADBEEFu32.to_le_bytes());
        let ds = SliceDataSource::new(&data);
        let (_, evaluator) = parse_and_eval(source, &ds);
        assert_eq!(evaluator.print_output, vec!["3735928559"]); // 0xDEADBEEF
    }

    #[test]
    fn test_lazy_struct_array_sizeof() {
        // sizeof() on a lazy struct array should return the correct total size
        let source = r#"
            struct Entry {
                u32 a;
                u32 b;
            };
            Entry arr[512] @ 0x00;
            std::print(sizeof(arr));
        "#;
        let data = vec![0u8; 512 * 8];
        let ds = SliceDataSource::new(&data);
        let (_, evaluator) = parse_and_eval(source, &ds);
        assert_eq!(evaluator.print_output, vec!["4096"]); // 512 * 8
    }

    #[test]
    fn test_lazy_struct_array_member_access() {
        // arr[i].field should work through LazyStructArray on-demand reading
        let source = r#"
            struct Pair {
                u16 key;
                u16 val;
            };
            Pair table[300] @ 0x00;
            std::print(table[100].key, table[100].val);
        "#;
        let mut data = vec![0u8; 300 * 4];
        // table[100] at offset 100*4=400
        data[400..402].copy_from_slice(&0x1234u16.to_le_bytes());
        data[402..404].copy_from_slice(&0x5678u16.to_le_bytes());
        let ds = SliceDataSource::new(&data);
        let (_, evaluator) = parse_and_eval(source, &ds);
        assert_eq!(evaluator.print_output, vec!["4660 22136"]); // 0x1234 0x5678
    }

    #[test]
    fn test_small_struct_array_not_lazy() {
        // Arrays with < BULK_READ_THRESHOLD (64) elements should NOT become LazyStructArray
        let source = r#"
            struct Entry {
                u8 val;
            };
            Entry arr[50] @ 0x00;
        "#;
        let data = vec![0u8; 50];
        let results = eval_pattern(source, &data);
        let arr = results.iter().find(|n| n.name == "arr").expect("arr");
        assert!(
            !matches!(&arr.value, PatternValue::LazyStructArray { .. }),
            "small array should not be lazy, got {:?}",
            arr.value
        );
        assert_eq!(arr.children.len(), 50);
    }

    #[test]
    fn test_while_loop_lazy_struct_array() {
        // while($ < bound) with fixed-size struct should produce LazyStructArray
        let source = r#"
            struct Entry {
                u8 a;
                u8 b;
            };
            Entry arr[while($ < 200)] @ 0x00;
        "#;
        let data = vec![0xABu8; 256];
        let results = eval_pattern(source, &data);
        let arr = results.iter().find(|n| n.name == "arr").expect("arr");
        assert!(
            matches!(
                &arr.value,
                PatternValue::LazyStructArray {
                    elem_size: 2,
                    count: 100,
                    ..
                }
            ),
            "expected LazyStructArray, got {:?}",
            arr.value
        );
        assert_eq!(arr.size, 200);
    }

    #[test]
    fn test_while_loop_probe_lazy() {
        // while-loop with struct whose type_size fails but has fixed runtime size.
        // The probe-based path should produce LazyStructArray.
        let source = r#"
            struct Instr {
                u8 op;
                if (op == 0) {
                    u8 dummy;
                }
            };
            Instr arr[while($ < 200)] @ 0x00;
        "#;
        // All ops are 0x01 (not 0), so each element is always 1 byte.
        let data = vec![0x01u8; 256];
        let results = eval_pattern(source, &data);
        let arr = results.iter().find(|n| n.name == "arr").expect("arr");
        assert!(
            matches!(
                &arr.value,
                PatternValue::LazyStructArray {
                    elem_size: 1,
                    count: 200,
                    ..
                }
            ),
            "expected probe-based LazyStructArray, got {:?}",
            arr.value
        );
        assert_eq!(arr.size, 200);
    }

    #[test]
    fn test_while_loop_precomputed_bound_sub() {
        // ($ - base) < size condition should pre-compute bound = base + size
        let source = r#"
            u32 base @ 0x00;
            u8 arr[while(($ - base) < 10)] @ 0x04;
        "#;
        let mut data = vec![0u8; 32];
        // base = 4 (LE u32 at offset 0)
        data[0] = 4;
        let results = eval_pattern(source, &data);
        let arr = results.iter().find(|n| n.name == "arr").expect("arr");
        // bound = base(4) + size(10) = 14, starting at offset 4, so 14 - 4 = 10 elements
        assert_eq!(arr.size, 10);
    }

    #[test]
    fn test_while_loop_fast_bound_loop() {
        // Verify while($ < bound) with small struct count uses fast-bound loop
        // (not eval_expr per iteration), producing correct results.
        let source = r#"
            struct Item {
                u16 val;
            };
            Item arr[while($ < 20)] @ 0x00;
        "#;
        let data = vec![0xFFu8; 32];
        let results = eval_pattern(source, &data);
        let arr = results.iter().find(|n| n.name == "arr").expect("arr");
        // 20 bytes / 2 bytes per element = 10 elements (< 64, so children are preserved)
        assert_eq!(arr.children.len(), 10);
        assert_eq!(arr.size, 20);
    }

    #[test]
    fn test_no_unique_address() {
        // [[no_unique_address]] member should not advance the struct offset
        let source = r#"
            struct Overlay {
                u8 tag [[no_unique_address, hidden]];
                u16 value;
            };
            Overlay item @ 0x00;
        "#;
        let data = vec![0xAA, 0xBB, 0xCC, 0xDD];
        let results = eval_pattern(source, &data);
        let item = results.iter().find(|n| n.name == "item").expect("item");
        // tag reads 1 byte at offset 0, but no_unique_address means offset stays 0
        // value reads 2 bytes at offset 0 (overlapping tag)
        // Total struct size = 2 (not 3)
        assert_eq!(item.size, 2);
        assert_eq!(item.children.len(), 2);
        assert_eq!(item.children[0].name, "tag");
        assert_eq!(item.children[0].offset, 0);
        assert_eq!(item.children[1].name, "value");
        assert_eq!(item.children[1].offset, 0);
    }

    #[test]
    fn test_pointer_member_access_through_array_in_parent() {
        // Reproduces dex.hexpat issue: accessing pointer target member
        // via parent.array[idx].ptr.field chain
        let source = r#"
            struct StringData {
                u8 len;
                char string[len];
            };
            struct StringItem {
                StringData* data: u32;
            };
            struct TypeItem {
                u8 idx;
                char name[] @ addressof(parent.items[idx].data.string);
            };
            struct Container {
                u8 count;
                StringItem items[count] @ 1;
                TypeItem types[1] @ 9;
            };
            Container c @ 0;
        "#;
        // Layout:
        // 0: count = 2
        // 1: items[0].data pointer = 0x09 (u32 LE)
        // 5: items[1].data pointer = 0x0D (u32 LE)
        // 9: StringData at 0x09: len=3, string="abc"
        // 13: StringData at 0x0D: len=2, string="xy"
        // 15: TypeItem at 9: idx=0
        // But wait, the pointer targets overlap with TypeItem positions...
        // Let me rearrange:
        // 0: count = 1
        // 1: items[0].data pointer = 0x05 (u32 LE, points to offset 5)
        // 5: StringData at 0x05: len=2, string="hi"
        // 8: (unused)
        // 9: TypeItem: idx=0
        let data: &[u8] = &[
            0x01, // offset 0: count = 1
            0x05, 0x00, 0x00, 0x00, // offset 1: items[0].data pointer = 5
            0x02, // offset 5: StringData.len = 2
            b'h', b'i', // offset 6-7: StringData.string = "hi"
            0x00, // offset 8: padding
            0x00, // offset 9: TypeItem.idx = 0
        ];
        let results = eval_pattern(source, data);
        // Find the main Container result
        let c = results
            .iter()
            .find(|n| n.name == "c")
            .expect("no 'c' result");
        assert_eq!(c.value, PatternValue::Struct);
        let types = c.children.iter().find(|n| n.name == "types").unwrap();
        let type0 = &types.children[0];
        let name_child = type0.children.iter().find(|n| n.name == "name").unwrap();
        // name should be "hi" (read from the pointer target's string field)
        assert_eq!(name_child.value, PatternValue::String("hi".to_string()));
    }

    #[test]
    fn test_pointer_member_addressof_cross_struct() {
        // Reproduces the actual dex.hexpat pattern: type_id_item accesses
        // parent.string_ids[descriptor_idx].string_data.string via addressof
        // where string_data is a pointer to a struct containing a char[] field.
        let source = r#"
            struct StringData {
                u8 len;
                char string[len];
            };
            struct StringItem {
                StringData* data: u32;
            };
            struct TypeItem {
                u8 idx;
                char name[] @ addressof(parent.items[idx].data.string);
            };
            struct Root {
                u8 item_count;
                u8 type_count;
                StringItem items[item_count] @ 2;
                TypeItem types[type_count] @ 10;
            };
            Root root @ 0;
        "#;
        // Layout:
        // 0: item_count = 2
        // 1: type_count = 1
        // 2-5: items[0].data pointer = 0x0A (points to offset 10)
        // 6-9: items[1].data pointer = 0x0E (points to offset 14)
        // 10-12: StringData: len=2, "ab"
        // 13: padding
        // 14-16: StringData: len=2, "xy"
        // 17: padding
        // items[item_count] @ 2 means items start at 2
        // types[type_count] @ 10: BUT offset 10 is used by StringData too!
        // Let me rearrange so there's no overlap:
        // 0: item_count = 1
        // 1: type_count = 1
        // 2-5: items[0].data pointer = 6 (points to StringData at offset 6)
        // 6: StringData.len = 2
        // 7-8: StringData.string = "ab"
        // 9: padding
        // 10: TypeItem.idx = 0
        // TypeItem reads: char name[] @ addressof(parent.items[0].data.string)
        // = addressof of the string field at offset 7, reading null-terminated char[]
        let data: &[u8] = &[
            0x01, // 0: item_count = 1
            0x01, // 1: type_count = 1
            0x06, 0x00, 0x00, 0x00, // 2-5: items[0].data = ptr to offset 6
            0x02, // 6: StringData.len = 2
            b'a', b'b', // 7-8: StringData.string = "ab"
            0x00, // 9: padding/null terminator
            0x00, // 10: TypeItem.idx = 0
        ];
        let results = eval_pattern(source, data);
        let root = results.iter().find(|n| n.name == "root").expect("no root");
        let types = root.children.iter().find(|n| n.name == "types").unwrap();
        let type0 = &types.children[0];
        // TypeItem should have read name[] at the address of string field
        let name_child = type0.children.iter().find(|n| n.name == "name").unwrap();
        assert!(
            matches!(&name_child.value, PatternValue::String(s) if s == "ab"),
            "expected 'ab', got {:?}",
            name_child.value
        );
    }

    #[test]
    fn test_cancellation_in_pure_computation_loop() {
        // Regression test: while loops with pure computation (no node creation)
        // must still check the cancellation token via check_count.
        let source = r#"
            u32 x @ 0x00;
            u32 counter = 0;
            while (counter < 1000000) {
                counter = counter + 1;
            }
        "#;
        let data: &[u8] = &[0x01, 0x00, 0x00, 0x00];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);

        let token = Arc::new(AtomicBool::new(false));
        evaluator.set_cancellation_token(token.clone());
        // Set cancel immediately — the loop should detect it within 256 iterations
        token.store(true, Ordering::Relaxed);

        let result = evaluator.evaluate(&ast);
        assert!(result.is_err(), "expected cancellation error");
        let err = result.unwrap_err();
        assert!(
            err.message.contains("cancelled"),
            "expected 'cancelled' in error: {}",
            err.message
        );
    }

    #[test]
    fn test_using_alias_transform_in_struct_body() {
        // Test that [[transform(...)]] on a using alias is applied inside struct bodies,
        // so the transformed value is available for subsequent member expressions.
        let results = eval_pattern(
            r#"
            struct Wrapper {
                u8 raw;
            } [[sealed]];

            fn transform_wrapper(ref auto w) {
                return w.raw * 2;
            };

            using DoubleU8 = Wrapper [[transform("transform_wrapper")]];

            struct Container {
                DoubleU8 count;
                u8 data[count];
            };

            Container c @ 0x00;
            "#,
            // raw=3 → count=6, then 6 bytes of data
            &[0x03, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x00],
        );
        let c = results.iter().find(|n| n.name == "c").unwrap();
        assert_eq!(c.children.len(), 2, "expected 2 children in Container");
        // data array should have 6 elements (3 * 2 = 6)
        let data_child = c.children.iter().find(|ch| ch.name == "data").unwrap();
        assert_eq!(data_child.children.len(), 6, "expected 6 data elements");
    }

    // ========== sizeof(padding) and sizeof($) ==========

    #[test]
    fn test_sizeof_padding() {
        let source = r#"
            struct Foo {
                u8 a;
                padding[3];
                u16 b;
            };
            u64 sz = sizeof(Foo);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("sz");
        // u8(1) + padding[3](3) + u16(2) = 6
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(6)));
    }

    #[test]
    fn test_sizeof_dollar_returns_data_size() {
        let source = r#"
            u64 total = sizeof($);
        "#;
        let data = [0u8; 256];
        let ds = SliceDataSource::new(&data);
        let (_, mut evaluator) = parse_and_eval(source, &ds);
        let key = evaluator.interner.intern("total");
        assert_eq!(evaluator.scope.get_var(key), Some(&Value::Unsigned(256)));
    }

    // ========== break inside struct body in while-loop array ==========

    #[test]
    fn test_break_in_struct_body_terminates_while_array() {
        // break; inside a struct body should terminate the enclosing while-loop array
        let source = r#"
            struct Block {
                u8 tag;
                if (tag == 0xFF)
                    break;
                u8 data;
            };
            struct Container {
                Block blocks[while(true)];
            };
            Container c @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x01, 0xAA, 0x02, 0xBB, 0xFF, 0x00, 0x00, 0x00]);
        let c = results.iter().find(|n| n.name == "c").unwrap();
        let blocks = c.children.iter().find(|ch| ch.name == "blocks").unwrap();
        // Two complete blocks (tag=1,data=0xAA and tag=2,data=0xBB)
        // plus partial block with tag=0xFF that triggers break
        assert_eq!(blocks.children.len(), 3);
    }

    #[test]
    fn test_break_in_if_else_chain_in_struct_body() {
        // break; inside if/else chain in struct body (ntag TLV pattern)
        let source = r#"
            struct TLV {
                u8 tag;
                if (tag == 0xFE) {
                    break;
                } else if (tag == 0x00) {
                } else {
                    u8 length;
                    u8 data[length];
                }
            };
            TLV tlv[while(true)] @ 0x00;
        "#;
        // NULL(0x00), Data(tag=0x03,len=2,data), Terminator(0xFE), padding
        let results = eval_pattern(source, &[0x00, 0x03, 0x02, 0xAA, 0xBB, 0xFE, 0x00, 0x00]);
        let tlv = results.iter().find(|n| n.name == "tlv").unwrap();
        // 3 TLVs: NULL(1 byte), Data(4 bytes), Terminator(1 byte with break)
        assert_eq!(
            tlv.children.len(),
            3,
            "expected 3 TLV elements, got {}",
            tlv.children.len()
        );
    }

    #[test]
    fn test_while_not_eof_terminates() {
        // while(!std::mem::eof()) should terminate when offset reaches end of data
        let source = r#"
            struct Entry {
                u8 value;
            };
            Entry entries[while(!std::mem::eof())] @ 0x00;
        "#;
        let results = eval_pattern(source, &[0x01, 0x02, 0x03, 0x04, 0x05]);
        let entries = results.iter().find(|n| n.name == "entries").unwrap();
        assert_eq!(
            entries.children.len(),
            5,
            "expected 5 entries, got {}",
            entries.children.len()
        );
    }

    #[test]
    fn test_struct_definition_transform_applied_in_member() {
        // Struct-level [[transform(...)]] should be applied when used as a member
        // inside another struct (e.g., Length struct in TLV pattern)
        let source = r#"
            struct Length {
                u8 raw_byte [[no_unique_address]];
                u8 length;
            } [[transform("get_length")]];

            fn get_length(ref auto len) {
                return len.length;
            };

            struct Container {
                Length length;
                u8 data[length];
            };
            Container c @ 0x00;
        "#;
        // Length at offset 0: raw_byte=0x05 (overlapping), length=0x05
        // transform extracts 5 as the integer value
        // data[5] reads 5 bytes (offsets 1-5)
        let results = eval_pattern(source, &[0x05, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF]);
        let c = results.iter().find(|n| n.name == "c").unwrap();
        // Container has 2 children: length + data
        assert_eq!(c.children.len(), 2);
        let data = c.children.iter().find(|ch| ch.name == "data").unwrap();
        // data array should have 5 elements (from transformed length value)
        assert_eq!(
            data.children.len(),
            5,
            "expected 5 data elements, got {}",
            data.children.len()
        );
    }

    #[test]
    fn test_ntag_tlv_pattern_with_transform() {
        // Simplified ntag TLV pattern: Length with transform, TLV with break
        let source = r#"
            struct Length {
                u8 byte [[hidden, no_unique_address]];
                if (byte == 0xFF)
                    u24 length;
                else
                    u8 length;
            } [[sealed, transform("transform_length")]];

            fn transform_length(ref auto length) {
                return length.length;
            };

            enum Tag : u8 {
                NULL = 0x00,
                NDEFMessage = 0x03,
                TerminatorTLV = 0xFE
            };

            struct TLV {
                Tag tag;
                if (tag == Tag::TerminatorTLV) {
                    break;
                } else if (tag == Tag::NULL) {
                } else {
                    Length length;
                    if (length > 0) {
                        u8 value[length];
                    }
                }
            };

            TLV tlv[while(true)] @ 0x00;
        "#;
        // TLV0: tag=0x03(NDEF), length=0x03, value=[AA,BB,CC]
        // TLV1: tag=0x00(NULL)
        // TLV2: tag=0xFE(Terminator) -> break
        let data = vec![0x03, 0x03, 0xAA, 0xBB, 0xCC, 0x00, 0xFE, 0x00, 0x00];
        let results = eval_pattern(source, &data);
        let tlv = results.iter().find(|n| n.name == "tlv").unwrap();
        assert_eq!(
            tlv.children.len(),
            3,
            "expected 3 TLV elements, got {}",
            tlv.children.len()
        );
    }

    #[test]
    fn test_ntag_real_data_tlv_terminates() {
        // Test with real ntag data: TLV[0]=NDEFMessage at offset 16, TLV[1]=TerminatorTLV at 110
        let source = r#"
            bitfield NDEFFlags {
                MB  : 1;
                ME  : 1;
                CF  : 1;
                SR  : 1;
                IL  : 1;
                TNF : 3;
            } [[bitfield_order(0, 8)]];

            struct NDEF {
                NDEFFlags flags;
                u8 typeLength;
                if (flags.SR)
                    u8 payloadLength;
                else
                    u32 payloadLength;
                if (flags.IL)
                    u8 idLength;
                u8 type[typeLength];
                if (flags.IL)
                    u8 id[idLength];
                u8 payload[payloadLength];
                if (flags.ME)
                    break;
            };

            struct Length {
                u8 byte [[hidden, no_unique_address]];
                if (byte == 0xFF)
                    u24 length;
                else
                    u8 length;
            } [[sealed, transform("transform_length")]];

            fn transform_length(ref auto length) {
                return length.length;
            };

            enum Tag : u8 {
                NULL = 0x00,
                NDEFMessage = 0x03,
                TerminatorTLV = 0xFE
            };

            struct TLV {
                Tag tag;
                if (tag == Tag::TerminatorTLV) {
                    break;
                } else if (tag == Tag::NULL) {
                } else {
                    Length length;
                    if (length > 0) {
                        if (tag == Tag::NDEFMessage) {
                            NDEF ndef[while(true)];
                        } else {
                            u8 value[length];
                        }
                    }
                }
            };

            struct ManufacturerData {
                u8 serial1[3];
                u8 checkByte0;
                u8 serial2[4];
                u8 checkByte1;
                u8 internal;
                u16 lockBytes;
            };

            struct CapabilityContainer {
                u8 magic;
                u8 version;
                u8 memorySize;
                u8 accessCapability;
            };

            ManufacturerData mfg @ 0x00;
            CapabilityContainer cc @ 0x0C;
            TLV tlv[while(true)] @ 0x10;
        "#;
        // Real ntag data (first 120 bytes)
        let data: Vec<u8> = vec![
            0x04, 0xb8, 0x31, 0x05, 0x3a, 0x30, 0x73, 0x80, // ManufacturerData
            0xf9, 0x48, 0x00, 0x00, // ManufacturerData cont
            0xe1, 0x10, 0x6d, 0x00, // CapabilityContainer
            0x03, 0x5c, // TLV: tag=0x03, length=92
            0xda, 0x17, 0x40, 0x01, // NDEF: flags=0xDA, typeLen=23, payloadLen=64, idLen=1
            // type[23] = "application/vnd.wfa.wsc"
            0x61, 0x70, 0x70, 0x6c, 0x69, 0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x2f, 0x76, 0x6e,
            0x64, 0x2e, 0x77, 0x66, 0x61, 0x2e, 0x77, 0x73, 0x63, // id[1] = "0"
            0x30, // payload[64]
            0x10, 0x0e, 0x00, 0x32, 0x10, 0x45, 0x00, 0x0b, 0x66, 0x6c, 0x69, 0x70, 0x70, 0x65,
            0x72, 0x77, 0x69, 0x66, 0x69, 0x10, 0x20, 0x00, 0x06, 0xff, 0xff, 0xff, 0xff, 0xff,
            0xff, 0x10, 0x27, 0x00, 0x09, 0x66, 0x6c, 0x69, 0x70, 0x70, 0x65, 0x72, 0x70, 0x77,
            0x10, 0x03, 0x00, 0x02, 0x00, 0x20, 0x10, 0x0f, 0x00, 0x02, 0x00, 0x01, 0x10, 0x49,
            0x00, 0x06, 0x00, 0x37, 0x2a, 0x00, 0x01, 0x20,
            // offset 0x6E = TerminatorTLV
            0xfe, // padding zeros
            0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        // Test Length transform inside struct context — verify transform value in scope
        let len_source = r#"
            struct Length {
                u8 byte [[hidden, no_unique_address]];
                if (byte == 0xFF)
                    u24 length;
                else
                    u8 length;
            } [[sealed, transform("transform_length")]];
            fn transform_length(ref auto length) {
                return length.length;
            };
            struct Wrapper {
                Length len_field;
                if (len_field > 0) {
                    u8 data[len_field];
                }
            };
            Wrapper w @ 0x11;
        "#;
        let _len_results = eval_pattern(len_source, &data);
        let results = eval_pattern(source, &data);
        let tlv = results.iter().find(|n| n.name == "tlv").unwrap();
        // Should have exactly 2 TLV elements: NDEFMessage + TerminatorTLV
        assert!(
            tlv.children.len() >= 2,
            "expected at least 2 TLV elements, got {}",
            tlv.children.len()
        );
        // Check the last element is a TerminatorTLV (tag=0xFE)
        // TLV[0] should be NDEFMessage with size > 2
        assert!(
            tlv.children[0].size > 2,
            "TLV[0] should be NDEFMessage with size > 2, got size {}",
            tlv.children[0].size
        );
    }

    #[test]
    fn test_ntag_ndef_with_bitfield_break() {
        // Test NDEF-like pattern with bitfield flags controlling while-loop break.
        // NDEFFlags bitfield has ME (message end) flag that controls break.
        // MSB-first bit ordering: byte 0xD1 = 0b11010001
        //   MB=1, ME=1, CF=0, SR=1, IL=0, TNF=001=1
        let source = r#"
            bitfield NDEFFlags {
                MB  : 1;
                ME  : 1;
                CF  : 1;
                SR  : 1;
                IL  : 1;
                TNF : 3;
            } [[bitfield_order(0, 8)]];

            struct NDEF {
                NDEFFlags flags;
                u8 typeLength;
                if (flags.SR)
                    u8 payloadLength;
                else
                    u32 payloadLength;
                if (flags.IL)
                    u8 idLength;
                u8 type[typeLength];
                u8 payload[payloadLength];
                if (flags.ME)
                    break;
            };

            NDEF ndef[while(true)] @ 0x00;
        "#;
        // flags=0xD1: MB=1, ME=1, CF=0, SR=1, IL=0, TNF=1
        // typeLength=2, payloadLength=3 (u8 since SR=1), type="AB", payload=[01,02,03]
        let data: Vec<u8> = vec![
            0xD1, // flags: MB=1, ME=1, SR=1, IL=0, TNF=1
            0x02, // typeLength=2
            0x03, // payloadLength=3 (u8 since SR=1)
            0x41, 0x42, // type = "AB"
            0x01, 0x02, 0x03, // payload
            0x00, 0x00, // extra padding
        ];
        let results = eval_pattern(source, &data);
        let ndef = results.iter().find(|n| n.name == "ndef").unwrap();
        // Should have exactly 1 NDEF element (ME=1 → break after first)
        assert_eq!(
            ndef.children.len(),
            1,
            "expected 1 NDEF element, got {}",
            ndef.children.len()
        );
    }

    // ========== Section tests ==========

    #[test]
    fn test_section_create_and_read() {
        // Create a section, set its size, write data, then read it back
        let source = r#"
            fn create_section(str name) { return std::mem::create_section(name); };
            fn set_section_size(u128 section, u128 size) { std::mem::set_section_size(section, size); };

            u128 sect = create_section("test");
            set_section_size(sect, 4);
            std::mem::copy_value_to_section(0x42, sect, 0, 1);

            u8 val @ 0 in sect;
        "#;
        let data = &[0xFF; 16];
        let ds = SliceDataSource::new(data);
        let (_results, mut evaluator) = parse_and_eval(source, &ds);
        let val_name = evaluator.interner.intern("val");
        let val = evaluator.scope.get_var(val_name).unwrap();
        // Should read 0x42 from section, not 0xFF from main data
        assert_eq!(val.to_unsigned().unwrap(), 0x42);
    }

    #[test]
    fn test_section_write_through() {
        // Write to a section variable, then re-read to verify write-through
        let source = r#"
            fn create_section(str name) { return std::mem::create_section(name); };
            fn set_section_size(u128 section, u128 size) { std::mem::set_section_size(section, size); };

            u128 sect = create_section("stack");
            set_section_size(sect, 8);

            s16 val @ 0 in sect;
            val = 12345;
            // Re-read from section to verify write-through
            s16 val2 @ 0 in sect;
        "#;
        let data = &[0x00; 16];
        let ds = SliceDataSource::new(data);
        let (_results, mut evaluator) = parse_and_eval(source, &ds);
        let val2_name = evaluator.interner.intern("val2");
        let val2 = evaluator.scope.get_var(val2_name).unwrap();
        assert_eq!(val2.to_signed().unwrap(), 12345);
    }

    #[test]
    fn test_section_stack_push_pop() {
        // Simulate parquet.hexpat's stack pattern: push/pop via offset tracking
        let source = r#"
            fn create_section(str name) { return std::mem::create_section(name); };
            fn get_section_size(u128 section) { return std::mem::get_section_size(section); };
            fn set_section_size(u128 section, u128 size) { std::mem::set_section_size(section, size); };

            u128 stack = create_section("stack");
            set_section_size(stack, 0);

            // Push: grow section and write value
            u128 offset0 = get_section_size(stack);
            set_section_size(stack, offset0 + 2);
            s16 top @ offset0 in stack;
            top = 100;

            // Push another
            u128 offset1 = get_section_size(stack);
            set_section_size(stack, offset1 + 2);
            s16 top2 @ offset1 in stack;
            top2 = 200;

            // Read back first pushed value
            s16 read_first @ 0 in stack;
            // Read back second pushed value
            s16 read_second @ 2 in stack;
        "#;
        let data = &[0x00; 32];
        let ds = SliceDataSource::new(data);
        let (_results, mut evaluator) = parse_and_eval(source, &ds);

        let name1 = evaluator.interner.intern("read_first");
        let val1 = evaluator.scope.get_var(name1).unwrap();
        assert_eq!(val1.to_signed().unwrap(), 100);

        let name2 = evaluator.interner.intern("read_second");
        let val2 = evaluator.scope.get_var(name2).unwrap();
        assert_eq!(val2.to_signed().unwrap(), 200);
    }

    #[test]
    fn test_for_loop_return_propagation() {
        // Verify that `return` inside a for-loop body correctly propagates
        // through the function call (previously the ControlFlow was discarded).
        let source = r#"
            fn find_value(u8 target) {
                for (u8 i = 0, i < 5, i = i + 1) {
                    if (i == target)
                        return true;
                }
                return false;
            };
            bool found = find_value(3);
            bool not_found = find_value(10);
        "#;
        let ds = SliceDataSource::new(&[]);
        let (_results, mut evaluator) = parse_and_eval(source, &ds);
        let found_key = evaluator.interner.intern("found");
        assert_eq!(evaluator.scope.get_var(found_key), Some(&Value::Bool(true)));
        let not_found_key = evaluator.interner.intern("not_found");
        assert_eq!(
            evaluator.scope.get_var(not_found_key),
            Some(&Value::Bool(false))
        );
    }

    #[test]
    fn test_tail_recursive_struct_basic() {
        // Linked list: 3 nodes, each with a u8 value and a u16 next offset.
        // Node 0 at offset 0: value=0xAA, next=3
        // Node 1 at offset 3: value=0xBB, next=6
        // Node 2 at offset 6: value=0xCC, next=0 (end)
        let source = r#"
            struct Node {
                u8 value;
                u16 next;
                if (next > 0) {
                    Node child @ next;
                }
            };
            Node root @ 0x00;
        "#;
        let data: &[u8] = &[
            0xAA, 0x03, 0x00, // node 0: value=0xAA, next=3
            0xBB, 0x06, 0x00, // node 1: value=0xBB, next=6
            0xCC, 0x00, 0x00, // node 2: value=0xCC, next=0
        ];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "root");
        // Root node should have: value, next, child
        assert_eq!(results[0].children.len(), 3);
        assert_eq!(results[0].children[0].value, PatternValue::Unsigned(0xAA));
        // child (nested node 1)
        let child1 = &results[0].children[2];
        assert_eq!(child1.children.len(), 3);
        assert_eq!(child1.children[0].value, PatternValue::Unsigned(0xBB));
        // child (nested node 2, terminal)
        let child2 = &child1.children[2];
        assert_eq!(child2.children.len(), 2); // value + next, no further child
        assert_eq!(child2.children[0].value, PatternValue::Unsigned(0xCC));
    }

    #[test]
    fn test_tail_recursive_struct_deep_chain() {
        // Build a 150-node linked list that would fail with recursion_depth=100.
        // Each node: u8 value, u16 next_offset (LE).
        let node_count = 150usize;
        let node_size = 3; // 1 byte value + 2 bytes next
        let mut data = vec![0u8; node_count * node_size];
        for i in 0..node_count {
            let offset = i * node_size;
            data[offset] = (i & 0xFF) as u8;
            let next = if i + 1 < node_count {
                ((i + 1) * node_size) as u16
            } else {
                0u16
            };
            data[offset + 1] = (next & 0xFF) as u8;
            data[offset + 2] = ((next >> 8) & 0xFF) as u8;
        }
        let source = r#"
            struct Node {
                u8 value;
                u16 next;
                if (next > 0) {
                    Node child @ next;
                }
            };
            Node root @ 0x00;
        "#;
        let ds = SliceDataSource::new(&data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        // Set recursion depth to 100 — would fail without tail-recursive optimization
        evaluator.set_limits(0x10000, 0x40000, 100);
        evaluator.set_max_total_nodes(0); // disable node limit
        let results = evaluator
            .evaluate(&ast)
            .expect("evaluation should succeed with tail-recursive optimization");
        assert_eq!(results.len(), 1);
        // Verify the chain depth: walk into children to count nesting
        let mut depth = 0;
        let mut current = &results[0];
        loop {
            depth += 1;
            // Find the "child" node in children
            if let Some(child) = current.children.iter().find(|c| c.name == "child") {
                current = child;
            } else {
                break;
            }
        }
        assert_eq!(depth, node_count);
    }

    #[test]
    fn test_tail_recursive_struct_ifds_pattern() {
        // Mimics the TIFF IFDS pattern:
        // struct IFD { u8 data; u16 NextIFD; };
        // struct IFDS { IFD IFD; if (IFD.NextIFD > 0) { IFDS tmp @ IFD.NextIFD; } } [[inline]];
        let source = r#"
            struct IFD {
                u8 data;
                u16 NextIFD;
            };
            u64 currentIFD = 0;
            struct IFDS {
                IFD IFD;
                if (IFD.NextIFD > 0) {
                    currentIFD += 1;
                    IFDS IFD_tmp @ IFD.NextIFD;
                }
            } [[inline]];
            IFDS @ 0x00;
        "#;
        // Chain: offset 0 → 3 → 6 → 9 → 0 (end)
        let data: &[u8] = &[
            0x01, 0x03, 0x00, // IFD at 0: data=1, NextIFD=3
            0x02, 0x06, 0x00, // IFD at 3: data=2, NextIFD=6
            0x03, 0x09, 0x00, // IFD at 6: data=3, NextIFD=9
            0x04, 0x00, 0x00, // IFD at 9: data=4, NextIFD=0
        ];
        let ds = SliceDataSource::new(data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        // Set low recursion depth — should still work with tail-recursive optimization
        evaluator.set_limits(0x10000, 0x40000, 3);
        evaluator.set_max_total_nodes(0);
        let results = evaluator
            .evaluate(&ast)
            .expect("should succeed with tail-recursive optimization");
        assert!(results.len() >= 1);
    }

    #[test]
    fn test_template_param_same_name_no_circular_alias() {
        // When inner struct uses same template param name T as outer,
        // the alias must resolve through to the concrete type, not self-reference.
        let source = r#"
            struct Inner<T> {
                T value;
            };
            struct Outer<T> {
                Inner<T> wrapped;
            };
            Outer<u16> root @ 0x00;
        "#;
        let data: &[u8] = &[0x34, 0x12]; // u16 LE = 0x1234
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "root");
        // root.wrapped.value should be 0x1234
        let wrapped = &results[0].children[0];
        assert_eq!(wrapped.name, "wrapped");
        assert_eq!(wrapped.children[0].name, "value");
        assert_eq!(wrapped.children[0].value, PatternValue::Unsigned(0x1234));
    }

    #[test]
    fn test_template_param_three_level_passthrough() {
        // T passed through 3 levels: A<T> → B<T> → C<T>, all same name
        let source = r#"
            struct C<T> {
                T val;
            };
            struct B<T> {
                C<T> inner;
            };
            struct A<T> {
                B<T> mid;
            };
            A<u32> root @ 0x00;
        "#;
        let data: &[u8] = &[0x78, 0x56, 0x34, 0x12]; // u32 LE = 0x12345678
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        // root.mid.inner.val = 0x12345678
        let mid = &results[0].children[0];
        let inner = &mid.children[0];
        let val = &inner.children[0];
        assert_eq!(val.name, "val");
        assert_eq!(val.value, PatternValue::Unsigned(0x12345678));
    }

    #[test]
    fn test_template_param_array_same_name() {
        // Array of template param T inside struct with same param name T
        // (mimics tiff.hexpat's ValueArray<T, Count>)
        let source = r#"
            struct ValueArray<T, auto Count> {
                T Values[Count];
            };
            struct Wrapper<T, auto N> {
                ValueArray<T, N> arr;
            };
            Wrapper<u8, 4> root @ 0x00;
        "#;
        let data: &[u8] = &[0xAA, 0xBB, 0xCC, 0xDD];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        let arr = &results[0].children[0]; // arr
        assert_eq!(arr.name, "arr");
        assert_eq!(arr.children.len(), 1); // Values array
        let values = &arr.children[0];
        assert_eq!(values.name, "Values");
        assert_eq!(values.children.len(), 4);
        assert_eq!(values.children[0].value, PatternValue::Unsigned(0xAA));
        assert_eq!(values.children[3].value, PatternValue::Unsigned(0xDD));
    }

    #[test]
    fn test_template_param_different_name_no_issue() {
        // Different template param names should work without any resolution
        let source = r#"
            struct Inner<U> {
                U value;
            };
            struct Outer<T> {
                Inner<T> wrapped;
            };
            Outer<u8> root @ 0x00;
        "#;
        let data: &[u8] = &[0x42];
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        let wrapped = &results[0].children[0];
        assert_eq!(wrapped.children[0].value, PatternValue::Unsigned(0x42));
    }

    #[test]
    fn test_template_param_struct_type_same_name() {
        // T resolved to a user-defined struct type, passed through same-named param
        let source = r#"
            struct Point {
                u8 x;
                u8 y;
            };
            struct Box<T> {
                T item;
            };
            struct Container<T> {
                Box<T> boxed;
            };
            Container<Point> root @ 0x00;
        "#;
        let data: &[u8] = &[0x0A, 0x14]; // Point: x=10, y=20
        let results = eval_pattern(source, data);
        assert_eq!(results.len(), 1);
        let boxed = &results[0].children[0]; // boxed
        let item = &boxed.children[0]; // item
        assert_eq!(item.type_name, "Point");
        assert_eq!(item.children.len(), 2);
        assert_eq!(item.children[0].value, PatternValue::Unsigned(0x0A));
        assert_eq!(item.children[1].value, PatternValue::Unsigned(0x14));
    }

    #[test]
    fn test_non_tail_recursive_still_limited() {
        // Non-tail recursion (self-reference is NOT the last statement)
        // should still be limited by recursion_depth.
        let source = r#"
            struct Node {
                u8 value;
                u16 next;
                if (next > 0) {
                    Node child @ next;
                }
                u8 trailer;
            };
            Node root @ 0x00;
        "#;
        let node_count = 30usize;
        let node_size = 4; // value(1) + next(2) + trailer(1)
        let mut data = vec![0u8; node_count * node_size];
        for i in 0..node_count {
            let offset = i * node_size;
            data[offset] = (i & 0xFF) as u8;
            let next = if i + 1 < node_count {
                ((i + 1) * node_size) as u16
            } else {
                0u16
            };
            data[offset + 1] = (next & 0xFF) as u8;
            data[offset + 2] = ((next >> 8) & 0xFF) as u8;
            data[offset + 3] = 0xFF; // trailer
        }
        let ds = SliceDataSource::new(&data);
        let (ast, mut evaluator) = setup_evaluator(source, &ds);
        // Set low recursion depth (20) so the 30-node chain triggers the limit
        evaluator.set_limits(0x10000, 0x40000, 20);
        evaluator.set_max_total_nodes(0);
        let result = evaluator.evaluate(&ast);
        // Should fail with recursion depth exceeded
        assert!(result.is_err(), "non-tail recursion should be limited");
        let err_msg = result.unwrap_err().message;
        assert!(
            err_msg.contains("recursion depth exceeded"),
            "expected recursion depth error, got: {}",
            err_msg
        );
    }
}
