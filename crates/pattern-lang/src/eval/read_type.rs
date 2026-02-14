// Binary data reading methods for the evaluator
use super::*;

/// Minimum element count for bulk/lazy read (matches LAZY_PROPAGATION_THRESHOLD).
/// Arrays >= this size already skip scope propagation, so bulk read is safe.
const BULK_READ_THRESHOLD: u128 = 64;

/// Common parameters for array reading functions.
struct ArrayReadContext<'a> {
    elem_ty: &'a TypeExpr,
    name: &'a str,
    offset: u64,
    endian: Endianness,
    attributes: PatternAttributes,
    type_name: &'a str,
}

/// Info about a tail-recursive struct pattern.
/// A struct is tail-recursive when its last effective statement is a conditional
/// (or unconditional) self-referencing DataPlacement at an explicit offset.
struct TailRecursiveInfo {
    /// Index of the tail statement in the body
    tail_stmt_index: usize,
    /// Guard condition (None = unconditional tail recursion)
    condition: Option<Expr>,
    /// Statements inside the if-body that execute before the tail placement
    pre_tail_stmts: Vec<Stmt>,
    /// The offset expression for the next iteration (the `@ expr`)
    offset_expr: Expr,
    /// Member name from the DataPlacement (e.g., "child", "IFD_tmp")
    member_name: Name,
}

impl<'a> Evaluator<'a> {
    /// Register template arguments in current scope.
    /// For Type args: define type alias + resolve enum member as value.
    /// For Expr args: eval as value, or fallback to type alias if ident names a type.
    pub(crate) fn register_template_args(
        &mut self,
        template_params: &[Name],
        args: &[TemplateArg],
    ) -> Result<(), EvalError> {
        for (param, arg) in template_params.iter().zip(args.iter()) {
            match arg {
                TemplateArg::Type(ty_arg) => {
                    // Resolve aliases to avoid circular self-references when inner
                    // and outer template params share the same name (e.g.,
                    // ValueArray<T, Count> inside a struct that also has param T).
                    let resolved_ty = if let TypeExprKind::Named(path) = &ty_arg.kind {
                        if path.len() == 1 && path[0] == *param {
                            if let Some(td) = self.scope.get_type(path[0]) {
                                if let TypeDef::Alias {
                                    ty: Some(actual), ..
                                } = &*td
                                {
                                    actual.clone()
                                } else {
                                    ty_arg.clone()
                                }
                            } else {
                                ty_arg.clone()
                            }
                        } else {
                            ty_arg.clone()
                        }
                    } else {
                        ty_arg.clone()
                    };
                    self.scope.define_type(
                        *param,
                        TypeDef::Alias {
                            name: *param,
                            template_params: vec![],
                            ty: Some(resolved_ty),
                            attrs: vec![],
                        },
                    );
                    // Also try to resolve as a value (for auto params)
                    if let TypeExprKind::Named(path) = &ty_arg.kind {
                        if path.len() >= 2 {
                            let member_str =
                                self.interner.resolve(path[path.len() - 1]).to_string();
                            let tname = self.resolve_path(&path[..path.len() - 1]);
                            if let Ok(val) =
                                self.resolve_enum_member(&tname, &member_str, ty_arg.span)
                            {
                                self.scope.define_var(*param, val, true);
                            }
                        }
                    }
                }
                TemplateArg::Expr(expr) => {
                    // Try eval as expression first; if it fails and the
                    // identifier names a known type, register as type alias
                    match self.eval_expr(expr) {
                        Ok(val) => {
                            self.scope.define_var(*param, val, true);
                        }
                        Err(_)
                            if matches!(&expr.kind, ExprKind::Ident(n) if {
                                self.scope.get_type(*n).is_some()
                                    || BuiltinType::from_str(self.interner.resolve(*n)).is_some()
                                    || parse_arbitrary_int_type(self.interner.resolve(*n)).is_some()
                            }) =>
                        {
                            let n = match &expr.kind {
                                ExprKind::Ident(n) => *n,
                                _ => unreachable!(),
                            };
                            let n_str = self.interner.resolve(n);
                            let ty_kind = if let Some(bt) = BuiltinType::from_str(n_str) {
                                TypeExprKind::Builtin(bt)
                            } else if n == *param {
                                // Same name: resolve alias to avoid circular reference
                                if let Some(td) = self.scope.get_type(n) {
                                    if let TypeDef::Alias {
                                        ty: Some(actual), ..
                                    } = &*td
                                    {
                                        actual.kind.clone()
                                    } else {
                                        TypeExprKind::Named(vec![n])
                                    }
                                } else {
                                    TypeExprKind::Named(vec![n])
                                }
                            } else {
                                TypeExprKind::Named(vec![n])
                            };
                            self.scope.define_type(
                                *param,
                                TypeDef::Alias {
                                    name: *param,
                                    template_params: vec![],
                                    ty: Some(TypeExpr {
                                        kind: ty_kind,
                                        span: expr.span,
                                    }),
                                    attrs: vec![],
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    }
                }
            }
        }
        Ok(())
    }

    /// Push a new scope and register `this`, `parent`, and enclosing vars
    /// for struct/union body evaluation. Caller must call scope.pop() when done.
    fn setup_struct_scope(&mut self, offset: u64) {
        let this_key = self.interner.intern("this");
        let enclosing_vars = self.scope.enclosing_struct_vars(this_key, &self.interner);
        self.scope.push();
        self.scope
            .define_var(this_key, Value::Unsigned(offset as u128), false);
        let parent_offset = if let Some(this_val) = enclosing_vars.iter().find(|(k, _)| k == "this")
        {
            match &this_val.1 {
                Value::Unsigned(v) => *v as u64,
                _ => offset,
            }
        } else {
            offset
        };
        let parent_key = self.interner.intern("parent");
        self.scope.define_var(
            parent_key,
            Value::SizedRef {
                offset: parent_offset,
                size: 0,
            },
            false,
        );
        for (vname, vval) in &enclosing_vars {
            if vname.starts_with("parent.parent") {
                continue;
            }
            let dotted = format!("parent.{}", vname);
            let dotted_key = self.interner.intern(&dotted);
            self.scope.define_var(dotted_key, vval.clone(), false);
            if vname.starts_with("__addr_") {
                let addr_dotted = format!("__addr_parent.{}", &vname["__addr_".len()..]);
                let addr_dotted_key = self.interner.intern(&addr_dotted);
                self.scope.define_var(addr_dotted_key, vval.clone(), false);
            }
        }
    }

    /// Read a pointer declaration and return (node, pointer_size_in_bytes).
    /// Registers the variable in scope as a SizedRef pointing to the target.
    /// Also evaluates the target type at the pointed address so that member access
    /// on the pointer target works (e.g., `ptr.member`).
    pub(crate) fn read_pointer(
        &mut self,
        target_ty: &TypeExpr,
        ptr_size_ty: &TypeExpr,
        name: &str,
        offset: u64,
        attrs: &[Attribute],
    ) -> Result<(PatternNode, u64), EvalError> {
        let ptr_endian = self.resolve_endianness(ptr_size_ty);
        let (ptr_node, ptr_bytes) = self.read_type(ptr_size_ty, name, offset, ptr_endian, &[])?;
        let pointed_offset = match &ptr_node.value {
            PatternValue::Unsigned(v) => *v as u64,
            PatternValue::Signed(v) => *v as u64,
            _ => 0,
        };

        // Try to evaluate the target type at the pointed address.
        // This makes pointer target members accessible via member access (e.g., ptr.field).
        // Save/restore offset so target evaluation doesn't affect auto-advance.
        let save_offset = self.offset;
        let target_endian = self.resolve_endianness(target_ty);
        let target_result = self.read_type(target_ty, name, pointed_offset, target_endian, attrs);
        self.offset = save_offset;

        let name_key = self.interner.intern(name);
        let mut node = PatternNode::new(
            name,
            &format!("{}*", type_name_of(target_ty, &self.interner)),
            offset,
            ptr_bytes,
            PatternValue::Pointer {
                address: pointed_offset,
            },
        );
        node.attributes = self.eval_attributes(attrs)?;

        match target_result {
            Ok((target_node, _)) => {
                self.scope.define_var(
                    name_key,
                    Value::SizedRef {
                        offset: pointed_offset,
                        size: target_node.size,
                    },
                    false,
                );
                // Store target children in pointer node for propagate_children
                node.children = target_node.children.clone();
                // Push target node to results so SizedRef-based lookup can find it
                self.results.push(target_node);
            }
            Err(_) => {
                // Target read failed (OOB, etc.) — register with estimated size
                let target_size = self.type_size(target_ty).unwrap_or(0);
                self.scope.define_var(
                    name_key,
                    Value::SizedRef {
                        offset: pointed_offset,
                        size: target_size,
                    },
                    false,
                );
            }
        }

        Ok((node, ptr_bytes))
    }

    /// Read a typed value from binary data
    pub(crate) fn read_type(
        &mut self,
        ty: &TypeExpr,
        name: &str,
        offset: u64,
        endian: Endianness,
        attrs: &[Attribute],
    ) -> Result<(PatternNode, u64), EvalError> {
        self.total_node_count += 1;
        self.read_type_count += 1;
        if let Some(ref counter) = self.shared_read_type_count {
            if self.read_type_count & 0x3F == 0 {
                counter.store(self.read_type_count, Ordering::Relaxed);
            }
        }
        self.check_resource_limits(ty.span)?;

        let attributes = self.eval_attributes(attrs)?;

        let span = ty.span;
        match &ty.kind {
            TypeExprKind::Builtin(builtin) => self
                .read_builtin(builtin, name, offset, endian, attributes)
                .map_err(|e| self.resolve_error(e.with_span_if_none(span))),
            TypeExprKind::Named(path) => {
                let type_name = self.resolve_path(path);
                self.read_named_type(&type_name, name, offset, endian, attributes)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(span)))
            }
            TypeExprKind::ArbitraryInt { signed, bits } => self
                .read_arbitrary_int(*signed, *bits, name, offset, endian, attributes)
                .map_err(|e| self.resolve_error(e.with_span_if_none(span))),
            TypeExprKind::Endian(end, inner) => {
                let inner_endian = match end {
                    Endianness::Little => Endianness::Little,
                    Endianness::Big => Endianness::Big,
                };
                self.read_type(inner, name, offset, inner_endian, attrs)
            }
            TypeExprKind::Array(elem_ty, size) => self
                .read_array(elem_ty, size, name, offset, endian, attributes)
                .map_err(|e| self.resolve_error(e.with_span_if_none(span))),
            TypeExprKind::Template(path, args) => {
                let type_name = self.resolve_path(path);
                let type_name_key = self.interner.intern(&type_name);
                // Try to resolve template aliases and structs
                if let Some(typedef) = self.scope.get_type(type_name_key) {
                    match &*typedef {
                        TypeDef::Alias {
                            template_params,
                            ty: Some(alias_ty),
                            ..
                        } if !template_params.is_empty() => {
                            if args.len() == template_params.len() {
                                let substituted = substitute_type(alias_ty, template_params, args);
                                return self.read_type(&substituted, name, offset, endian, attrs);
                            }
                        }
                        TypeDef::Struct {
                            template_params, ..
                        }
                        | TypeDef::Union {
                            template_params, ..
                        }
                        | TypeDef::Bitfield {
                            template_params, ..
                        } if !template_params.is_empty() => {
                            self.scope.push();
                            self.register_template_args(template_params, args)?;
                            let result = self
                                .read_named_type(&type_name, name, offset, endian, attributes)
                                .map_err(|e| self.resolve_error(e.with_span_if_none(span)));
                            self.scope.pop();
                            return result;
                        }
                        _ => {}
                    }
                }
                self.read_named_type(&type_name, name, offset, endian, attributes)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(span)))
            }
            TypeExprKind::Auto => {
                Err(self.make_error("cannot evaluate 'auto' type directly", span))
            }
            TypeExprKind::Pointer(pointee_ty, size_ty) => {
                // Read the pointer value
                let (ptr_node, ptr_size) = self.read_type(size_ty, name, offset, endian, attrs)?;
                let ptr_val = match &ptr_node.value {
                    PatternValue::Unsigned(v) => *v as u64,
                    _ => {
                        return Err(self.make_error("pointer value must be unsigned integer", span));
                    }
                };

                let node = PatternNode {
                    name: name.to_string(),
                    type_name: format!("*{}", type_name_of(pointee_ty, &self.interner)),
                    offset,
                    size: ptr_size,
                    value: PatternValue::Pointer { address: ptr_val },
                    children: Vec::new(),
                    attributes,
                };
                Ok((node, ptr_size))
            }
            TypeExprKind::Padding => {
                // Single byte of padding
                let node = PatternNode::new(name, "padding", offset, 1, PatternValue::Padding(1));
                Ok((node, 1))
            }
        }
    }

    /// Read an arbitrary-width integer type (u24, s48, etc.) from binary data
    pub(crate) fn read_arbitrary_int(
        &self,
        signed: bool,
        bits: u64,
        name: &str,
        offset: u64,
        endian: Endianness,
        attributes: PatternAttributes,
    ) -> Result<(PatternNode, u64), EvalError> {
        let byte_size = bits.div_ceil(8);
        let bytes = self.read_data_bytes(offset, byte_size)?;
        let raw = bytes_to_unsigned(&bytes, endian);
        let mask = if bits >= 128 {
            u128::MAX
        } else {
            (1u128 << bits) - 1
        };
        let masked = raw & mask;
        let value = if signed {
            let sign_bit = 1u128 << (bits - 1);
            if masked & sign_bit != 0 {
                let extended = masked | !mask;
                PatternValue::Signed(extended as i128)
            } else {
                PatternValue::Signed(masked as i128)
            }
        } else {
            PatternValue::Unsigned(masked)
        };
        let type_name = format!("{}{}", if signed { "s" } else { "u" }, bits);
        let mut node = PatternNode::new(name, &type_name, offset, byte_size, value);
        node.attributes = attributes;
        Ok((node, byte_size))
    }

    /// Read a builtin type from binary data
    pub(crate) fn read_builtin(
        &self,
        builtin: &BuiltinType,
        name: &str,
        offset: u64,
        endian: Endianness,
        attributes: PatternAttributes,
    ) -> Result<(PatternNode, u64), EvalError> {
        let size = builtin
            .size()
            .ok_or_else(|| EvalError::new("unsized builtin type"))?;
        let bytes = self.read_data_bytes(offset, size)?;
        let type_name = builtin_type_name(builtin);

        macro_rules! read_int {
            ($bytes:expr, $endian:expr, $size:literal, $int_ty:ty, $variant:ident) => {{
                let arr = bytes_to_array::<$size>($bytes)?;
                let val = match $endian {
                    Endianness::Little => <$int_ty>::from_le_bytes(arr),
                    Endianness::Big => <$int_ty>::from_be_bytes(arr),
                };
                PatternValue::$variant(val as _)
            }};
        }

        let value = match builtin {
            BuiltinType::U8 => PatternValue::Unsigned(bytes[0] as u128),
            BuiltinType::U16 => read_int!(&bytes, endian, 2, u16, Unsigned),
            BuiltinType::U32 => read_int!(&bytes, endian, 4, u32, Unsigned),
            BuiltinType::U64 => read_int!(&bytes, endian, 8, u64, Unsigned),
            BuiltinType::U128 => read_int!(&bytes, endian, 16, u128, Unsigned),
            BuiltinType::S8 => PatternValue::Signed(bytes[0] as i8 as i128),
            BuiltinType::S16 => read_int!(&bytes, endian, 2, i16, Signed),
            BuiltinType::S32 => read_int!(&bytes, endian, 4, i32, Signed),
            BuiltinType::S64 => read_int!(&bytes, endian, 8, i64, Signed),
            BuiltinType::S128 => read_int!(&bytes, endian, 16, i128, Signed),
            BuiltinType::Float => read_int!(&bytes, endian, 4, f32, Float),
            BuiltinType::Double => {
                let arr = bytes_to_array::<8>(&bytes)?;
                let val = match endian {
                    Endianness::Little => f64::from_le_bytes(arr),
                    Endianness::Big => f64::from_be_bytes(arr),
                };
                PatternValue::Float(val)
            }
            BuiltinType::Char => PatternValue::Char(bytes[0] as char),
            BuiltinType::Char16 => {
                let arr = bytes_to_array::<2>(&bytes)?;
                let val = match endian {
                    Endianness::Little => u16::from_le_bytes(arr),
                    Endianness::Big => u16::from_be_bytes(arr),
                };
                PatternValue::Char(char::from_u32(val as u32).unwrap_or('\u{FFFD}'))
            }
            BuiltinType::Bool => PatternValue::Bool(bytes[0] != 0),
            BuiltinType::Str => {
                return Err(EvalError::new("str type requires explicit size"));
            }
        };

        let mut node = PatternNode::new(name, type_name, offset, size, value);
        node.attributes = attributes;
        Ok((node, size))
    }

    /// Read struct body fields into children, advancing current_offset
    pub(crate) fn read_struct_body(
        &mut self,
        body: &[Stmt],
        current_offset: &mut u64,
        children: &mut Vec<PatternNode>,
        endian: Endianness,
    ) -> Result<(), EvalError> {
        let prev_in_struct_body = self.in_struct_body;
        self.in_struct_body = true;
        let result = self.read_struct_body_inner(body, current_offset, children, endian);
        self.in_struct_body = prev_in_struct_body;
        result
    }

    pub(crate) fn read_struct_body_inner(
        &mut self,
        body: &[Stmt],
        current_offset: &mut u64,
        children: &mut Vec<PatternNode>,
        endian: Endianness,
    ) -> Result<(), EvalError> {
        for member_stmt in body {
            if let StmtKind::LocalVar {
                name: member_name,
                init: member_init,
                ty: member_ty,
                ..
            } = &member_stmt.kind
            {
                // Local variable with initializer (no data read)
                self.offset = *current_offset;
                let val = self.eval_expr(member_init)?;
                self.scope.define_var(*member_name, val.clone(), false);

                // Also create a virtual PatternNode so parent.xxx.localvar is accessible
                let member_name_str = self.interner.resolve(*member_name).to_string();
                let type_name_str = type_name_of(member_ty, &self.interner);
                let pval = match &val {
                    Value::Unsigned(v) => PatternValue::Unsigned(*v),
                    Value::Signed(v) => PatternValue::Signed(*v),
                    Value::Bool(b) => PatternValue::Unsigned(if *b { 1 } else { 0 }),
                    Value::String(s) => PatternValue::String(s.clone()),
                    Value::Float(f) => PatternValue::Float(*f),
                    _ => PatternValue::Unsigned(0),
                };
                let virtual_node =
                    PatternNode::new(member_name_str, type_name_str, *current_offset, 0, pval);
                children.push(virtual_node);
            } else if let StmtKind::DataPlacement(d) = &member_stmt.kind
            {
                // Sync $ with struct's sequential position
                self.offset = *current_offset;

                // Determine offset: explicit placement or auto-advance
                let member_offset = if let Some(p) = &d.offset {
                    let val = self.eval_expr(p)?;
                    val.to_unsigned()
                        .map_err(|e| self.resolve_error(e.with_span_if_none(member_stmt.span)))?
                        as u64
                } else {
                    *current_offset
                };

                // Handle pointer declarations: `Type *name : SizeType;`
                let member_name_str = self.interner.resolve(d.name).to_string();
                if let Some(ptr_size_ty) = &d.pointer_size {
                    let (node, ptr_bytes) = self.read_pointer(
                        &d.ty,
                        ptr_size_ty,
                        &member_name_str,
                        member_offset,
                        &d.attrs,
                    )?;
                    if d.offset.is_none() {
                        *current_offset += ptr_bytes;
                    }
                    // Propagate pointer target children for dotted name lookups
                    // (e.g., "ptr.field" where ptr is a pointer to a struct with "field")
                    propagate_children(
                        &mut self.scope,
                        &mut self.interner,
                        &member_name_str,
                        &node.children,
                    );
                    children.push(node);
                    continue;
                }

                let (mut child_node, child_size) = self.read_type(
                    &d.ty,
                    &member_name_str,
                    member_offset,
                    endian,
                    &d.attrs,
                )?;
                let mut member_val = value_from_node(&child_node);
                // Apply field-level transform attribute (e.g., [[transform("parse_hex")]])
                if let Some(transformed) =
                    self.apply_transform(&d.attrs, &member_val, d.ty.span)?
                {
                    member_val = transformed;
                }
                // Apply type-level transform attributes (struct/union definition + using aliases).
                // Struct/union transforms (e.g., struct Length { } [[transform("...")]])
                // convert the SizedRef to a scalar value needed by downstream code
                // (e.g., `if (length > 0)` or `u8 value[length]`).
                if matches!(member_val, Value::SizedRef { .. }) {
                    // Try struct/union definition attrs first
                    let mut type_attrs = self.get_type_attrs(&d.ty);
                    // Fall back to using-alias attrs
                    if type_attrs.is_empty() {
                        type_attrs = self.get_alias_type_attrs(&d.ty);
                    }
                    if !type_attrs.is_empty() {
                        // Temporarily push node to results so SizedRef lookups work in transform fn.
                        // call_fn_by_name uses find_node_by_offset_rev (reverse search) so it
                        // prefers our just-pushed node over any existing node at the same offset.
                        self.results.push(child_node.clone());
                        let transform_result =
                            self.apply_transform(&type_attrs, &member_val, d.ty.span);
                        self.results.pop();
                        if let Some(transformed) = transform_result? {
                            member_val = transformed;
                        }
                    }
                }
                // Update child_node.value to reflect the transformed value so that
                // propagate_children / node tree lookups return the correct value.
                if let Some(pval) = value_to_pattern_value(&member_val) {
                    if matches!(
                        child_node.value,
                        PatternValue::Struct | PatternValue::Union | PatternValue::Bitfield
                    ) {
                        child_node.value = pval;
                    }
                }
                // For non-addressable values (e.g. char[] stored as String),
                // also store a SizedRef under __addr_<name> so addressof() can resolve the offset.
                if matches!(member_val, Value::String(_) | Value::Array(_)) {
                    let addr_key_str = format!("__addr_{}", member_name_str);
                    let addr_key = self.interner.intern(&addr_key_str);
                    self.scope.define_var(
                        addr_key,
                        Value::SizedRef {
                            offset: child_node.offset,
                            size: child_node.size,
                        },
                        false,
                    );
                }
                self.scope.define_var(d.name, member_val, false);
                // Propagate children with dotted names
                propagate_children(
                    &mut self.scope,
                    &mut self.interner,
                    &member_name_str,
                    &child_node.children,
                );

                // no_unique_address: member does not advance struct offset
                let is_no_unique_addr = d.attrs
                    .iter()
                    .any(|a| self.interner.resolve(a.name) == "no_unique_address");
                if d.offset.is_none() && !is_no_unique_addr {
                    *current_offset += child_size;
                }
                children.push(child_node);
            } else if let StmtKind::AnonymousPlacement(d) = &member_stmt.kind
            {
                // Anonymous type member: `Type;` or `Type @ offset;`
                let member_offset = if let Some(p) = &d.placement {
                    let val = self.eval_expr(p)?;
                    val.to_unsigned()
                        .map_err(|e| self.resolve_error(e.with_span_if_none(member_stmt.span)))?
                        as u64
                } else {
                    *current_offset
                };
                self.offset = member_offset;
                let anon_name = format!("_{}", type_name_of(&d.ty, &self.interner));
                let (child_node, child_size) =
                    self.read_type(&d.ty, &anon_name, member_offset, endian, &d.attrs)?;
                // Propagate fields from anonymous member into current scope
                for child in &child_node.children {
                    let child_key = self.interner.intern(&child.name);
                    self.scope
                        .define_var(child_key, value_from_node(child), false);
                    propagate_children(
                        &mut self.scope,
                        &mut self.interner,
                        &child.name,
                        &child.children,
                    );
                }
                if d.placement.is_none() {
                    *current_offset += child_size;
                }
                children.push(child_node);
            } else {
                // Sync offset before control flow and capture results
                let saved_results_len = self.results.len();
                self.offset = *current_offset;
                let cf = self.eval_stmt(member_stmt)?;
                *current_offset = self.offset;
                // A `break` inside a struct body sets a flag that propagates
                // up to the enclosing while-loop array (ImHex convention).
                if matches!(cf, ControlFlow::Break) {
                    self.break_signaled = true;
                    break;
                }
                // Capture any results produced by control-flow body
                while self.results.len() > saved_results_len {
                    let node = self.results.remove(saved_results_len);
                    let node_key = self.interner.intern(&node.name);
                    self.scope
                        .define_var(node_key, value_from_node(&node), false);
                    // For anonymous type placements (name starts with '_'),
                    // propagate child fields directly into scope (like AnonymousPlacement handler)
                    if node.name.starts_with('_') {
                        for child in &node.children {
                            let child_key = self.interner.intern(&child.name);
                            self.scope
                                .define_var(child_key, value_from_node(child), false);
                            propagate_children(
                                &mut self.scope,
                                &mut self.interner,
                                &child.name,
                                &child.children,
                            );
                        }
                    }
                    propagate_children(
                        &mut self.scope,
                        &mut self.interner,
                        &node.name,
                        &node.children,
                    );
                    children.push(node);
                }
            }
        }
        Ok(())
    }

    /// Read parent struct fields (recursive for multi-level inheritance)
    pub(crate) fn read_parent_fields(
        &mut self,
        parent_name: Name,
        parent_template_args: &[TemplateArg],
        current_offset: &mut u64,
        children: &mut Vec<PatternNode>,
        endian: Endianness,
    ) -> Result<(), EvalError> {
        if let Some(parent_td) = self.scope.get_type(parent_name) {
            if let TypeDef::Struct {
                parent: grandparent,
                template_params,
                body,
                ..
            } = &*parent_td
            {
                // Push scope for parent template args
                let has_template_args =
                    !parent_template_args.is_empty() && !template_params.is_empty();
                if has_template_args {
                    self.scope.push();
                    self.register_template_args(&template_params, parent_template_args)?;
                }
                // Read grandparent fields first
                if let Some((gp_name, gp_args)) = &grandparent {
                    self.read_parent_fields(*gp_name, gp_args, current_offset, children, endian)?;
                }
                // Read parent body fields
                self.read_struct_body(&body, current_offset, children, endian)?;
                if has_template_args {
                    self.scope.pop();
                }
            }
        }
        Ok(())
    }

    /// Detect MSB-first ordering from a bitfield_order attribute expression.
    /// Handles `BitfieldOrder::MostToLeastSignificant` and `BitfieldOrder::LeftToRight`
    /// even when the BitfieldOrder enum is not resolvable in the current scope.
    fn is_msb_first_from_expr(interner: &StringInterner, expr: &Expr) -> bool {
        if let ExprKind::ScopedIdent(path) = &expr.kind {
            if let Some(last) = path.last() {
                let name = interner.resolve(*last);
                return matches!(name, "MostToLeastSignificant" | "LeftToRight" | "MSB");
            }
        }
        false // default: LSB-first
    }

    /// Extract bit values from binary data for a bitfield field.
    ///
    /// `offset`: byte offset of the bitfield in data
    /// `bit_offset`: current bit position within the bitfield
    /// `width`: number of bits for this field
    /// `msb_first`: true for MostToLeastSignificant ordering
    /// `group_size`: bit group size for MSB-first ordering (e.g. 8)
    fn extract_bitfield_value(
        &self,
        offset: u64,
        bit_offset: u64,
        width: u64,
        msb_first: bool,
        group_size: u64,
    ) -> Result<u128, EvalError> {
        if width == 0 {
            return Ok(0);
        }
        if msb_first && group_size > 0 {
            // MSB-first: first logical bit maps to highest physical bit in group
            let mut value: u128 = 0;
            for i in 0..width {
                let logical_bit = bit_offset + i;
                let group_idx = logical_bit / group_size;
                let bit_in_group = logical_bit % group_size;
                let physical_bit_in_group = group_size - 1 - bit_in_group;
                let absolute_bit = group_idx * group_size + physical_bit_in_group;
                let byte_idx = absolute_bit / 8;
                let bit_in_byte = absolute_bit % 8;
                let byte_val = self.read_data_bytes(offset + byte_idx, 1)?[0];
                let bit = ((byte_val >> bit_in_byte) & 1) as u128;
                // First logical bit is MSB of the result value
                value |= bit << (width - 1 - i);
            }
            Ok(value)
        } else {
            // LSB-first (default): extract bits from least significant upward
            let byte_start = bit_offset / 8;
            let bit_in_start_byte = bit_offset % 8;
            let needed_bytes = (bit_in_start_byte + width + 7) / 8;
            let bytes = self.read_data_bytes(offset + byte_start, needed_bytes)?;
            let raw = bytes_to_unsigned(&bytes, Endianness::Little);
            let mask = if width >= 128 {
                u128::MAX
            } else {
                (1u128 << width) - 1
            };
            Ok((raw >> bit_in_start_byte) & mask)
        }
    }

    /// Evaluate a statement inside a bitfield body (handles BitfieldFieldStmt, if/while, etc.)
    pub(crate) fn eval_bitfield_stmt(
        &mut self,
        stmt: &Stmt,
        offset: u64,
        total_bits: &mut u64,
        children: &mut Vec<PatternNode>,
        msb_first: bool,
        group_size: u64,
    ) -> Result<(), EvalError> {
        match &stmt.kind {
            StmtKind::If(d) => {
                let cond_val = self.eval_expr(&d.cond)?;
                let body = if cond_val
                    .to_bool()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(stmt.span)))?
                {
                    Some(&d.then_body)
                } else {
                    d.else_body.as_ref()
                };
                if let Some(stmts) = body {
                    for s in stmts {
                        self.eval_bitfield_stmt(
                            s, offset, total_bits, children, msb_first, group_size,
                        )?;
                    }
                }
            }
            StmtKind::BitfieldFieldStmt { name, width, .. } => {
                let width_val = self.eval_expr(width)?;
                let bits = width_val
                    .to_unsigned()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(stmt.span)))?
                    as u64;
                // Read actual bit values from binary data
                let value =
                    self.extract_bitfield_value(offset, *total_bits, bits, msb_first, group_size)?;
                let name_str = self.interner.resolve(*name);
                self.scope.define_var(*name, Value::Unsigned(value), false);
                *total_bits = total_bits.saturating_add(bits);
                children.push(PatternNode::new(
                    name_str,
                    "bitfield",
                    offset,
                    0,
                    PatternValue::Unsigned(value),
                ));
            }
            _ => {
                // Other statements (LocalVar, etc.): evaluate and capture any virtual nodes
                let saved_results_len = self.results.len();
                self.eval_stmt(stmt)?;
                // Capture virtual nodes from LocalVar promotion
                while self.results.len() > saved_results_len {
                    let node = self.results.remove(saved_results_len);
                    let node_key = self.interner.intern(&node.name);
                    self.scope
                        .define_var(node_key, value_from_node(&node), false);
                    children.push(node);
                }
            }
        }
        Ok(())
    }

    /// Detect whether a struct body ends with a tail-recursive self-reference.
    /// Returns `Some(TailRecursiveInfo)` when the pattern matches.
    fn detect_tail_recursive_struct(
        &self,
        type_name: &str,
        body: &[Stmt],
    ) -> Option<TailRecursiveInfo> {
        let last_idx = body.len().checked_sub(1)?;
        let last_stmt = &body[last_idx];

        match &last_stmt.kind {
            // Pattern 1: `if (cond) { ...; SameType name @ offset; }`
            StmtKind::If(d) if d.else_body.is_none() => {
                let inner_last = d.then_body.last()?;
                if let StmtKind::DataPlacement(dp) = &inner_last.kind
                {
                    if let Some(ref offset_expr) = dp.offset {
                        let tn = type_name_of(&dp.ty, &self.interner);
                        if tn == type_name {
                            let pre_stmts = d.then_body[..d.then_body.len() - 1].to_vec();
                            return Some(TailRecursiveInfo {
                                tail_stmt_index: last_idx,
                                condition: Some(d.cond.clone()),
                                pre_tail_stmts: pre_stmts,
                                offset_expr: offset_expr.clone(),
                                member_name: dp.name,
                            });
                        }
                    }
                }
                None
            }
            // Pattern 2: `SameType name @ offset;` directly at body end
            StmtKind::DataPlacement(d) => {
                if let Some(ref offset_expr) = d.offset {
                    let tn = type_name_of(&d.ty, &self.interner);
                    if tn == type_name {
                        Some(TailRecursiveInfo {
                            tail_stmt_index: last_idx,
                            condition: None,
                            pre_tail_stmts: vec![],
                            offset_expr: offset_expr.clone(),
                            member_name: d.name,
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Evaluate tail-recursive continuation: condition + pre-tail stmts + offset.
    /// Returns Some(offset) to continue, None to stop.
    fn eval_tail_next_offset(&mut self, tail_info: &TailRecursiveInfo) -> Option<u64> {
        let should_continue = match &tail_info.condition {
            Some(cond) => self
                .eval_expr(cond)
                .ok()
                .and_then(|v| v.to_bool().ok())
                .unwrap_or(false),
            None => true,
        };
        if !should_continue {
            return None;
        }
        for s in &tail_info.pre_tail_stmts {
            if self.eval_stmt(s).is_err() {
                return None;
            }
        }
        match self.eval_expr(&tail_info.offset_expr) {
            Ok(val) => {
                let off = val.to_unsigned().unwrap_or(0) as u64;
                if off == 0 {
                    None
                } else {
                    Some(off)
                }
            }
            Err(_) => None,
        }
    }

    /// Read a tail-recursive struct as a loop, avoiding recursion_depth limits.
    /// Produces the same nested node tree as the recursive version.
    fn read_struct_tail_recursive(
        &mut self,
        type_name: &str,
        name: &str,
        offset: u64,
        endian: Endianness,
        attributes: PatternAttributes,
        parent: &Option<(Name, Vec<TemplateArg>)>,
        body: &[Stmt],
        tail_info: TailRecursiveInfo,
    ) -> Result<(PatternNode, u64), EvalError> {
        let non_tail_body = &body[..tail_info.tail_stmt_index];
        let mut current_offset = offset;
        let mut node_stack: Vec<PatternNode> = Vec::new();

        loop {
            // Resource limit check (cancellation, total nodes)
            self.check_resource_limits(Span::dummy())?;

            // Set up scope for this iteration
            self.setup_struct_scope(current_offset);
            let mut children = Vec::new();
            let mut iter_offset = current_offset;

            // Read parent fields
            if let Some((parent_name, parent_args)) = parent {
                self.read_parent_fields(
                    *parent_name,
                    parent_args,
                    &mut iter_offset,
                    &mut children,
                    endian,
                )?;
            }

            // Read body up to (not including) the tail statement
            self.read_struct_body(non_tail_body, &mut iter_offset, &mut children, endian)?;

            // Evaluate tail condition + pre-tail stmts + offset to determine next iteration
            let next_offset = self.eval_tail_next_offset(&tail_info);

            // Common cleanup: scope.pop + build node + push to stack
            self.scope.pop();
            let iter_size = iter_offset.saturating_sub(current_offset);
            let mut node = PatternNode::new(
                name,
                type_name,
                current_offset,
                iter_size,
                PatternValue::Struct,
            );
            node.children = children;
            node_stack.push(node);

            // Continue or break based on next_offset
            match next_offset {
                Some(off) => current_offset = off,
                None => break,
            }
        }

        // Build nested node tree: last node is innermost, first is outermost.
        // [node0, node1, node2] → node0 { children: [..., node1 { children: [..., node2] }] }
        let member_name_str = self.interner.resolve(tail_info.member_name).to_string();
        let mut tail = node_stack.pop().unwrap();
        while let Some(mut parent_node) = node_stack.pop() {
            // Inner nodes get the member name from the DataPlacement
            tail.name = member_name_str.clone();
            parent_node.children.push(tail);
            tail = parent_node;
        }
        // Outermost node gets the caller-provided name
        tail.name = name.to_string();
        tail.attributes = attributes;
        let total_size = tail.size;
        Ok((tail, total_size))
    }

    /// Read a named (user-defined) type from binary data
    pub(crate) fn read_named_type(
        &mut self,
        type_name: &str,
        name: &str,
        offset: u64,
        endian: Endianness,
        attributes: PatternAttributes,
    ) -> Result<(PatternNode, u64), EvalError> {
        // Resolve the type
        let type_name_key = self.interner.intern(type_name);
        let typedef = if let Some(td) = self.scope.get_type(type_name_key) {
            td
        } else if let Some((signed, bits)) = parse_arbitrary_int_type(type_name) {
            return self.read_arbitrary_int(signed, bits, name, offset, endian, attributes);
        } else {
            return Err(EvalError::new(format!("undefined type '{}'", type_name)));
        };

        self.recursion_depth += 1;
        if self.recursion_depth > self.max_recursion_depth {
            self.recursion_depth -= 1;
            return Err(EvalError::new(format!(
                "recursion depth exceeded (max {})",
                self.max_recursion_depth
            )));
        }

        let result = match &*typedef {
            TypeDef::Struct { parent, body, .. } => {
                // Check for tail-recursive struct pattern and optimize to loop
                if let Some(tail_info) = self.detect_tail_recursive_struct(type_name, &body) {
                    // Tail-recursive: undo the recursion_depth bump and use loop
                    self.recursion_depth -= 1;
                    return self.read_struct_tail_recursive(
                        type_name, name, offset, endian, attributes, &parent, &body, tail_info,
                    );
                }

                self.setup_struct_scope(offset);
                let mut children = Vec::new();
                let mut current_offset = offset;

                // Read parent + body fields, ensuring scope.pop() on all paths
                let parent_result = if let Some((parent_name, parent_args)) = &parent {
                    self.read_parent_fields(
                        *parent_name,
                        parent_args,
                        &mut current_offset,
                        &mut children,
                        endian,
                    )
                } else {
                    Ok(())
                };
                let body_result = parent_result.and_then(|_| {
                    self.read_struct_body(&body, &mut current_offset, &mut children, endian)
                });

                self.scope.pop();

                match body_result {
                    Ok(()) => {
                        let total_size = current_offset.saturating_sub(offset);
                        let mut node = PatternNode::new(
                            name,
                            type_name,
                            offset,
                            total_size,
                            PatternValue::Struct,
                        );
                        node.children = children;
                        node.attributes = attributes;
                        Ok((node, total_size))
                    }
                    Err(e) => Err(e),
                }
            }
            TypeDef::Union { body, .. } => {
                self.setup_struct_scope(offset);
                let mut children = Vec::new();
                let mut max_size = 0u64;
                let mut union_err: Option<EvalError> = None;

                for member_stmt in body {
                    if let StmtKind::DataPlacement(d) = &member_stmt.kind
                    {
                        let mn_str = self.interner.resolve(d.name).to_string();
                        match self.read_type(&d.ty, &mn_str, offset, endian, &d.attrs) {
                            Ok((child_node, child_size)) => {
                                self.scope.define_var(
                                    d.name,
                                    value_from_pattern(&child_node.value),
                                    false,
                                );
                                max_size = max_size.max(child_size);
                                children.push(child_node);
                            }
                            Err(e) => {
                                union_err = Some(e);
                                break;
                            }
                        }
                    }
                }

                self.scope.pop();

                match union_err {
                    Some(e) => Err(e),
                    None => {
                        let mut node = PatternNode::new(
                            name,
                            type_name,
                            offset,
                            max_size,
                            PatternValue::Union,
                        );
                        node.children = children;
                        node.attributes = attributes;
                        Ok((node, max_size))
                    }
                }
            }
            TypeDef::Enum {
                underlying,
                members,
                ..
            } => {
                match self.read_type(&underlying, name, offset, endian, &[]) {
                    Err(e) => Err(e),
                    Ok((value_node, size)) => {
                        let raw_val_result: Result<u128, EvalError> = match &value_node.value {
                            PatternValue::Unsigned(v) => Ok(*v),
                            PatternValue::Signed(v) => Ok(*v as u128),
                            PatternValue::Char(v) => Ok(*v as u128),
                            _ => {
                                // Underlying type is a struct/union (e.g. vint).
                                // Try applying the type's transform attribute to get an integer.
                                let val = value_from_node(&value_node);
                                let underlying_td = match &underlying.kind {
                                    TypeExprKind::Named(path) if path.len() == 1 => {
                                        self.scope.get_type(path[0])
                                    }
                                    _ => None,
                                };
                                let type_attrs = match underlying_td.as_deref() {
                                    Some(TypeDef::Struct { attrs, .. })
                                    | Some(TypeDef::Union { attrs, .. }) => attrs.clone(),
                                    _ => vec![],
                                };
                                self.results.push(value_node.clone());
                                let transform_result =
                                    self.apply_transform(&type_attrs, &val, underlying.span);
                                self.results.pop();
                                match transform_result {
                                    Err(e) => Err(e),
                                    Ok(Some(transformed)) => {
                                        transformed.to_unsigned().map_err(|e| self.resolve_error(e))
                                    }
                                    Ok(None) => {
                                        Err(EvalError::new("enum underlying type must be integer"))
                                    }
                                }
                            }
                        };

                        match raw_val_result {
                            Err(e) => Err(e),
                            Ok(raw_val) => {
                                let variant_name = members
                                    .iter()
                                    .find(|(_, v)| {
                                        v.as_ref()
                                            .map(|val| {
                                                val.to_unsigned().unwrap_or(u128::MAX) == raw_val
                                            })
                                            .unwrap_or(false)
                                    })
                                    .map(|(name, _)| self.interner.resolve(*name).to_string())
                                    .unwrap_or_else(|| format!("??? ({})", raw_val));

                                let mut node = PatternNode::new(
                                    name,
                                    type_name,
                                    offset,
                                    size,
                                    PatternValue::Enum {
                                        value: raw_val,
                                        name: variant_name,
                                    },
                                );
                                node.attributes = attributes;
                                Ok((node, size))
                            }
                        }
                    }
                }
            }
            TypeDef::Bitfield { body, .. } => {
                if let Some(StmtKind::BitfieldDef(bf_data)) = body.first().map(|s| &s.kind)
                {
                    // Parse bitfield_order attribute: [[bitfield_order(Order, group_size)]]
                    let bf_order_name = self.interner.intern("bitfield_order");
                    let mut msb_first = false;
                    let mut group_size = 8u64; // default group size
                    for attr in &bf_data.attrs {
                        if attr.name == bf_order_name && !attr.args.is_empty() {
                            msb_first = match self.eval_expr(&attr.args[0]) {
                                Ok(val) => {
                                    let order = val.to_unsigned().unwrap_or(1);
                                    order == 0 // MostToLeastSignificant = 0
                                }
                                Err(_) => {
                                    Self::is_msb_first_from_expr(&self.interner, &attr.args[0])
                                }
                            };
                            if attr.args.len() >= 2 {
                                if let Ok(gs_val) = self.eval_expr(&attr.args[1]) {
                                    group_size = gs_val.to_unsigned().unwrap_or(8) as u64;
                                }
                            }
                        }
                    }

                    let mut total_bits = 0u64;
                    let mut children = Vec::new();
                    let mut bf_err: Option<EvalError> = None;

                    for stmt in &bf_data.body {
                        if let Err(e) = self.eval_bitfield_stmt(
                            stmt,
                            offset,
                            &mut total_bits,
                            &mut children,
                            msb_first,
                            group_size,
                        ) {
                            bf_err = Some(e);
                            break;
                        }
                    }

                    match bf_err {
                        Some(e) => Err(e),
                        None => {
                            let total_bytes = total_bits.div_ceil(8);
                            let mut node = PatternNode::new(
                                name,
                                type_name,
                                offset,
                                total_bytes,
                                PatternValue::Bitfield,
                            );
                            node.children = children;
                            node.attributes = attributes;
                            Ok((node, total_bytes))
                        }
                    }
                } else {
                    Err(EvalError::new(format!(
                        "invalid bitfield definition for '{}'",
                        type_name
                    )))
                }
            }
            TypeDef::Alias { ty: Some(ty), .. } => self.read_type(&ty, name, offset, endian, &[]),
            TypeDef::Alias {
                ty: None,
                name: alias_name,
                ..
            } => {
                // Forward-declared alias (e.g., `using CGB;` in a namespace).
                // Try to find the actual type definition by the alias name in scope.
                let alias_name = *alias_name;
                if let Some(actual_td) = self.scope.get_type(alias_name) {
                    if !matches!(&*actual_td, TypeDef::Alias { ty: None, .. }) {
                        let alias_str = self.interner.resolve(alias_name).to_string();
                        // Decrement depth before recursive call (it will re-increment)
                        self.recursion_depth -= 1;
                        return self.read_named_type(&alias_str, name, offset, endian, attributes);
                    }
                }
                Err(EvalError::new(format!(
                    "forward-declared type alias used without definition for '{}'",
                    type_name
                )))
            }
        };

        self.recursion_depth -= 1;
        result
    }

    /// Resolve element type to (byte_size, BuiltinType, optional endian override) for bulk read.
    /// Returns Some((size, bt, endian_override)) for builtin types, enums with builtin underlying,
    /// and bitfields with known size. Endian-wrapped types return the explicit endianness.
    fn resolve_bulk_elem_info(
        &mut self,
        elem_ty: &TypeExpr,
    ) -> Option<(u64, BuiltinType, Option<Endianness>)> {
        match &elem_ty.kind {
            TypeExprKind::Builtin(bt) => bt.size().map(|s| (s, *bt, None)),
            TypeExprKind::Named(path) => {
                let name = self.resolve_path(path);
                let key = self.interner.intern(&name);
                let td = self.scope.get_type(key)?;
                match &*td {
                    TypeDef::Enum { underlying, .. } => {
                        if let TypeExprKind::Builtin(bt) = &underlying.kind {
                            bt.size().map(|s| (s, *bt, None))
                        } else {
                            None
                        }
                    }
                    TypeDef::Bitfield { .. } => {
                        let size = self.type_size(elem_ty).ok()?;
                        let bt = match size {
                            1 => BuiltinType::U8,
                            2 => BuiltinType::U16,
                            4 => BuiltinType::U32,
                            8 => BuiltinType::U64,
                            16 => BuiltinType::U128,
                            _ => return None,
                        };
                        Some((size, bt, None))
                    }
                    TypeDef::Alias {
                        ty: Some(alias_ty), ..
                    } => {
                        let alias_ty = alias_ty.clone();
                        self.resolve_bulk_elem_info(&alias_ty)
                    }
                    _ => None,
                }
            }
            TypeExprKind::Endian(end, inner) => {
                let (size, bt, _) = self.resolve_bulk_elem_info(inner)?;
                Some((size, bt, Some(*end)))
            }
            TypeExprKind::Padding => Some((1, BuiltinType::U8, None)),
            _ => None,
        }
    }

    /// Read an array from binary data
    pub(crate) fn read_array(
        &mut self,
        elem_ty: &TypeExpr,
        size: &ArraySize,
        name: &str,
        offset: u64,
        endian: Endianness,
        attributes: PatternAttributes,
    ) -> Result<(PatternNode, u64), EvalError> {
        // Handle str[N] as reading N bytes into a string (not an array of str)
        if matches!(&elem_ty.kind, TypeExprKind::Builtin(BuiltinType::Str)) {
            if let ArraySize::Fixed(size_expr) = size {
                let count_val = self.eval_expr(size_expr)?;
                let count = count_val
                    .to_unsigned()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(size_expr.span)))?
                    as u64;
                let bytes = self.read_data_bytes(offset, count)?;
                // Read as null-terminated Latin-1 string
                let s: String = bytes
                    .iter()
                    .take_while(|&&b| b != 0)
                    .map(|&b| b as char)
                    .collect();
                let mut node =
                    PatternNode::new(name, "str", offset, count, PatternValue::String(s));
                node.attributes = attributes;
                return Ok((node, count));
            }
        }

        let type_name = format!("{}[]", type_name_of(elem_ty, &self.interner));
        let is_sealed = attributes.sealed;
        let ctx = ArrayReadContext {
            elem_ty,
            name,
            offset,
            endian,
            attributes,
            type_name: &type_name,
        };

        match size {
            ArraySize::Fixed(size_expr) => self.read_array_fixed(&ctx, size_expr, is_sealed),
            ArraySize::Conditional(cond_expr) => self.read_array_conditional(&ctx, cond_expr),
            ArraySize::NullTerminated => self.read_array_null_terminated(&ctx),
        }
    }

    fn read_array_fixed(
        &mut self,
        ctx: &ArrayReadContext,
        size_expr: &Expr,
        is_sealed: bool,
    ) -> Result<(PatternNode, u64), EvalError> {
        let count_val = self.eval_expr(size_expr)?;
        let count = count_val
            .to_unsigned()
            .map_err(|e| self.resolve_error(e.with_span_if_none(size_expr.span)))?;

        // For sealed arrays, compute total size without reading all elements
        if is_sealed {
            if let Ok(elem_size) = self.type_size(ctx.elem_ty) {
                let total_size = (count as u64).saturating_mul(elem_size);
                let mut node = PatternNode::new(
                    ctx.name,
                    ctx.type_name,
                    ctx.offset,
                    total_size,
                    PatternValue::Array,
                );
                node.attributes = ctx.attributes.clone();
                return Ok((node, total_size));
            }
        }

        // Always bulk-read char arrays as String (char is 1 byte, index
        // access on String returns Char, so no children needed at any size).
        if !is_sealed
            && matches!(&ctx.elem_ty.kind, TypeExprKind::Builtin(BuiltinType::Char))
            && count > 0
        {
            let byte_count = count as u64;
            let bytes = self
                .data
                .read_bytes(ctx.offset, byte_count)
                .map_err(|e| self.resolve_error(e.with_span_if_none(ctx.elem_ty.span)))?;
            let s: String = bytes.iter().map(|&b| b as char).collect();
            let mut node = PatternNode::new(
                ctx.name,
                ctx.type_name,
                ctx.offset,
                byte_count,
                PatternValue::String(s),
            );
            node.attributes = ctx.attributes.clone();
            return Ok((node, byte_count));
        }

        // Bulk read optimization for fixed-size arrays (builtin, enum, bitfield).
        // Only for large arrays (>=64) to avoid breaking scope propagation
        // in structs where small arrays need element-by-element children.
        if !is_sealed && count >= BULK_READ_THRESHOLD {
            if let Some((elem_size, bulk_bt, endian_override)) =
                self.resolve_bulk_elem_info(ctx.elem_ty)
            {
                let bulk_endian = endian_override.unwrap_or(ctx.endian);
                let byte_count = (count as u64).saturating_mul(elem_size);
                let bytes = self
                    .data
                    .read_bytes(ctx.offset, byte_count)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(ctx.elem_ty.span)))?;
                let pv = if bulk_bt == BuiltinType::Char {
                    let s: String = bytes.iter().map(|&b| b as char).collect();
                    PatternValue::String(s)
                } else {
                    PatternValue::BulkData {
                        data: Arc::new(bytes),
                        elem_type: bulk_bt,
                        endian: bulk_endian,
                    }
                };
                let mut node =
                    PatternNode::new(ctx.name, ctx.type_name, ctx.offset, byte_count, pv);
                node.attributes = ctx.attributes.clone();
                return Ok((node, byte_count));
            }
        }

        // Lazy read for large fixed-size struct arrays (Named/Template types only).
        // Instead of reading all elements, create a LazyStructArray node that
        // reads elements on-demand when indexed.
        if !is_sealed && count >= BULK_READ_THRESHOLD {
            let (inner_ty, eff_endian) = match &ctx.elem_ty.kind {
                TypeExprKind::Endian(end, inner) => (inner.as_ref(), *end),
                _ => (ctx.elem_ty, ctx.endian),
            };
            if matches!(
                &inner_ty.kind,
                TypeExprKind::Named(_) | TypeExprKind::Template(..)
            ) {
                if let Ok(elem_size) = self.type_size(ctx.elem_ty) {
                    if elem_size > 0 {
                        let tn = type_name_of(inner_ty, &self.interner);
                        let total_size = (count as u64).saturating_mul(elem_size);
                        let mut node = PatternNode::new(
                            ctx.name,
                            ctx.type_name,
                            ctx.offset,
                            total_size,
                            PatternValue::LazyStructArray {
                                type_name: tn,
                                elem_size,
                                count: count as u64,
                                endian: eff_endian,
                            },
                        );
                        node.attributes = ctx.attributes.clone();
                        return Ok((node, total_size));
                    }
                }
            }
        }

        let mut children = Vec::with_capacity((count as usize).min(0x10000));
        let mut current_offset = ctx.offset;
        for i in 0..count as u64 {
            let elem_name = self.fmt_elem_name(i);
            match self.read_type(ctx.elem_ty, &elem_name, current_offset, ctx.endian, &[]) {
                Ok((child_node, child_size)) => {
                    current_offset += child_size;
                    children.push(child_node);
                }
                Err(e) if e.is_read_oob => break,
                Err(e) => return Err(e),
            }
            if self.break_signaled {
                self.break_signaled = false;
                break;
            }
        }

        let total_size = current_offset - ctx.offset;
        let mut node = PatternNode::new(
            ctx.name,
            ctx.type_name,
            ctx.offset,
            total_size,
            PatternValue::Array,
        );
        node.children = children;
        node.attributes = ctx.attributes.clone();
        Ok((node, total_size))
    }

    fn read_array_conditional(
        &mut self,
        ctx: &ArrayReadContext,
        cond_expr: &Expr,
    ) -> Result<(PatternNode, u64), EvalError> {
        let mut children = Vec::new();
        let mut current_offset = ctx.offset;

        // Pre-compute loop bound from condition expression.
        // Supports patterns:
        //   $ < bound_expr          → bound = bound_expr
        //   $ <= bound_expr         → bound = bound_expr + 1
        //   ($ - base_expr) < size  → bound = base + size
        //   ($ - base_expr) <= size → bound = base + size + 1
        let pre_computed_bound: Option<u64> = match &cond_expr.kind {
            ExprKind::Binary {
                op: BinOp::Lt,
                lhs,
                rhs,
            } if matches!(lhs.kind, ExprKind::Dollar) => self
                .eval_expr(rhs)
                .ok()
                .and_then(|v| v.to_unsigned().ok())
                .map(|v| v as u64),
            ExprKind::Binary {
                op: BinOp::Le,
                lhs,
                rhs,
            } if matches!(lhs.kind, ExprKind::Dollar) => self
                .eval_expr(rhs)
                .ok()
                .and_then(|v| v.to_unsigned().ok())
                .map(|v| v as u64 + 1),
            ExprKind::Binary {
                op: BinOp::Lt | BinOp::Le,
                lhs,
                rhs,
            } => {
                // ($ - base) < size or ($ - base) <= size
                if let ExprKind::Binary {
                    op: BinOp::Sub,
                    lhs: inner_lhs,
                    rhs: base_expr,
                } = &lhs.kind
                {
                    if matches!(inner_lhs.kind, ExprKind::Dollar) {
                        let base = self
                            .eval_expr(base_expr)
                            .ok()
                            .and_then(|v| v.to_unsigned().ok())
                            .map(|v| v as u64);
                        let size = self
                            .eval_expr(rhs)
                            .ok()
                            .and_then(|v| v.to_unsigned().ok())
                            .map(|v| v as u64);
                        match (base, size) {
                            (Some(b), Some(s)) => {
                                let adjust = if matches!(
                                    &cond_expr.kind,
                                    ExprKind::Binary { op: BinOp::Le, .. }
                                ) {
                                    1
                                } else {
                                    0
                                };
                                Some(b + s + adjust)
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        // Clamp bound to data size
        let pre_computed_bound = pre_computed_bound.map(|b| b.min(self.active_data_size()));

        // Bulk read optimization for while-loop with pre-computed bound
        // and fixed-size builtin elements.
        // Skip if array_limit would be exceeded (let the loop handle the error).
        if let Some(bound) = pre_computed_bound {
            if let Some((elem_size, bulk_bt, endian_override)) =
                self.resolve_bulk_elem_info(ctx.elem_ty)
            {
                if bound > current_offset {
                    let avail = bound - current_offset;
                    let count = avail / elem_size;
                    let byte_count = count * elem_size;
                    if count >= BULK_READ_THRESHOLD as u64
                        && byte_count > 0
                        && (self.max_pattern_limit == 0 || count <= self.max_pattern_limit)
                    {
                        let bulk_endian = endian_override.unwrap_or(ctx.endian);
                        let bytes =
                            self.read_data_bytes(current_offset, byte_count)
                                .map_err(|e| {
                                    self.resolve_error(e.with_span_if_none(ctx.elem_ty.span))
                                })?;
                        let pv = if bulk_bt == BuiltinType::Char {
                            let s: String = bytes.iter().map(|&b| b as char).collect();
                            PatternValue::String(s)
                        } else {
                            PatternValue::BulkData {
                                data: Arc::new(bytes),
                                elem_type: bulk_bt,
                                endian: bulk_endian,
                            }
                        };
                        let mut node = PatternNode::new(
                            ctx.name,
                            ctx.type_name,
                            current_offset,
                            byte_count,
                            pv,
                        );
                        node.attributes = ctx.attributes.clone();
                        return Ok((node, byte_count));
                    }
                }
            }
        }

        // LazyStructArray for while-loop with pre-computed bound and
        // named/template types with known element size.
        if let Some(bound) = pre_computed_bound {
            if bound > current_offset {
                let (inner_ty, eff_endian) = match &ctx.elem_ty.kind {
                    TypeExprKind::Endian(end, inner) => (inner.as_ref(), *end),
                    _ => (ctx.elem_ty, ctx.endian),
                };
                if matches!(
                    &inner_ty.kind,
                    TypeExprKind::Named(_) | TypeExprKind::Template(..)
                ) {
                    if let Ok(elem_size) = self.type_size(ctx.elem_ty) {
                        if elem_size > 0 {
                            let avail = bound - current_offset;
                            let count = avail / elem_size;
                            if count >= BULK_READ_THRESHOLD as u64
                                && (self.max_pattern_limit == 0 || count <= self.max_pattern_limit)
                            {
                                let tn = type_name_of(inner_ty, &self.interner);
                                let total_size = count * elem_size;
                                let mut node = PatternNode::new(
                                    ctx.name,
                                    ctx.type_name,
                                    ctx.offset,
                                    total_size,
                                    PatternValue::LazyStructArray {
                                        type_name: tn,
                                        elem_size,
                                        count,
                                        endian: eff_endian,
                                    },
                                );
                                node.attributes = ctx.attributes.clone();
                                return Ok((node, total_size));
                            }
                        }
                    }

                    // Probe-based LazyStructArray: read one element to measure size,
                    // then lazily represent the rest if it evenly divides the range.
                    let avail = bound - current_offset;
                    if avail > 0 {
                        let probe_result =
                            self.read_type(ctx.elem_ty, "[0]", current_offset, ctx.endian, &[]);
                        if let Ok((_probe_node, probe_size)) = probe_result {
                            if probe_size > 0 {
                                let total_count = avail / probe_size;
                                let remainder = avail % probe_size;
                                if total_count >= BULK_READ_THRESHOLD as u64
                                    && remainder == 0
                                    && (self.max_pattern_limit == 0
                                        || total_count <= self.max_pattern_limit)
                                {
                                    let tn = type_name_of(inner_ty, &self.interner);
                                    let total_size = total_count * probe_size;
                                    let mut node = PatternNode::new(
                                        ctx.name,
                                        ctx.type_name,
                                        ctx.offset,
                                        total_size,
                                        PatternValue::LazyStructArray {
                                            type_name: tn,
                                            elem_size: probe_size,
                                            count: total_count,
                                            endian: eff_endian,
                                        },
                                    );
                                    node.attributes = ctx.attributes.clone();
                                    return Ok((node, total_size));
                                }
                            }
                        }
                    }
                }
            }
        }

        // Fast-bound loop: use pre-computed bound for offset comparison
        // instead of re-evaluating the condition expression each iteration.
        if let Some(bound) = pre_computed_bound {
            let mut i = 0u64;
            let saved_offset = self.offset;
            self.offset = current_offset;
            loop {
                if current_offset >= bound {
                    break;
                }
                // Conditional (while) arrays use pattern_limit (not array_limit)
                // as their per-array bound, matching ImHex behavior.
                if self.max_pattern_limit > 0 && i >= self.max_pattern_limit {
                    self.offset = saved_offset;
                    return Err(self.make_error(
                        format!(
                            "while-loop element count exceeded pattern limit (max {})",
                            self.max_pattern_limit
                        ),
                        cond_expr.span,
                    ));
                }
                let elem_name = self.fmt_elem_name(i);
                let (child_node, child_size) = match self.read_type(
                    ctx.elem_ty,
                    &elem_name,
                    current_offset,
                    ctx.endian,
                    &[],
                ) {
                    Ok(v) => v,
                    Err(e) if e.is_read_oob => break,
                    Err(e) => return Err(e),
                };
                if self.break_signaled {
                    self.break_signaled = false;
                    current_offset += child_size;
                    self.offset = current_offset;
                    children.push(child_node);
                    break;
                }
                if child_size == 0 {
                    children.push(child_node);
                    break;
                }
                current_offset += child_size;
                self.offset = current_offset;
                children.push(child_node);
                i += 1;
            }
            self.offset = saved_offset;
        } else {
            // Fallback: evaluate condition expression each iteration
            let mut i = 0u64;
            let saved_offset = self.offset;
            self.offset = current_offset;
            loop {
                // Conditional (while) arrays use pattern_limit (not array_limit)
                if self.max_pattern_limit > 0 && i >= self.max_pattern_limit {
                    self.offset = saved_offset;
                    return Err(self.make_error(
                        format!(
                            "while-loop element count exceeded pattern limit (max {})",
                            self.max_pattern_limit
                        ),
                        cond_expr.span,
                    ));
                }
                // OOB during condition evaluation (e.g. std::mem::read_unsigned at EOF)
                // is treated as loop termination
                let cond_val = match self.eval_expr(cond_expr) {
                    Ok(v) => v,
                    Err(e) if e.is_read_oob => break,
                    Err(e) => return Err(e),
                };
                if !cond_val
                    .to_bool()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(cond_expr.span)))?
                {
                    break;
                }
                let elem_name = self.fmt_elem_name(i);
                // OOB during element read is treated as loop termination
                let (child_node, child_size) = match self.read_type(
                    ctx.elem_ty,
                    &elem_name,
                    current_offset,
                    ctx.endian,
                    &[],
                ) {
                    Ok(v) => v,
                    Err(e) if e.is_read_oob => break,
                    Err(e) => return Err(e),
                };
                if self.break_signaled {
                    self.break_signaled = false;
                    current_offset += child_size;
                    self.offset = current_offset;
                    children.push(child_node);
                    break;
                }
                // Prevent infinite loop when element reads zero bytes
                if child_size == 0 {
                    children.push(child_node);
                    break;
                }
                current_offset += child_size;
                self.offset = current_offset;
                children.push(child_node);
                i += 1;
            }
            self.offset = saved_offset;
        }

        let total_size = current_offset - ctx.offset;
        let mut node = PatternNode::new(
            ctx.name,
            ctx.type_name,
            ctx.offset,
            total_size,
            PatternValue::Array,
        );
        node.children = children;
        node.attributes = ctx.attributes.clone();
        Ok((node, total_size))
    }

    fn read_array_null_terminated(
        &mut self,
        ctx: &ArrayReadContext,
    ) -> Result<(PatternNode, u64), EvalError> {
        let mut current_offset = ctx.offset;

        // Bulk read for NullTerminated builtin/enum/bitfield arrays
        if let Some((elem_size, bulk_bt, endian_override)) =
            self.resolve_bulk_elem_info(ctx.elem_ty)
        {
            let bulk_endian = endian_override.unwrap_or(ctx.endian);
            let remaining = self.active_data_size().saturating_sub(current_offset);
            let max_bytes = if self.max_array_limit > 0 {
                self.max_array_limit * elem_size
            } else {
                remaining
            };
            // Align to elem_size boundary
            let byte_count = (max_bytes.min(remaining) / elem_size) * elem_size;
            if byte_count > 0 {
                let bytes = self
                    .read_data_bytes(current_offset, byte_count)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(ctx.elem_ty.span)))?;
                let pv = if bulk_bt == BuiltinType::Char {
                    // For char[], build string (stops at null if present)
                    let s: String = bytes
                        .iter()
                        .take_while(|&&b| b != 0)
                        .map(|&b| b as char)
                        .collect();
                    PatternValue::String(s)
                } else {
                    PatternValue::BulkData {
                        data: Arc::new(bytes),
                        elem_type: bulk_bt,
                        endian: bulk_endian,
                    }
                };
                let mut node =
                    PatternNode::new(ctx.name, ctx.type_name, current_offset, byte_count, pv);
                node.attributes = ctx.attributes.clone();
                return Ok((node, byte_count));
            }
        }

        // Fallback: element-by-element loop
        let mut children = Vec::new();
        let mut i = 0u64;
        while current_offset < self.active_data_size() {
            if self.max_array_limit > 0 && i >= self.max_array_limit {
                break;
            }
            let elem_name = self.fmt_elem_name(i);
            match self.read_type(ctx.elem_ty, &elem_name, current_offset, ctx.endian, &[]) {
                Ok((child_node, child_size)) => {
                    if self.break_signaled {
                        self.break_signaled = false;
                        current_offset += child_size;
                        children.push(child_node);
                        break;
                    }
                    current_offset += child_size;
                    children.push(child_node);
                }
                Err(_) => break,
            }
            i += 1;
        }

        let total_size = current_offset - ctx.offset;
        let mut node = PatternNode::new(
            ctx.name,
            ctx.type_name,
            ctx.offset,
            total_size,
            PatternValue::Array,
        );
        node.children = children;
        node.attributes = ctx.attributes.clone();
        Ok((node, total_size))
    }
}
