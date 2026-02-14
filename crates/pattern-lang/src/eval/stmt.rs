// Statement evaluation methods for the evaluator
use super::*;

impl<'a> Evaluator<'a> {
    /// Evaluate a statement
    pub(crate) fn eval_stmt(&mut self, stmt: &Stmt) -> Result<ControlFlow, EvalError> {
        self.stmt_count += 1;
        if let Some(ref counter) = self.shared_stmt_count {
            if self.stmt_count & 0x3F == 0 {
                counter.store(self.stmt_count, Ordering::Relaxed);
            }
        }
        match &stmt.kind {
            StmtKind::LocalVar { ty, name, init } => {
                let val = self.eval_expr(init)?;
                if matches!(&ty.kind, TypeExprKind::Builtin(BuiltinType::Str))
                    && matches!(&val, Value::Unsigned(_) | Value::Signed(_))
                {
                    // str type with integer initializer defaults to empty string
                    self.scope
                        .define_var(*name, Value::String(String::new()), false);
                } else {
                    self.scope.define_var(*name, val.clone(), false);
                    // In struct body context (e.g., inside if/match), promote LocalVar
                    // to a virtual PatternNode so it can be found via member access
                    if self.in_struct_body {
                        let name_str = self.interner.resolve(*name).to_string();
                        let type_name_str = type_name_of(ty, &self.interner);
                        let pval = match &val {
                            Value::Unsigned(v) => PatternValue::Unsigned(*v),
                            Value::Signed(v) => PatternValue::Signed(*v),
                            Value::Bool(b) => PatternValue::Unsigned(if *b { 1 } else { 0 }),
                            Value::String(s) => PatternValue::String(s.clone()),
                            Value::Float(f) => PatternValue::Float(*f),
                            _ => PatternValue::Unsigned(0),
                        };
                        self.results.push(PatternNode::new(
                            name_str,
                            type_name_str,
                            self.offset,
                            0,
                            pval,
                        ));
                    }
                }
            }
            StmtKind::DataPlacement(d) => {
                let name_str = self.interner.resolve(d.name).to_string();
                self.eval_data_placement(
                    &d.ty,
                    &name_str,
                    d.offset.as_ref(),
                    &d.attrs,
                    d.pointer_size.as_ref(),
                    d.section.as_ref(),
                )?;
            }
            StmtKind::If(d) => {
                let cond_val = self.eval_expr(&d.cond)?;
                if cond_val
                    .to_bool()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(stmt.span)))?
                {
                    for s in &d.then_body {
                        match self.eval_stmt(s)? {
                            ControlFlow::None => {}
                            cf => return Ok(cf),
                        }
                    }
                } else if let Some(else_stmts) = &d.else_body {
                    for s in else_stmts {
                        match self.eval_stmt(s)? {
                            ControlFlow::None => {}
                            cf => return Ok(cf),
                        }
                    }
                }
            }
            StmtKind::While(d) => {
                let mut iterations = 0u64;
                loop {
                    iterations += 1;
                    self.check_resource_limits(stmt.span)?;
                    if self.max_pattern_limit > 0 && iterations > self.max_pattern_limit {
                        return Err(self.make_error(
                            format!(
                                "while loop iteration limit exceeded (max {})",
                                self.max_pattern_limit
                            ),
                            stmt.span,
                        ));
                    }
                    let cond_val = match self.eval_expr(&d.cond) {
                        Ok(v) => v,
                        Err(e) if e.is_read_oob => break,
                        Err(e) => return Err(e),
                    };
                    if !cond_val
                        .to_bool()
                        .map_err(|e| self.resolve_error(e.with_span_if_none(stmt.span)))?
                    {
                        break;
                    }
                    match self.exec_loop_body(&d.body) {
                        Ok(LoopAction::Continue) => {}
                        Ok(LoopAction::Break) => break,
                        Ok(LoopAction::Return(cf)) => return Ok(cf),
                        Err(e) if e.is_read_oob => break,
                        Err(e) => return Err(e),
                    }
                }
            }
            StmtKind::For(d) => {
                self.scope.push();
                let result = self.eval_for_loop(&d.init, &d.cond, &d.step, &d.body, stmt.span);
                self.scope.pop();
                let cf = result?;
                if !matches!(cf, ControlFlow::None) {
                    return Ok(cf);
                }
            }
            StmtKind::Match(d) => {
                let match_vals: Vec<Value> = d.exprs
                    .iter()
                    .map(|e| self.eval_expr(e))
                    .collect::<Result<_, _>>()?;
                for arm in &d.arms {
                    if self.match_patterns_multi(&match_vals, &arm.patterns)? {
                        self.scope.push();
                        for s in &arm.body {
                            match self.eval_stmt(s)? {
                                ControlFlow::None => {}
                                cf => {
                                    self.scope.pop();
                                    return Ok(cf);
                                }
                            }
                        }
                        self.scope.pop();
                        break;
                    }
                }
            }
            StmtKind::TryCatch(d) => {
                self.scope.push();
                let mut try_failed = false;
                for s in &d.try_body {
                    if let Err(_) = self.eval_stmt(s) {
                        try_failed = true;
                        break;
                    }
                }
                self.scope.pop();
                if try_failed {
                    if let Some(catch_body) = &d.catch_body {
                        self.scope.push();
                        for s in catch_body {
                            self.eval_stmt(s)?;
                        }
                        self.scope.pop();
                    }
                }
            }
            StmtKind::Break => return Ok(ControlFlow::Break),
            StmtKind::Continue => return Ok(ControlFlow::Continue),
            StmtKind::Return(expr) => {
                let val = if let Some(e) = expr {
                    Some(self.eval_expr(e)?)
                } else {
                    None
                };
                return Ok(ControlFlow::Return(val));
            }
            StmtKind::AnonymousPlacement(d) => {
                let var_offset = if let Some(placement_expr) = &d.placement {
                    let val = self.eval_expr(placement_expr)?;
                    val.to_unsigned()
                        .map_err(|e| self.resolve_error(e.with_span_if_none(d.ty.span)))?
                        as u64
                } else {
                    self.offset
                };
                let endian = self.resolve_endianness(&d.ty);
                let anon_name = format!("_{}", type_name_of(&d.ty, &self.interner));
                let (node, size) = self.read_type(&d.ty, &anon_name, var_offset, endian, &d.attrs)?;
                if d.placement.is_none() {
                    self.offset = var_offset + size;
                }
                self.results.push(node);
                self.check_pattern_limit(d.ty.span)?;
            }
            StmtKind::ExprStmt(expr) => {
                // Check if this is actually an anonymous type placement that the
                // parser couldn't disambiguate (e.g. `u32;` or `RecordHeader;`).
                let is_anon_type = match &expr.kind {
                    ExprKind::Ident(name) => {
                        let name_str = self.interner.resolve(*name);
                        BuiltinType::from_str(name_str).is_some()
                            || self.scope.get_type(*name).is_some()
                    }
                    _ => false,
                };
                if is_anon_type {
                    if let ExprKind::Ident(name) = &expr.kind {
                        let name_str = self.interner.resolve(*name).to_string();
                        let ty_kind = if let Some(bt) = BuiltinType::from_str(&name_str) {
                            TypeExprKind::Builtin(bt)
                        } else {
                            TypeExprKind::Named(vec![*name])
                        };
                        let ty = TypeExpr {
                            kind: ty_kind,
                            span: expr.span,
                        };
                        let anon_name = format!("_{}", name_str);
                        let var_offset = self.offset;
                        let endian = self.resolve_endianness(&ty);
                        let (node, size) =
                            self.read_type(&ty, &anon_name, var_offset, endian, &[])?;
                        self.offset = var_offset + size;
                        self.results.push(node);
                        self.pattern_count += 1;
                    }
                } else {
                    self.eval_expr(expr)?;
                }
            }
            // BitfieldFieldStmt is handled during bitfield evaluation, skip here
            StmtKind::BitfieldFieldStmt { .. } => {}
            // Type/function definitions are handled in collect_definitions
            StmtKind::StructDef(..)
            | StmtKind::UnionDef(..)
            | StmtKind::EnumDef(..)
            | StmtKind::BitfieldDef(..)
            | StmtKind::FnDef(..)
            | StmtKind::TypeAlias(..)
            | StmtKind::Import { .. } => {}
            StmtKind::Namespace(d) => {
                // Set current namespace context for qualified name resolution
                let prev_ns = self.current_namespace.clone();
                let ns_str = self.interner.resolve(d.name).to_string();
                self.current_namespace = Some(match &prev_ns {
                    Some(parent) => format!("{}::{}", parent, ns_str),
                    None => ns_str,
                });
                // Execute non-definition statements (e.g. variable declarations)
                for inner_stmt in &d.body {
                    match &inner_stmt.kind {
                        StmtKind::StructDef(..)
                        | StmtKind::UnionDef(..)
                        | StmtKind::EnumDef(..)
                        | StmtKind::BitfieldDef(..)
                        | StmtKind::FnDef(..)
                        | StmtKind::TypeAlias(..) => {}
                        _ => {
                            self.eval_stmt(inner_stmt)?;
                        }
                    }
                }
                self.current_namespace = prev_ns;
            }
        }
        Ok(ControlFlow::None)
    }

    /// Evaluate a data placement: read type from binary data and register in scope
    pub(crate) fn eval_data_placement(
        &mut self,
        ty: &TypeExpr,
        name: &str,
        placement: Option<&Expr>,
        attrs: &[Attribute],
        pointer_size: Option<&TypeExpr>,
        section_expr: Option<&Expr>,
    ) -> Result<(), EvalError> {
        // Determine the offset for this placement
        let var_offset = if let Some(placement_expr) = placement {
            let val = self.eval_expr(placement_expr)?;
            val.to_unsigned()
                .map_err(|e| self.resolve_error(e.with_span_if_none(ty.span)))? as u64
        } else if matches!(&ty.kind, TypeExprKind::Builtin(BuiltinType::Str)) {
            // str type without initializer defaults to empty string
            let name_key = self.interner.intern(name);
            self.scope
                .define_var(name_key, Value::String(String::new()), false);
            return Ok(());
        } else {
            // Auto-advance: use current offset
            self.offset
        };

        // Handle section placement: read from section buffer instead of main data
        if let Some(sect_expr) = section_expr {
            return self.eval_section_placement(ty, name, var_offset, attrs, sect_expr);
        }

        // Read value from binary data
        let endian = self.resolve_endianness(ty);

        if let Some(ptr_size_ty) = pointer_size {
            let (node, ptr_bytes) = self.read_pointer(ty, ptr_size_ty, name, var_offset, attrs)?;
            if placement.is_none() {
                self.offset = var_offset + ptr_bytes;
            }
            // Propagate pointer target children for dotted name lookups
            propagate_children(&mut self.scope, &mut self.interner, name, &node.children);
            self.results.push(node);
            self.check_pattern_limit(ty.span)?;
            return Ok(());
        }

        let (node, size) = self.read_type(ty, name, var_offset, endian, attrs)?;

        // Update current offset if no explicit placement
        if placement.is_none() {
            self.offset = var_offset + size;
        }

        // Push node to results first so SizedRef lookups work during transform
        let name_key = self.interner.intern(name);
        let mut val = value_from_node(&node);

        // Propagate children with dotted names (e.g., "h.version") for member access
        propagate_children(&mut self.scope, &mut self.interner, name, &node.children);

        self.results.push(node);

        // Apply transform attribute after node is in results (transform may access members via SizedRef)
        // Check field-level attrs first, then type-level attrs
        let mut transformed_val = None;
        if transformed_val.is_none() {
            transformed_val = self.apply_transform(attrs, &val, ty.span)?;
        }
        if transformed_val.is_none() {
            // Check struct/union type-level attrs (e.g., struct vint { } [[transform("...")]])
            let type_attrs = self.get_type_attrs(ty);
            if !type_attrs.is_empty() {
                transformed_val = self.apply_transform(&type_attrs, &val, ty.span)?;
            }
        }
        if transformed_val.is_none() {
            // Check using alias type-level attrs (e.g., using uLEB128 = ... [[transform(...)]])
            let alias_attrs = self.get_alias_type_attrs(ty);
            if !alias_attrs.is_empty() {
                transformed_val = self.apply_transform(&alias_attrs, &val, ty.span)?;
            }
        }
        if let Some(transformed) = transformed_val {
            val = transformed;
        }
        self.scope.define_var(name_key, val, false);

        self.check_pattern_limit(ty.span)?;

        Ok(())
    }

    /// Evaluate a data placement that reads from a section buffer.
    fn eval_section_placement(
        &mut self,
        ty: &TypeExpr,
        name: &str,
        var_offset: u64,
        attrs: &[Attribute],
        sect_expr: &Expr,
    ) -> Result<(), EvalError> {
        let handle = self
            .eval_expr(sect_expr)?
            .to_unsigned()
            .map_err(|e| self.resolve_error(e.with_span_if_none(ty.span)))?;

        let endian = self.resolve_endianness(ty);

        // Clone section bytes, auto-grow if needed for the read
        let mut section_bytes = self.sections.get(&handle).cloned().unwrap_or_default();

        // We don't know the exact size yet, but ensure section is at least var_offset large
        if section_bytes.len() < var_offset as usize {
            section_bytes.resize(var_offset as usize, 0);
        }

        // Write grown bytes back before reading
        self.sections.insert(handle, section_bytes.clone());

        // Set override for read_type to read from section data
        self.section_data_override = Some(section_bytes);
        let result = self.read_type(ty, name, var_offset, endian, attrs);
        self.section_data_override = None;
        let (node, size) = result?;

        // Register section write-through metadata
        let name_key = self.interner.intern(name);
        self.section_vars
            .insert(name_key, (handle, var_offset, size));

        // Define variable in scope
        let val = value_from_node(&node);
        self.scope.define_var(name_key, val, false);

        Ok(())
    }

    /// Apply [[transform("fn_name")]] attribute: call the function with the value
    /// and return the transformed result, or None if no transform attribute.
    pub(crate) fn apply_transform(
        &mut self,
        attrs: &[Attribute],
        val: &Value,
        span: Span,
    ) -> Result<Option<Value>, EvalError> {
        let transform_name = self.interner.intern("transform");
        for attr in attrs {
            if attr.name == transform_name {
                if let Some(arg) = attr.args.first() {
                    let fn_name_val = self.eval_expr(arg)?;
                    let fn_name = fn_name_val.to_display_string();
                    return self
                        .call_fn_by_name(&fn_name, &[val.clone()], span)
                        .map(Some);
                }
            }
        }
        Ok(None)
    }

    /// Call a user-defined or builtin function by name with pre-evaluated arguments.
    pub(crate) fn call_fn_by_name(
        &mut self,
        fn_name: &str,
        args: &[Value],
        span: Span,
    ) -> Result<Value, EvalError> {
        let fn_name_key = self.interner.intern(fn_name);
        if let Some(func_def) = self.scope.get_fn(fn_name_key) {
            self.scope.push();
            // params are (Name, TypeExpr, Option<Expr>, bool) tuples
            for (i, param) in func_def.params.iter().enumerate() {
                if let Some(val) = args.get(i) {
                    // Propagate children for SizedRef params (e.g., struct passed to transform fn)
                    if let Value::SizedRef { offset, size } = val {
                        let param_name_str = self.interner.resolve(param.0).to_string();
                        // Search results in reverse to prefer the most recently added node,
                        // which is the correct one when nodes share the same offset
                        // (e.g., temp push for alias transform vs pre-existing node).
                        if let Some(node) =
                            find_node_by_offset_rev(&self.results, *offset, *size)
                        {
                            propagate_children(
                                &mut self.scope,
                                &mut self.interner,
                                &param_name_str,
                                &node.children,
                            );
                        }
                    }
                    self.scope.define_var(param.0, val.clone(), true);
                }
            }
            let mut return_val = Value::Null;
            for s in &func_def.body {
                match self.eval_stmt(s)? {
                    ControlFlow::Return(Some(v)) => {
                        return_val = v;
                        break;
                    }
                    ControlFlow::Return(None) => break,
                    _ => {}
                }
            }
            self.scope.pop();
            Ok(return_val)
        } else {
            // Try builtin dispatch
            self.eval_data_builtin(fn_name, args, span)
                .map(|opt| opt.unwrap_or(Value::Null))
        }
    }

    /// Match patterns in a match statement
    /// Check if a single value matches any of the given patterns (OR semantics for `|` separated patterns)
    pub(crate) fn match_patterns_multi(
        &mut self,
        values: &[Value],
        patterns: &[MatchPattern],
    ) -> Result<bool, EvalError> {
        for pattern in patterns {
            if self.match_single_pattern_multi(values, pattern)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Check if values match a single pattern (which may be a tuple)
    pub(crate) fn match_single_pattern_multi(
        &mut self,
        values: &[Value],
        pattern: &MatchPattern,
    ) -> Result<bool, EvalError> {
        match pattern {
            MatchPattern::Tuple(sub_patterns) => {
                if sub_patterns.len() != values.len() {
                    return Ok(false);
                }
                for (val, pat) in values.iter().zip(sub_patterns.iter()) {
                    if !self.match_single_pattern(val, pat)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            // For single-value patterns with a single match expression
            _ => {
                if values.len() == 1 {
                    self.match_single_pattern(&values[0], pattern)
                } else {
                    Ok(false)
                }
            }
        }
    }

    /// Check if a single value matches a single (non-tuple) pattern
    pub(crate) fn match_single_pattern(
        &mut self,
        value: &Value,
        pattern: &MatchPattern,
    ) -> Result<bool, EvalError> {
        match pattern {
            MatchPattern::Wildcard => Ok(true),
            MatchPattern::Value(expr) => {
                let pattern_val = self.eval_expr(expr)?;
                Ok(values_equal(value, &pattern_val))
            }
            MatchPattern::Range(start_expr, end_expr) => {
                let start = self.eval_expr(start_expr)?;
                let end = self.eval_expr(end_expr)?;
                let val = value.to_signed()?;
                let s = start.to_signed()?;
                let e = end.to_signed()?;
                Ok(val >= s && val <= e)
            }
            MatchPattern::Tuple(_) => Ok(false), // tuple inside tuple not supported
        }
    }
}
