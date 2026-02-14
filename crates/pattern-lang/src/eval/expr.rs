// Expression evaluation methods for the evaluator
use super::*;

impl<'a> Evaluator<'a> {
    /// Resolve a path of Names to a joined string (e.g. ["std", "mem"] → "std::mem")
    /// Try to resolve the data offset of an expression by searching the result tree.
    /// Used by addressof() when eval_expr returns a non-addressable value
    /// (e.g. char[] stored as Value::String).
    pub(crate) fn resolve_node_offset(&mut self, expr: &Expr) -> Option<u64> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                let name_str = self.interner.resolve(*name);
                // Search results recursively for a node with this name
                fn find_named_node(nodes: &[PatternNode], name: &str) -> Option<u64> {
                    for node in nodes {
                        if node.name == name {
                            return Some(node.offset);
                        }
                        if let Some(offset) = find_named_node(&node.children, name) {
                            return Some(offset);
                        }
                    }
                    None
                }
                find_named_node(&self.results, name_str)
            }
            ExprKind::MemberAccess { expr: obj, member } => {
                let member_str = self.interner.resolve(*member).to_string();
                // Try __addr_ scope lookup for the full dotted path
                if let Some(chain) = flatten_member_chain(expr, &self.interner) {
                    let addr_key_str = format!("__addr_{}", chain);
                    if let Some(addr_key) = self.interner.lookup(&addr_key_str) {
                        if let Some(Value::SizedRef { offset, .. }) = self.scope.get_var(addr_key) {
                            return Some(*offset);
                        }
                    }
                }
                // Try __addr_ with flatten_expr_to_scope_key (handles Index in chain)
                if let Some(chain) = self.flatten_expr_to_scope_key(expr) {
                    let addr_key_str = format!("__addr_{}", chain);
                    let addr_key = self.interner.intern(&addr_key_str);
                    if let Some(Value::SizedRef { offset, .. }) = self.scope.get_var(addr_key) {
                        return Some(*offset);
                    }
                }
                // Evaluate the object to get a SizedRef, then find the child node
                if let Ok(obj_val) = self.eval_expr_for_addressof(obj) {
                    if let Value::SizedRef { offset, size } = obj_val {
                        if let Some(parent_node) = find_node_by_offset(&self.results, offset, size)
                            .or_else(|| find_node_by_offset(&self.results, offset, 0))
                        {
                            if let Some(child) = find_child_member(parent_node, &member_str) {
                                return Some(child.offset);
                            }
                        }
                    }
                }
                None
            }
            ExprKind::Index {
                expr: arr,
                index: idx_expr,
            } => {
                // Try scope-based lookup first
                let idx = self.eval_expr(idx_expr).ok()?.to_unsigned().ok()? as usize;
                if let Some(arr_chain) = self.flatten_expr_to_scope_key(arr) {
                    let elem_key_str = format!("{}.[{}]", arr_chain, idx);
                    if let Some(elem_key) = self.interner.lookup(&elem_key_str) {
                        if let Some(Value::SizedRef { offset, .. }) = self.scope.get_var(elem_key) {
                            return Some(*offset);
                        }
                    }
                }
                // Fall back to result tree search
                let arr_val = self.eval_expr_for_addressof(arr).ok()?;
                if let Value::SizedRef { offset, size } = arr_val {
                    let node = find_node_by_offset_deep(&self.results, offset, size)
                        .or_else(|| find_node_by_offset_deep(&self.results, offset, 0))?;
                    if idx < node.children.len() {
                        return Some(node.children[idx].offset);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Helper for resolve_node_offset: evaluate an expression but convert
    /// non-addressable values back to SizedRef by searching the result tree.
    /// Flatten an expression to a scope key, handling Ident, MemberAccess, and Index.
    /// E.g., `parent.string_ids[0].string_data` → "parent.string_ids.[0].string_data"
    pub(crate) fn flatten_expr_to_scope_key(&mut self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Ident(n) => Some(self.interner.resolve(*n).to_string()),
            ExprKind::MemberAccess {
                expr: inner,
                member,
            } => {
                let base = self.flatten_expr_to_scope_key(inner)?;
                Some(format!("{}.{}", base, self.interner.resolve(*member)))
            }
            ExprKind::Index { expr: arr, index } => {
                let base = self.flatten_expr_to_scope_key(arr)?;
                let idx = self.eval_expr(index).ok()?.to_unsigned().ok()?;
                Some(format!("{}.[{}]", base, idx))
            }
            _ => None,
        }
    }

    pub(crate) fn eval_expr_for_addressof(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if let Some(val) = self.scope.get_var(*name) {
                    match val {
                        Value::SizedRef { .. } | Value::Unsigned(_) | Value::Signed(_) => {
                            return Ok(val.clone());
                        }
                        _ => {
                            // Non-addressable (e.g. String from char[]); find in results
                            let name_str = self.interner.resolve(*name);
                            fn find_named_ref(nodes: &[PatternNode], name: &str) -> Option<Value> {
                                for node in nodes {
                                    if node.name == name {
                                        return Some(Value::SizedRef {
                                            offset: node.offset,
                                            size: node.size,
                                        });
                                    }
                                    if let Some(v) = find_named_ref(&node.children, name) {
                                        return Some(v);
                                    }
                                }
                                None
                            }
                            if let Some(v) = find_named_ref(&self.results, name_str) {
                                return Ok(v);
                            }
                        }
                    }
                }
                Err(EvalError::new(format!(
                    "undefined variable '{}'",
                    self.interner.resolve(*name)
                )))
            }
            ExprKind::MemberAccess { expr: obj, member } => {
                let member_str = self.interner.resolve(*member).to_string();
                // Try scope-based lookup with full dotted key (handles Index in chain)
                if let Some(chain) = self.flatten_expr_to_scope_key(obj) {
                    let full = format!("{}.{}", chain, member_str);
                    if let Some(full_key) = self.interner.lookup(&full) {
                        if let Some(val) = self.scope.get_var(full_key) {
                            if let Value::SizedRef { .. } = val {
                                return Ok(val.clone());
                            }
                            // Value exists but is not SizedRef (e.g., String from char[]).
                            // Try __addr_ lookup for the offset.
                            let addr_key_str = format!("__addr_{}", full);
                            let addr_key = self.interner.intern(&addr_key_str);
                            if let Some(addr_val) = self.scope.get_var(addr_key) {
                                if let Value::SizedRef { .. } = addr_val {
                                    return Ok(addr_val.clone());
                                }
                            }
                        }
                    }
                }
                // Fall back to evaluating obj and finding child in result tree
                if let Ok(obj_val) = self.eval_expr_for_addressof(obj) {
                    if let Value::SizedRef { offset, size } = obj_val {
                        if let Some(node) = find_node_by_offset(&self.results, offset, size)
                            .or_else(|| find_node_by_offset(&self.results, offset, 0))
                        {
                            if let Some(child) = find_child_member(node, &member_str) {
                                return Ok(Value::SizedRef {
                                    offset: child.offset,
                                    size: child.size,
                                });
                            }
                        }
                    }
                }
                Err(EvalError::new(format!(
                    "cannot access member '{}'",
                    member_str
                )))
            }
            ExprKind::Index {
                expr: arr,
                index: idx_expr,
            } => {
                // Evaluate index value
                let idx_val = self
                    .eval_expr(idx_expr)
                    .map_err(|_| EvalError::new("cannot evaluate index in addressof"))?;
                let idx = idx_val
                    .to_unsigned()
                    .map_err(|_| EvalError::new("index is not an integer"))?
                    as usize;
                // Try scope-based lookup: construct key like "parent.string_ids.[0]"
                if let Some(arr_chain) = self.flatten_expr_to_scope_key(arr) {
                    let elem_key_str = format!("{}.[{}]", arr_chain, idx);
                    if let Some(elem_key) = self.interner.lookup(&elem_key_str) {
                        if let Some(val) = self.scope.get_var(elem_key) {
                            if let Value::SizedRef { .. } = val {
                                return Ok(val.clone());
                            }
                        }
                    }
                }
                // Fall back to result tree search
                let arr_val = self.eval_expr_for_addressof(arr)?;
                if let Value::SizedRef { offset, size } = arr_val {
                    if let Some(node) = find_node_by_offset_deep(&self.results, offset, size)
                        .or_else(|| find_node_by_offset_deep(&self.results, offset, 0))
                    {
                        if idx < node.children.len() {
                            let child = &node.children[idx];
                            return Ok(Value::SizedRef {
                                offset: child.offset,
                                size: child.size,
                            });
                        }
                    }
                }
                Err(EvalError::new(
                    "addressof expression is not an addressable value",
                ))
            }
            _ => Err(EvalError::new(
                "addressof expression is not an addressable value",
            )),
        }
    }

    // ========== Expression evaluation ==========

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        match &expr.kind {
            ExprKind::IntLiteral(v) => Ok(Value::Unsigned(*v)),
            ExprKind::FloatLiteral(v) => Ok(Value::Float(*v)),
            ExprKind::StringLiteral(v) => Ok(Value::String(v.clone())),
            ExprKind::CharLiteral(v) => Ok(Value::Char(*v)),
            ExprKind::BoolLiteral(v) => Ok(Value::Bool(*v)),
            ExprKind::Null => Ok(Value::Null),

            ExprKind::Dollar => Ok(Value::Unsigned(self.offset as u128)),

            ExprKind::Ident(name) => {
                if let Some(val) = self.scope.get_var(*name) {
                    Ok(val.clone())
                } else {
                    Err(self.make_error(
                        format!("undefined variable '{}'", self.interner.resolve(*name)),
                        expr.span,
                    ))
                }
            }

            ExprKind::ScopedIdent(path) => {
                let full_name = self.resolve_path(path);
                let full_name_key = self.interner.intern(&full_name);
                if let Some(val) = self.scope.get_var(full_name_key) {
                    Ok(val.clone())
                } else if path.len() >= 2 {
                    // Try enum member lookup: last segment is member, rest is type name
                    let member_str = self.interner.resolve(path[path.len() - 1]).to_string();
                    let type_name = self.resolve_path(&path[..path.len() - 1]);
                    match self.resolve_enum_member(&type_name, &member_str, expr.span) {
                        Ok(val) => Ok(val),
                        Err(_) if self.current_namespace.is_some() => {
                            // Try with current namespace prefix (e.g., Type::REPT → fstack::Type::REPT)
                            let qualified = format!(
                                "{}::{}",
                                self.current_namespace.as_ref().unwrap(),
                                type_name
                            );
                            self.resolve_enum_member(&qualified, &member_str, expr.span)
                        }
                        Err(e) => Err(e),
                    }
                } else {
                    Err(self.make_error(format!("undefined '{}'", full_name), expr.span))
                }
            }

            ExprKind::Binary { op, lhs, rhs } => {
                let left = self.eval_expr(lhs)?;
                // Short-circuit for logical operators
                match op {
                    BinOp::LogAnd => {
                        if !left
                            .to_bool()
                            .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))?
                        {
                            return Ok(Value::Bool(false));
                        }
                        let right = self.eval_expr(rhs)?;
                        return Ok(Value::Bool(right.to_bool().map_err(|e| {
                            self.resolve_error(e.with_span_if_none(expr.span))
                        })?));
                    }
                    BinOp::LogOr => {
                        if left
                            .to_bool()
                            .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))?
                        {
                            return Ok(Value::Bool(true));
                        }
                        let right = self.eval_expr(rhs)?;
                        return Ok(Value::Bool(right.to_bool().map_err(|e| {
                            self.resolve_error(e.with_span_if_none(expr.span))
                        })?));
                    }
                    _ => {}
                }

                let right = self.eval_expr(rhs)?;
                self.eval_binary_op(*op, &left, &right)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))
            }

            ExprKind::Unary { op, expr: inner } => {
                let val = self.eval_expr(inner)?;
                match op {
                    UnaryOp::Neg => match &val {
                        Value::Unsigned(v) => Ok(Value::Signed(-(*v as i128))),
                        Value::Signed(v) => Ok(Value::Signed(-v)),
                        Value::Float(v) => Ok(Value::Float(-v)),
                        _ => Err(self.make_error("cannot negate non-numeric value", expr.span)),
                    },
                    UnaryOp::Not => {
                        let b = val
                            .to_bool()
                            .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))?;
                        Ok(Value::Bool(!b))
                    }
                    UnaryOp::BitNot => {
                        let v = val
                            .to_unsigned()
                            .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))?;
                        let bit_width = self.infer_bit_width(inner);
                        let result = if bit_width < 128 {
                            !v & ((1u128 << bit_width) - 1)
                        } else {
                            !v
                        };
                        Ok(Value::Unsigned(result))
                    }
                }
            }

            ExprKind::Call { func, args } => self.eval_call_expr(func, args, expr.span),

            ExprKind::Index { expr: arr, index } => self.eval_expr_index(arr, index, expr.span),

            ExprKind::MemberAccess { expr: obj, member } => {
                self.eval_expr_member_access(obj, *member, expr.span)
            }

            ExprKind::Sizeof(inner) => match inner.as_ref() {
                TypeExprOrExpr::Type(ty) => {
                    let size = self
                        .type_size(ty)
                        .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))?;
                    Ok(Value::Unsigned(size as u128))
                }
                TypeExprOrExpr::Expr(sizeof_expr) => {
                    // sizeof($) returns total data source size (ImHex convention)
                    if matches!(sizeof_expr.kind, ExprKind::Dollar) {
                        return Ok(Value::Unsigned(self.data.size() as u128));
                    }
                    let val = self.eval_expr(sizeof_expr)?;
                    match val {
                        Value::SizedRef { size, .. } | Value::BulkRef { size, .. } => {
                            Ok(Value::Unsigned(size as u128))
                        }
                        Value::String(s) => Ok(Value::Unsigned(s.len() as u128)),
                        Value::Array(items) => Ok(Value::Unsigned(items.len() as u128)),
                        other => self
                            .value_size(&other)
                            .map(|s| Value::Unsigned(s as u128))
                            .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span))),
                    }
                }
            },

            ExprKind::Addressof(inner) => self.eval_expr_addressof(inner),

            ExprKind::Cast { ty, expr: inner } => {
                let val = self.eval_expr(inner)?;
                self.cast_value(&val, ty)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))
            }

            ExprKind::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_val = self.eval_expr(cond)?;
                if cond_val
                    .to_bool()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(expr.span)))?
                {
                    self.eval_expr(then_expr)
                } else {
                    self.eval_expr(else_expr)
                }
            }

            ExprKind::Assign { op, lhs, rhs } => self.eval_expr_assign(op, lhs, rhs, expr.span),

            ExprKind::InitializerList(elements) => {
                let values = elements
                    .iter()
                    .map(|e| self.eval_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Array(values))
            }
        }
    }

    fn eval_expr_index(
        &mut self,
        arr: &Expr,
        index: &Expr,
        span: Span,
    ) -> Result<Value, EvalError> {
        // Handle padding[size]: skip N bytes of padding
        if let ExprKind::Ident(ref name) = arr.kind {
            if self.interner.resolve(*name) == "padding" {
                let idx_val = self.eval_expr(index)?;
                let skip_bytes = idx_val
                    .to_unsigned()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(span)))?
                    as u64;
                // Guard against unsigned underflow from negative subtraction
                // (e.g., padding[end - $] where $ > end wraps to huge value)
                let remaining = self.data.size().saturating_sub(self.offset);
                self.offset += skip_bytes.min(remaining);
                return Ok(Value::Null);
            }
        }
        let arr_val = self.eval_expr(arr)?;
        let idx_val = self.eval_expr(index)?;
        let idx = idx_val
            .to_unsigned()
            .map_err(|e| self.resolve_error(e.with_span_if_none(span)))? as usize;
        match arr_val {
            Value::String(ref s) => {
                if let Some(ch) = s.chars().nth(idx) {
                    Ok(Value::Char(ch))
                } else {
                    Err(self.make_error(
                        format!(
                            "string index {} out of bounds (length {})",
                            idx,
                            s.chars().count()
                        ),
                        span,
                    ))
                }
            }
            Value::Array(ref items) => Ok(items.get(idx).cloned().unwrap_or(Value::Unsigned(0))),
            Value::BulkRef {
                ref data,
                ref elem_type,
                endian,
                ..
            } => {
                // Direct decode from bulk data buffer — no node lookup needed
                let elem_size = elem_type.size().unwrap_or(1) as usize;
                let byte_offset = idx * elem_size;
                if byte_offset + elem_size <= data.len() {
                    Ok(decode_bulk_element(
                        &data[byte_offset..byte_offset + elem_size],
                        elem_type,
                        endian,
                    ))
                } else {
                    Ok(Value::Unsigned(0))
                }
            }
            Value::SizedRef { offset, size } => {
                // Extract action from immutable borrow of self.results, then release borrow
                enum NodeAction {
                    BulkDecode(Value),
                    LazyStruct {
                        type_name: String,
                        elem_size: u64,
                        endian: Endianness,
                        node_offset: u64,
                    },
                    ChildValue(Value),
                    NotFound,
                }
                let action = if let Some(n) = find_node_by_offset(&self.results, offset, size)
                    .or_else(|| find_node_by_offset(&self.results, offset, 0))
                {
                    if let PatternValue::BulkData {
                        ref data,
                        ref elem_type,
                        endian,
                    } = n.value
                    {
                        let es = elem_type.size().unwrap_or(1) as usize;
                        let byte_offset = idx * es;
                        if byte_offset + es <= data.len() {
                            NodeAction::BulkDecode(decode_bulk_element(
                                &data[byte_offset..byte_offset + es],
                                elem_type,
                                endian,
                            ))
                        } else {
                            NodeAction::BulkDecode(Value::Unsigned(0))
                        }
                    } else if let PatternValue::LazyStructArray {
                        ref type_name,
                        elem_size,
                        count,
                        endian: lazy_endian,
                    } = n.value
                    {
                        if (idx as u64) < count {
                            let elem_offset = n.offset + idx as u64 * elem_size;
                            // Check if already materialized in results
                            if let Some(existing) =
                                find_node_by_offset(&self.results, elem_offset, elem_size)
                            {
                                NodeAction::ChildValue(value_from_node(existing))
                            } else {
                                NodeAction::LazyStruct {
                                    type_name: type_name.clone(),
                                    elem_size,
                                    endian: lazy_endian,
                                    node_offset: n.offset,
                                }
                            }
                        } else {
                            NodeAction::BulkDecode(Value::Unsigned(0))
                        }
                    } else if idx < n.children.len() {
                        NodeAction::ChildValue(value_from_node(&n.children[idx]))
                    } else {
                        NodeAction::BulkDecode(Value::Unsigned(0))
                    }
                } else {
                    NodeAction::NotFound
                };
                // self.results borrow is released — safe to use &mut self
                match action {
                    NodeAction::BulkDecode(val) | NodeAction::ChildValue(val) => Ok(val),
                    NodeAction::LazyStruct {
                        type_name,
                        elem_size,
                        endian: lazy_endian,
                        node_offset,
                    } => {
                        let elem_offset = node_offset + idx as u64 * elem_size;
                        let elem_name = format!("[{}]", idx);
                        let parts: Vec<Name> = type_name
                            .split("::")
                            .map(|s| self.interner.intern(s))
                            .collect();
                        let type_expr = TypeExpr {
                            kind: TypeExprKind::Named(parts),
                            span: Span::dummy(),
                        };
                        let (elem_node, _) = self.read_type(
                            &type_expr,
                            &elem_name,
                            elem_offset,
                            lazy_endian,
                            &[],
                        )?;
                        let val = value_from_node(&elem_node);
                        self.results.push(elem_node);
                        Ok(val)
                    }
                    NodeAction::NotFound => {
                        // Fallback: try scope-based lookup using the source expression name.
                        // propagate_children creates "arr.[0]", "arr.[1]", etc. in scope.
                        if let Some(arr_name) = flatten_member_chain(arr, &self.interner) {
                            let scope_key = format!("{}.[{}]", arr_name, idx);
                            let scope_name = self.interner.intern(&scope_key);
                            if let Some(val) = self.scope.get_var(scope_name) {
                                return Ok(val.clone());
                            }
                        }
                        Err(self.make_error(
                            format!("cannot find node at offset {:#x} size {}", offset, size),
                            span,
                        ))
                    }
                }
            }
            // $[n] pattern: read a byte from binary data at absolute offset n.
            // In ImHex, $[n] reads the byte at file offset n regardless of $.
            Value::Unsigned(_) | Value::Signed(_) => {
                let read_offset = idx as u64;
                if read_offset < self.data.size() {
                    let bytes = self
                        .data
                        .read_bytes(read_offset, 1)
                        .map_err(|e| self.resolve_error(e))?;
                    Ok(Value::Unsigned(bytes[0] as u128))
                } else {
                    // Out-of-bounds $[n] read returns 0 (ImHex tolerant behavior)
                    Ok(Value::Unsigned(0))
                }
            }
            _ => Err(self.make_error(format!("indexing not supported for {:?}", arr_val), span)),
        }
    }

    fn eval_expr_member_access(
        &mut self,
        obj: &Expr,
        member: Name,
        span: Span,
    ) -> Result<Value, EvalError> {
        let member_str = self.interner.resolve(member).to_string();
        if let Some(chain) = flatten_member_chain(obj, &self.interner) {
            let full = format!("{}.{}", chain, member_str);
            let full_key = self.interner.intern(&full);
            if let Some(val) = self.scope.get_var(full_key) {
                return Ok(val.clone());
            }
            // this.member → member (struct fields are registered directly)
            if let Some(stripped) = full.strip_prefix("this.") {
                let stripped_key = self.interner.intern(stripped);
                if let Some(val) = self.scope.get_var(stripped_key) {
                    return Ok(val.clone());
                }
            }
        }
        // Fallback: try simple obj.member lookup
        if let ExprKind::Ident(obj_name) = &obj.kind {
            let qualified = format!("{}.{}", self.interner.resolve(*obj_name), member_str);
            let qualified_key = self.interner.intern(&qualified);
            if let Some(val) = self.scope.get_var(qualified_key) {
                return Ok(val.clone());
            }
        }
        // Fallback: try prefix-based scope lookup (e.g., "parent.parent" + ".header")
        // This handles chains like parent.parent.header.xxx where intermediate
        // values are not actual PatternNodes but scope variable prefixes.
        if let Some(chain) = flatten_member_chain(obj, &self.interner) {
            let prefix_vars = self.scope.get_vars_with_prefix(&chain, &self.interner);
            // Look for the member directly in the prefix vars
            if let Some((_, val)) = prefix_vars.iter().find(|(suffix, _)| suffix == &member_str) {
                return Ok(val.clone());
            }
        }

        // Try scope-based array element member access: arr[i].member → "arr.[i].member" in scope
        // This handles cases where propagate_children created dotted variables for array elements
        // (e.g., "arguments.[3].varType" or "parent.entries.[0].id")
        // Also handles nested chains with Index: e.g., arr[i].ptr.field
        if let Some(obj_chain) = self.flatten_expr_to_scope_key(obj) {
            let scope_key = format!("{}.{}", obj_chain, member_str);
            let scope_name = self.interner.intern(&scope_key);
            if let Some(val) = self.scope.get_var(scope_name) {
                return Ok(val.clone());
            }
        }

        // Evaluate obj and try SizedRef-based member lookup
        let obj_val = self.eval_expr(obj)?;
        if let Value::SizedRef { offset, size } = obj_val {
            // Try exact match first, then offset-only match
            if let Some(n) = find_node_by_offset(&self.results, offset, size)
                .or_else(|| find_node_by_offset(&self.results, offset, 0))
            {
                // Direct child lookup
                if let Some(child) = n.children.iter().find(|c| c.name.as_str() == member_str) {
                    return Ok(value_from_node(child));
                }
                // Pointer auto-dereference: follow pointer address to find target node
                if let PatternValue::Pointer { address } = &n.value {
                    let addr = *address;
                    if let Some(target_node) = find_node_by_offset(&self.results, addr, 0) {
                        if let Some(child) = target_node
                            .children
                            .iter()
                            .find(|c| c.name.as_str() == member_str)
                        {
                            return Ok(value_from_node(child));
                        }
                    }
                }
            }
        }
        // Graceful degradation: if obj was an Index expression and the result is
        // not a SizedRef (e.g., array was OOB-truncated returning Unsigned(0)),
        // return 0 instead of erroring for ImHex compatibility.
        if let ExprKind::Index { .. } = &obj.kind {
            if matches!(obj_val, Value::Unsigned(0) | Value::Null) {
                return Ok(Value::Unsigned(0));
            }
        }
        Err(self.make_error(format!("cannot access member '{}'", member_str), span))
    }

    fn eval_expr_addressof(&mut self, inner: &Expr) -> Result<Value, EvalError> {
        let val = self.eval_expr(inner)?;
        match val {
            Value::SizedRef { offset, .. } | Value::BulkRef { offset, .. } => {
                Ok(Value::Unsigned(offset as u128))
            }
            ref other => {
                // Value is not directly addressable (e.g. char[] stored as String,
                // or Unsigned/Signed from scalar member access).
                // Try scope __addr_ lookup (for struct body members)
                if let Some(chain) = flatten_member_chain(inner, &self.interner) {
                    let addr_key_str = format!("__addr_{}", chain);
                    let addr_key = self.interner.intern(&addr_key_str);
                    if let Some(Value::SizedRef { offset, .. }) = self.scope.get_var(addr_key) {
                        return Ok(Value::Unsigned(*offset as u128));
                    }
                }
                // Try __addr_ with flatten_expr_to_scope_key (handles Index in chain)
                if let Some(key) = self.flatten_expr_to_scope_key(inner) {
                    let addr_key_str = format!("__addr_{}", key);
                    let addr_key = self.interner.intern(&addr_key_str);
                    if let Some(Value::SizedRef { offset, .. }) = self.scope.get_var(addr_key) {
                        return Ok(Value::Unsigned(*offset as u128));
                    }
                }
                // Try eval_expr_for_addressof which uses scope + result tree navigation
                if let Ok(Value::SizedRef { offset, .. }) = self.eval_expr_for_addressof(inner) {
                    return Ok(Value::Unsigned(offset as u128));
                }
                // Try to find the corresponding node offset via the expression path.
                if let Some(offset) = self.resolve_node_offset(inner) {
                    return Ok(Value::Unsigned(offset as u128));
                }
                // Fall back to eval_expr value for simple cases like addressof(this)
                match other {
                    Value::Unsigned(v) => Ok(Value::Unsigned(*v)),
                    Value::Signed(v) => Ok(Value::Unsigned(*v as u128)),
                    _ => Err(self.make_error(
                        "addressof expression is not an addressable value",
                        inner.span,
                    )),
                }
            }
        }
    }

    fn eval_expr_assign(
        &mut self,
        op: &AssignOp,
        lhs: &Expr,
        rhs: &Expr,
        span: Span,
    ) -> Result<Value, EvalError> {
        let rhs_val = self.eval_expr(rhs)?;
        let final_val = match op {
            AssignOp::Assign => rhs_val,
            _ => {
                let lhs_val = self.eval_expr(lhs)?;
                let bin_op = match op {
                    AssignOp::AddAssign => BinOp::Add,
                    AssignOp::SubAssign => BinOp::Sub,
                    AssignOp::MulAssign => BinOp::Mul,
                    AssignOp::DivAssign => BinOp::Div,
                    AssignOp::ModAssign => BinOp::Mod,
                    AssignOp::BitAndAssign => BinOp::BitAnd,
                    AssignOp::BitOrAssign => BinOp::BitOr,
                    AssignOp::BitXorAssign => BinOp::BitXor,
                    AssignOp::ShlAssign => BinOp::Shl,
                    AssignOp::ShrAssign => BinOp::Shr,
                    AssignOp::Assign => unreachable!(),
                };
                self.eval_binary_op(bin_op, &lhs_val, &rhs_val)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(span)))?
            }
        };

        // Store the value
        match &lhs.kind {
            ExprKind::Ident(name) => {
                self.scope
                    .set_var(*name, final_val.clone(), &self.interner)
                    .map_err(|e| self.resolve_error(e.with_span_if_none(span)))?;
                // Section write-through: update section buffer on assignment
                if let Some((handle, offset, size)) = self.section_vars.get(name).copied() {
                    self.write_section_value(handle, offset, size, &final_val);
                }
            }
            ExprKind::Dollar => {
                let new_offset = final_val
                    .to_unsigned()
                    .map_err(|e| self.resolve_error(e.with_span_if_none(span)))?;
                self.offset = new_offset as u64;
            }
            ExprKind::Index {
                expr: arr_expr,
                index: idx_expr,
            } => {
                if let ExprKind::Ident(arr_name) = &arr_expr.kind {
                    let idx = self
                        .eval_expr(idx_expr)?
                        .to_unsigned()
                        .map_err(|e| self.resolve_error(e.with_span_if_none(span)))?
                        as usize;
                    self.scope
                        .set_var_element(*arr_name, idx, final_val.clone(), &self.interner)
                        .map_err(|e| self.resolve_error(e.with_span_if_none(span)))?;
                } else if let Some(chain) = self.flatten_expr_to_scope_key(lhs) {
                    // Complex index target: obj.arr[idx] = val
                    let chain_key = self.interner.intern(&chain);
                    match self
                        .scope
                        .set_var(chain_key, final_val.clone(), &self.interner)
                    {
                        Ok(()) => {}
                        Err(_) => {
                            self.scope.define_var(chain_key, final_val.clone(), false);
                        }
                    }
                } else {
                    return Err(self.make_error("invalid assignment target", span));
                }
            }
            ExprKind::MemberAccess { .. } => {
                // Support dotted assignment: obj.field = val
                // Also handles complex chains like obj.arr[idx].field = val
                if let Some(chain) = self.flatten_expr_to_scope_key(lhs) {
                    let chain_key = self.interner.intern(&chain);
                    // Try to set existing variable
                    match self
                        .scope
                        .set_var(chain_key, final_val.clone(), &self.interner)
                    {
                        Ok(()) => {}
                        Err(_) => {
                            // Define as new variable
                            self.scope.define_var(chain_key, final_val.clone(), false);
                        }
                    }
                } else {
                    return Err(self.make_error("invalid assignment target", span));
                }
            }
            _ => {
                return Err(self.make_error("invalid assignment target", span));
            }
        }
        Ok(final_val)
    }

    /// Execute loop body statements within a new scope.
    /// Returns LoopAction indicating what the loop caller should do.
    pub(crate) fn exec_loop_body(&mut self, body: &[Stmt]) -> Result<LoopAction, EvalError> {
        self.scope.push();
        let action = self.exec_loop_body_inner(body);
        self.scope.pop();
        action
    }

    pub(crate) fn exec_loop_body_inner(&mut self, body: &[Stmt]) -> Result<LoopAction, EvalError> {
        for s in body {
            match self.eval_stmt(s)? {
                ControlFlow::None => {}
                ControlFlow::Break => return Ok(LoopAction::Break),
                ControlFlow::Continue => return Ok(LoopAction::Continue),
                cf @ ControlFlow::Return(_) => return Ok(LoopAction::Return(cf)),
            }
        }
        Ok(LoopAction::Continue)
    }

    /// Execute a for loop body. Caller is responsible for push/pop of scope.
    pub(crate) fn eval_for_loop(
        &mut self,
        init: &[Stmt],
        cond: &Expr,
        step: &Stmt,
        body: &[Stmt],
        span: Span,
    ) -> Result<ControlFlow, EvalError> {
        for s in init {
            self.eval_stmt(s)?;
        }
        let mut iterations = 0u64;
        loop {
            iterations += 1;
            self.check_resource_limits(span)?;
            if self.max_pattern_limit > 0 && iterations > self.max_pattern_limit {
                return Err(self.make_error(
                    format!(
                        "for loop iteration limit exceeded (max {})",
                        self.max_pattern_limit
                    ),
                    span,
                ));
            }
            let cond_val = match self.eval_expr(cond) {
                Ok(v) => v,
                Err(e) if e.is_read_oob => break,
                Err(e) => return Err(e),
            };
            if !cond_val
                .to_bool()
                .map_err(|e| self.resolve_error(e.with_span_if_none(span)))?
            {
                break;
            }
            match self.exec_loop_body(body) {
                Ok(LoopAction::Continue) => {}
                Ok(LoopAction::Break) => break,
                Ok(LoopAction::Return(cf)) => return Ok(cf),
                Err(e) if e.is_read_oob => break,
                Err(e) => return Err(e),
            }
            match self.eval_stmt(step) {
                Ok(_) => {}
                Err(e) if e.is_read_oob => break,
                Err(e) => return Err(e),
            }
        }
        Ok(ControlFlow::None)
    }

    /// Infer the bit width of an expression for bitwise operations.
    /// Returns 128 if the width cannot be determined.
    pub(crate) fn infer_bit_width(&self, expr: &Expr) -> u32 {
        match &expr.kind {
            ExprKind::Ident(name) => {
                // Check if the variable was read from data (has a PatternNode)
                if let Some(Value::SizedRef { size, .. } | Value::BulkRef { size, .. }) =
                    self.scope.get_var(*name)
                {
                    return (*size as u32) * 8;
                }
                // Try to infer from builtin type name used as variable type
                let name_str = self.interner.resolve(*name);
                if let Some(bt) = BuiltinType::from_str(name_str) {
                    if let Some(s) = bt.size() {
                        return (s as u32) * 8;
                    }
                }
                128
            }
            ExprKind::Call { func, .. } => {
                // Type-cast calls like u8(...), u16(...) — infer from the type name
                if let ExprKind::Ident(name) = &func.kind {
                    let name_str = self.interner.resolve(*name);
                    if let Some(bt) = BuiltinType::from_str(name_str) {
                        if let Some(s) = bt.size() {
                            return (s as u32) * 8;
                        }
                    }
                    if let Some((_, bits)) = parse_arbitrary_int_type(name_str) {
                        return bits as u32;
                    }
                }
                128
            }
            ExprKind::IntLiteral(_) => {
                // Integer literals: conservatively use 128 bits
                128
            }
            _ => 128,
        }
    }

    /// Evaluate a function call expression
    pub(crate) fn eval_call_expr(
        &mut self,
        func: &Expr,
        args: &[Expr],
        span: Span,
    ) -> Result<Value, EvalError> {
        let func_name: String = match &func.kind {
            ExprKind::Ident(name) => self.interner.resolve(*name).to_string(),
            ExprKind::ScopedIdent(path) => self.resolve_path(path),
            _ => {
                return Err(self.make_error("cannot call non-function value", span));
            }
        };

        // Evaluate arguments
        let arg_vals: Vec<Value> = args
            .iter()
            .map(|arg| self.eval_expr(arg))
            .collect::<Result<_, _>>()?;

        // Handle type-cast calls like u8(expr), u16(expr), etc.
        // Only if there is no user-defined function with the same name.
        let func_name_key = self.interner.intern(&func_name);
        if self.scope.get_fn(func_name_key).is_none() {
            if let Some(builtin) = BuiltinType::from_str(&func_name) {
                if let Some(arg) = arg_vals.first() {
                    let ty = TypeExpr {
                        kind: TypeExprKind::Builtin(builtin),
                        span: func.span.clone(),
                    };
                    return self
                        .cast_value(arg, &ty)
                        .map_err(|e| self.resolve_error(e.with_span_if_none(span)));
                }
            }
        }

        // Check user-defined functions first (so .pat includes override builtins)
        if let Some(func_def) = self.scope.get_fn(func_name_key) {
            // Capture calling context before push (for parent keyword in functions)
            let this_key = self.interner.intern("this");
            let func_parent_ctx: Option<Vec<(String, Value)>> = self
                .scope
                .get_var(this_key)
                .is_some()
                .then(|| self.scope.enclosing_struct_vars(this_key, &self.interner));

            self.scope.push();

            // Set up parent context for function (ImHex semantics: parent = calling struct)
            if let Some(caller_vars) = &func_parent_ctx {
                if let Some((_, this_val)) = caller_vars.iter().find(|(k, _)| k == "this") {
                    let parent_key = self.interner.intern("parent");
                    self.scope.define_var(parent_key, this_val.clone(), false);
                }
                for (name, val) in caller_vars {
                    // Skip deep parent chains to prevent exponential growth
                    if name == "this" || name.starts_with("parent.parent") {
                        continue;
                    }
                    // caller's field → parent.field
                    // caller's parent → parent.parent
                    // caller's parent.xxx → parent.parent.xxx
                    let dotted = format!("parent.{}", name);
                    let dotted_key = self.interner.intern(&dotted);
                    self.scope.define_var(dotted_key, val.clone(), false);
                }
            }

            for (i, (param_name, _, default_expr, is_variadic)) in
                func_def.params.iter().enumerate()
            {
                let val = if *is_variadic {
                    // Collect all remaining arguments into an array
                    Value::Array(arg_vals[i..].to_vec())
                } else if let Some(v) = arg_vals.get(i) {
                    v.clone()
                } else if let Some(def_expr) = default_expr {
                    self.eval_expr(def_expr)?
                } else {
                    Value::Null
                };
                // If param is a SizedRef, propagate children for member access
                if let Value::SizedRef { offset, size } = &val {
                    let param_name_str = self.interner.resolve(*param_name).to_string();
                    let mut propagated = false;
                    if let Some(node) = find_node_by_offset(&self.results, *offset, *size) {
                        propagate_children(
                            &mut self.scope,
                            &mut self.interner,
                            &param_name_str,
                            &node.children,
                        );
                        propagated = true;
                    }
                    // Fallback: copy dotted-name vars from argument source name
                    if !propagated {
                        if let Some(arg_expr) = args.get(i) {
                            if let Some(src_str) = flatten_member_chain(arg_expr, &self.interner) {
                                let children =
                                    self.scope.get_vars_with_prefix(&src_str, &self.interner);
                                for (suffix, child_val) in children {
                                    let dotted = format!("{}.{}", param_name_str, suffix);
                                    let dotted_key = self.interner.intern(&dotted);
                                    self.scope.define_var(dotted_key, child_val, false);
                                }
                            }
                        }
                    }
                }
                self.scope.define_var(*param_name, val, false);
            }

            let mut return_val = Value::Null;
            for stmt in &func_def.body {
                match self.eval_stmt(stmt)? {
                    ControlFlow::Return(val) => {
                        return_val = val.unwrap_or(Value::Null);
                        break;
                    }
                    ControlFlow::None => {}
                    _ => break,
                }
            }

            self.scope.pop();
            return Ok(return_val);
        }

        // Handle data-dependent builtins that need access to self.data
        let stripped = func_name.strip_prefix("builtin::").unwrap_or(&func_name);
        if let Some(result) = self.eval_data_builtin(stripped, &arg_vals, span)? {
            return Ok(result);
        }

        // Check scope builtins
        if let Some(result) = self.scope.call_builtin(func_name_key, &arg_vals) {
            return result.map_err(|e| self.resolve_error(e.with_span_if_none(span)));
        }
        // Also check without builtin:: prefix
        if func_name.starts_with("builtin::") {
            let stripped_key = self.interner.intern(stripped);
            if let Some(result) = self.scope.call_builtin(stripped_key, &arg_vals) {
                return result.map_err(|e| self.resolve_error(e.with_span_if_none(span)));
            }
        }
        // Backward compat: if called without builtin:: prefix, try with it
        if !func_name.starts_with("builtin::") {
            let builtin_name = format!("builtin::{}", func_name);
            let builtin_key = self.interner.intern(&builtin_name);
            if let Some(result) = self.scope.call_builtin(builtin_key, &arg_vals) {
                return result.map_err(|e| self.resolve_error(e.with_span_if_none(span)));
            }
        }

        // Special built-in: std::io::print
        if stripped == "std::print" || stripped == "print" || stripped == "std::io::print" {
            let msg: String = arg_vals
                .iter()
                .map(|v| v.to_display_string())
                .collect::<Vec<_>>()
                .join(" ");
            self.print_output.push(msg);
            return Ok(Value::Null);
        }

        // Silently ignore unknown builtins instead of erroring
        if func_name.starts_with("builtin::") || func_name.starts_with("std::") {
            return Ok(Value::Null);
        }

        Err(self.make_error(format!("undefined function '{}'", func_name), span))
    }

    /// Evaluate data-dependent builtin functions (std::mem::*, std::string::*, etc.)
    /// Returns Ok(Some(value)) if handled, Ok(None) if not a data builtin.
    pub(crate) fn eval_data_builtin(
        &mut self,
        name: &str,
        args: &[Value],
        span: Span,
    ) -> Result<Option<Value>, EvalError> {
        match name {
            "std::mem::size" => Ok(Some(Value::Unsigned(self.data.size() as u128))),
            "std::mem::read_unsigned" => self.builtin_mem_read_unsigned(args).map(Some),
            "std::mem::read_signed" => self.builtin_mem_read_signed(args).map(Some),
            "std::mem::base_address" => Ok(Some(Value::Unsigned(0))),
            "std::mem::eof" => Ok(Some(Value::Bool(self.offset >= self.data.size()))),
            "std::mem::find_sequence_in_range" => self.builtin_mem_find_sequence(args).map(Some),
            "std::mem::find_string_in_range" => self.builtin_mem_find_string(args).map(Some),
            "std::mem::read_string" => self.builtin_mem_read_string(args).map(Some),
            "std::string::length" => Ok(Some(self.builtin_string_length(args))),
            "std::assert" | "std::sys::assert" => self.builtin_assert(args, span).map(Some),
            "std::ctype::isprint" => Ok(Some(self.builtin_ctype_isprint(args))),
            "std::format" => Ok(Some(self.builtin_format(args))),
            "std::core::set_endian" => {
                // set_endian(u32): 0=Native(Little), 1=Big, 2=Little
                let val = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0);
                let endian = match val {
                    1 => Endianness::Big,
                    _ => Endianness::Little,
                };
                self.default_endian = endian;
                Ok(Some(Value::Null))
            }
            "std::core::get_endian" => {
                let val = match self.default_endian {
                    Endianness::Big => 1u128,
                    Endianness::Little => 2u128,
                };
                Ok(Some(Value::Unsigned(val)))
            }
            "std::mem::create_section" => {
                let id = self.next_section_id;
                self.next_section_id += 1;
                self.sections.insert(id, Vec::new());
                Ok(Some(Value::Unsigned(id)))
            }
            "std::mem::delete_section" => {
                if let Some(handle) = args.first().and_then(|v| v.to_unsigned().ok()) {
                    self.sections.remove(&handle);
                }
                Ok(Some(Value::Null))
            }
            "std::mem::get_section_size" => {
                let handle = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0);
                let size = self.sections.get(&handle).map(|s| s.len()).unwrap_or(0);
                Ok(Some(Value::Unsigned(size as u128)))
            }
            "std::mem::set_section_size" => {
                let handle = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0);
                let size = args.get(1).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
                if let Some(section) = self.sections.get_mut(&handle) {
                    section.resize(size, 0);
                }
                Ok(Some(Value::Null))
            }
            "std::mem::copy_section_to_section" => {
                // copy_section_to_section(from_section, from_addr, to_section, to_addr, size)
                let from_handle = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0);
                let from_addr =
                    args.get(1).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
                let to_handle = args.get(2).and_then(|v| v.to_unsigned().ok()).unwrap_or(0);
                let to_addr = args.get(3).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
                let copy_size =
                    args.get(4).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
                // Read source bytes
                let src_bytes = if let Some(src) = self.sections.get(&from_handle) {
                    let end = (from_addr + copy_size).min(src.len());
                    if from_addr < end {
                        src[from_addr..end].to_vec()
                    } else {
                        vec![0u8; copy_size]
                    }
                } else {
                    vec![0u8; copy_size]
                };
                // Write to dest
                if let Some(dst) = self.sections.get_mut(&to_handle) {
                    let end = to_addr + src_bytes.len();
                    if dst.len() < end {
                        dst.resize(end, 0);
                    }
                    dst[to_addr..to_addr + src_bytes.len()].copy_from_slice(&src_bytes);
                }
                Ok(Some(Value::Null))
            }
            "std::mem::copy_value_to_section" => {
                // copy_value_to_section(value, section, addr, size)
                let value = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0);
                let handle = args.get(1).and_then(|v| v.to_unsigned().ok()).unwrap_or(0);
                let addr = args.get(2).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
                let size = args.get(3).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
                if let Some(section) = self.sections.get_mut(&handle) {
                    let end = addr + size;
                    if section.len() < end {
                        section.resize(end, 0);
                    }
                    let bytes = value.to_le_bytes();
                    for i in 0..size {
                        section[addr + i] = if i < bytes.len() { bytes[i] } else { 0 };
                    }
                }
                Ok(Some(Value::Null))
            }
            _ => Ok(None),
        }
    }

    pub(crate) fn builtin_mem_read_unsigned(&self, args: &[Value]) -> Result<Value, EvalError> {
        let addr = args
            .first()
            .map(|v| v.to_unsigned().unwrap_or(0))
            .unwrap_or(0) as u64;
        let size = args
            .get(1)
            .map(|v| v.to_unsigned().unwrap_or(1))
            .unwrap_or(1) as u64;
        let endian = self.parse_endian_arg(args.get(2));
        if size == 0
            || addr
                .checked_add(size)
                .map_or(true, |end| end > self.data.size())
        {
            return Ok(Value::Unsigned(0));
        }
        let bytes = self.data.read_bytes(addr, size)?;
        Ok(Value::Unsigned(bytes_to_unsigned(&bytes, endian)))
    }

    pub(crate) fn builtin_mem_read_signed(&self, args: &[Value]) -> Result<Value, EvalError> {
        let addr = args
            .first()
            .map(|v| v.to_unsigned().unwrap_or(0))
            .unwrap_or(0) as u64;
        let size = args
            .get(1)
            .map(|v| v.to_unsigned().unwrap_or(1))
            .unwrap_or(1) as u64;
        let endian = self.parse_endian_arg(args.get(2));
        if size == 0
            || addr
                .checked_add(size)
                .map_or(true, |end| end > self.data.size())
        {
            return Ok(Value::Signed(0));
        }
        let bytes = self.data.read_bytes(addr, size)?;
        let raw = bytes_to_unsigned(&bytes, endian);
        let bits = size * 8;
        let sign_bit = 1u128 << (bits - 1);
        let val = if raw & sign_bit != 0 {
            let mask = if bits >= 128 {
                u128::MAX
            } else {
                (1u128 << bits) - 1
            };
            (raw | !mask) as i128
        } else {
            raw as i128
        };
        Ok(Value::Signed(val))
    }

    pub(crate) fn builtin_mem_find_sequence(&self, args: &[Value]) -> Result<Value, EvalError> {
        let occurrence = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
        let from = args.get(1).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as u64;
        let to = args
            .get(2)
            .and_then(|v| v.to_unsigned().ok())
            .unwrap_or(self.data.size() as u128) as u64;
        let to = to.min(self.data.size());
        let bytes: Vec<u8> = flatten_args(&args[3..])
            .iter()
            .filter_map(|v| v.to_unsigned().ok().map(|n| n as u8))
            .collect();
        if bytes.is_empty() || from >= to {
            return Ok(Value::Signed(-1));
        }
        let data = self.data.read_bytes(from, to - from)?;
        let mut count = 0usize;
        for i in 0..data.len().saturating_sub(bytes.len() - 1) {
            if data[i..i + bytes.len()] == bytes[..] {
                if count == occurrence {
                    return Ok(Value::Unsigned((from + i as u64) as u128));
                }
                count += 1;
            }
        }
        Ok(Value::Signed(-1))
    }

    /// find_string_in_range(occurrence, start, end, pattern_string)
    /// Searches for a byte pattern (given as a string) in the data.
    pub(crate) fn builtin_mem_find_string(&self, args: &[Value]) -> Result<Value, EvalError> {
        let occurrence = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as usize;
        let from = args.get(1).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as u64;
        let to = args
            .get(2)
            .and_then(|v| v.to_unsigned().ok())
            .unwrap_or(self.data.size() as u128) as u64;
        let to = to.min(self.data.size());
        // Pattern is a string argument (bytes of the string are the search pattern)
        let bytes: Vec<u8> = match args.get(3) {
            Some(Value::String(s)) => s.bytes().collect(),
            Some(v) => {
                // Fallback: treat as sequence of byte args like find_sequence_in_range
                flatten_args(&args[3..])
                    .iter()
                    .filter_map(|v| v.to_unsigned().ok().map(|n| n as u8))
                    .collect()
            }
            None => return Ok(Value::Signed(-1)),
        };
        if bytes.is_empty() || from >= to {
            return Ok(Value::Signed(-1));
        }
        let data = self.data.read_bytes(from, to - from)?;
        let mut count = 0usize;
        for i in 0..data.len().saturating_sub(bytes.len() - 1) {
            if data[i..i + bytes.len()] == bytes[..] {
                if count == occurrence {
                    return Ok(Value::Unsigned((from + i as u64) as u128));
                }
                count += 1;
            }
        }
        Ok(Value::Signed(-1))
    }

    pub(crate) fn builtin_mem_read_string(&self, args: &[Value]) -> Result<Value, EvalError> {
        let addr = args.first().and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as u64;
        let size = args.get(1).and_then(|v| v.to_unsigned().ok()).unwrap_or(0) as u64;
        if size == 0
            || addr
                .checked_add(size)
                .map_or(true, |end| end > self.data.size())
        {
            return Ok(Value::String(String::new()));
        }
        let bytes = self.data.read_bytes(addr, size)?;
        // Latin-1 decoding: each byte maps directly to a Unicode code point
        let s: String = bytes.iter().map(|&b| b as char).collect();
        Ok(Value::String(s))
    }

    pub(crate) fn builtin_string_length(&self, args: &[Value]) -> Value {
        if let Some(Value::String(s)) = args.first() {
            // Use chars().count() because pattern language strings
            // may contain non-ASCII bytes stored as Unicode chars.
            Value::Unsigned(s.chars().count() as u128)
        } else {
            Value::Unsigned(0)
        }
    }

    pub(crate) fn builtin_assert(&self, args: &[Value], span: Span) -> Result<Value, EvalError> {
        if let Some(cond) = args.first() {
            let ok = cond.to_bool().unwrap_or(false);
            if !ok {
                let msg = args
                    .get(1)
                    .map(|v| v.to_display_string())
                    .unwrap_or_else(|| "assertion failed".to_string());
                return Err(self.make_error(format!("assert: {}", msg), span));
            }
        }
        Ok(Value::Null)
    }

    pub(crate) fn builtin_ctype_isprint(&self, args: &[Value]) -> Value {
        if let Some(val) = args.first() {
            let c = match val {
                Value::Char(ch) => *ch as u8,
                Value::Unsigned(v) => *v as u8,
                Value::Signed(v) => *v as u8,
                _ => return Value::Bool(false),
            };
            Value::Bool(c >= 0x20 && c <= 0x7E)
        } else {
            Value::Bool(false)
        }
    }

    pub(crate) fn builtin_format(&self, args: &[Value]) -> Value {
        if let Some(Value::String(fmt)) = args.first() {
            let mut result = String::new();
            let mut arg_idx = 1;
            let chars: Vec<char> = fmt.chars().collect();
            let mut i = 0;
            while i < chars.len() {
                if chars[i] == '{' && i + 1 < chars.len() {
                    if let Some(end) = chars[i..].iter().position(|&c| c == '}') {
                        let spec: String = chars[i + 1..i + end].iter().collect();
                        let val = args.get(arg_idx).cloned().unwrap_or(Value::Null);
                        arg_idx += 1;
                        result.push_str(&format_value_with_spec(&val, &spec));
                        i += end + 1;
                        continue;
                    }
                }
                result.push(chars[i]);
                i += 1;
            }
            Value::String(result)
        } else {
            Value::String(String::new())
        }
    }

    /// Parse endian argument from builtin calls (1=Big, 2=Little, else=default)
    pub(crate) fn parse_endian_arg(&self, arg: Option<&Value>) -> Endianness {
        match arg.and_then(|v| v.to_unsigned().ok()) {
            Some(1) => Endianness::Big,
            Some(2) => Endianness::Little,
            _ => self.default_endian,
        }
    }

    pub(crate) fn eval_binary_op(
        &self,
        op: BinOp,
        left: &Value,
        right: &Value,
    ) -> Result<Value, EvalError> {
        // If either is float, do float arithmetic
        if matches!(left, Value::Float(_)) || matches!(right, Value::Float(_)) {
            let l = left.to_float()?;
            let r = right.to_float()?;
            return match op {
                BinOp::Add => Ok(Value::Float(l + r)),
                BinOp::Sub => Ok(Value::Float(l - r)),
                BinOp::Mul => Ok(Value::Float(l * r)),
                BinOp::Div => {
                    if r == 0.0 {
                        Ok(Value::Float(0.0))
                    } else {
                        Ok(Value::Float(l / r))
                    }
                }
                BinOp::Mod => Ok(Value::Float(l % r)),
                BinOp::Eq => Ok(Value::Bool(l == r)),
                BinOp::Ne => Ok(Value::Bool(l != r)),
                BinOp::Lt => Ok(Value::Bool(l < r)),
                BinOp::Gt => Ok(Value::Bool(l > r)),
                BinOp::Le => Ok(Value::Bool(l <= r)),
                BinOp::Ge => Ok(Value::Bool(l >= r)),
                _ => Err(EvalError::new("invalid operator for float values")),
            };
        }

        // String operations
        if matches!(left, Value::String(_)) || matches!(right, Value::String(_)) {
            if matches!(op, BinOp::Add) {
                return Ok(Value::String(format!(
                    "{}{}",
                    left.to_display_string(),
                    right.to_display_string()
                )));
            }
            // String repetition: "abc" * 3 → "abcabcabc" or 3 * "abc" → "abcabcabc"
            if matches!(op, BinOp::Mul) {
                match (left, right) {
                    (Value::String(s), r) => {
                        let count = r.to_unsigned().unwrap_or(0) as usize;
                        return Ok(Value::String(s.repeat(count.min(0x10000))));
                    }
                    (l, Value::String(s)) => {
                        let count = l.to_unsigned().unwrap_or(0) as usize;
                        return Ok(Value::String(s.repeat(count.min(0x10000))));
                    }
                    _ => {}
                }
            }
            if matches!(op, BinOp::Eq) {
                // Direct comparison when both are String, otherwise compare display forms
                let result = match (left, right) {
                    (Value::String(a), Value::String(b)) => a == b,
                    _ => left.to_display_string() == right.to_display_string(),
                };
                return Ok(Value::Bool(result));
            }
            if matches!(op, BinOp::Ne) {
                let result = match (left, right) {
                    (Value::String(a), Value::String(b)) => a != b,
                    _ => left.to_display_string() != right.to_display_string(),
                };
                return Ok(Value::Bool(result));
            }
        }

        macro_rules! int_binary_op {
            ($op:expr, $l:expr, $r:expr, $variant:ident) => {
                match $op {
                    BinOp::Add => Ok(Value::$variant($l.wrapping_add($r))),
                    BinOp::Sub => Ok(Value::$variant($l.wrapping_sub($r))),
                    BinOp::Mul => Ok(Value::$variant($l.wrapping_mul($r))),
                    BinOp::Div => {
                        if $r == 0 {
                            Ok(Value::$variant(0))
                        } else {
                            Ok(Value::$variant($l.wrapping_div($r)))
                        }
                    }
                    BinOp::Mod => {
                        if $r == 0 {
                            Ok(Value::$variant(0))
                        } else {
                            Ok(Value::$variant($l.wrapping_rem($r)))
                        }
                    }
                    BinOp::BitAnd => Ok(Value::$variant($l & $r)),
                    BinOp::BitOr => Ok(Value::$variant($l | $r)),
                    BinOp::BitXor => Ok(Value::$variant($l ^ $r)),
                    BinOp::Shl => Ok(Value::$variant($l.wrapping_shl($r as u32))),
                    BinOp::Shr => Ok(Value::$variant($l.wrapping_shr($r as u32))),
                    BinOp::Eq => Ok(Value::Bool($l == $r)),
                    BinOp::Ne => Ok(Value::Bool($l != $r)),
                    BinOp::Lt => Ok(Value::Bool($l < $r)),
                    BinOp::Gt => Ok(Value::Bool($l > $r)),
                    BinOp::Le => Ok(Value::Bool($l <= $r)),
                    BinOp::Ge => Ok(Value::Bool($l >= $r)),
                    BinOp::LogAnd | BinOp::LogOr => unreachable!("handled by short-circuit"),
                }
            };
        }

        // Integer arithmetic - use signed if either is signed
        if matches!(left, Value::Signed(_)) || matches!(right, Value::Signed(_)) {
            let l = left.to_signed()?;
            let r = right.to_signed()?;
            return int_binary_op!(op, l, r, Signed);
        }

        // Unsigned arithmetic
        let l = left.to_unsigned()?;
        let r = right.to_unsigned()?;
        int_binary_op!(op, l, r, Unsigned)
    }

    /// Get the size of a type
    pub(crate) fn type_size(&mut self, ty: &TypeExpr) -> Result<u64, EvalError> {
        match &ty.kind {
            TypeExprKind::Builtin(builtin) => {
                builtin.size().ok_or_else(|| EvalError::new("unsized type"))
            }
            TypeExprKind::ArbitraryInt { bits, .. } => Ok(bits.div_ceil(8)),
            TypeExprKind::Endian(_, inner) => self.type_size(inner),
            TypeExprKind::Named(path) => {
                let name = self.resolve_path(path);
                let name_key = self.interner.intern(&name);
                if let Some(typedef) = self.scope.get_type(name_key) {
                    match &*typedef {
                        TypeDef::Alias { ty: Some(ty), .. } => self.type_size(ty),
                        TypeDef::Enum { underlying, .. } => self.type_size(underlying),
                        TypeDef::Struct { parent, body, .. } => {
                            let mut total = 0u64;
                            // Add parent struct size
                            if let Some((parent_name, _)) = parent {
                                let pn = self.interner.resolve(*parent_name).to_string();
                                let pn_key = self.interner.intern(&pn);
                                if let Some(parent_td) = self.scope.get_type(pn_key) {
                                    if let TypeDef::Struct { .. } = &*parent_td {
                                        if let Ok(ps) = self.type_size(&TypeExpr {
                                            kind: TypeExprKind::Named(vec![*parent_name]),
                                            span: ty.span,
                                        }) {
                                            total += ps;
                                        }
                                    }
                                }
                            }
                            // Sum field sizes from body
                            for stmt in body {
                                match &stmt.kind {
                                    StmtKind::DataPlacement(d) => {
                                        total += self.type_size(&d.ty)?;
                                    }
                                    StmtKind::AnonymousPlacement(d) => {
                                        total += self.type_size(&d.ty)?;
                                    }
                                    _ => {}
                                }
                            }
                            Ok(total)
                        }
                        TypeDef::Union { body, .. } => {
                            let mut max_size = 0u64;
                            for stmt in body {
                                if let StmtKind::DataPlacement(d) = &stmt.kind {
                                    let field_ty = &d.ty;
                                    if let Ok(s) = self.type_size(field_ty) {
                                        max_size = max_size.max(s);
                                    }
                                }
                            }
                            Ok(max_size)
                        }
                        TypeDef::Bitfield { body, .. } => {
                            let items = match body.first().map(|s| &s.kind) {
                                Some(StmtKind::BitfieldDef(d)) => d.body.clone(),
                                _ => {
                                    return Err(EvalError::new(format!(
                                        "cannot determine size of type '{}'",
                                        name
                                    )))
                                }
                            };
                            let mut total_bits = 0u64;
                            for stmt in &items {
                                if let StmtKind::BitfieldFieldStmt { width, .. } = &stmt.kind {
                                    if let Ok(val) = self.eval_expr(width) {
                                        total_bits += val.to_unsigned().unwrap_or(1) as u64;
                                    }
                                }
                            }
                            Ok(total_bits.div_ceil(8))
                        }
                        _ => Err(EvalError::new(format!(
                            "cannot determine size of type '{}'",
                            name
                        ))),
                    }
                } else if let Some((_, bits)) = parse_arbitrary_int_type(&name) {
                    Ok(bits.div_ceil(8))
                } else if let Some(val) = self.scope.get_var(name_key).cloned() {
                    // Fallback: sizeof(variable) — parser can't distinguish
                    // type names from variable names for single identifiers.
                    self.value_size(&val)
                } else {
                    Err(EvalError::new(format!(
                        "cannot determine size of unknown type '{}'",
                        name
                    )))
                }
            }
            TypeExprKind::Template(path, args) => {
                let name = self.resolve_path(path);
                let name_key = self.interner.intern(&name);
                if let Some(typedef) = self.scope.get_type(name_key) {
                    match &*typedef {
                        TypeDef::Alias {
                            template_params,
                            ty: Some(alias_ty),
                            ..
                        } if !template_params.is_empty() && args.len() == template_params.len() => {
                            let substituted = substitute_type(alias_ty, template_params, args);
                            self.type_size(&substituted)
                        }
                        TypeDef::Alias { ty: Some(ty), .. } => self.type_size(ty),
                        TypeDef::Struct {
                            template_params, ..
                        }
                        | TypeDef::Union {
                            template_params, ..
                        } if !template_params.is_empty() => {
                            // Push scope, register template args, compute size, pop
                            self.scope.push();
                            if let Err(e) = self.register_template_args(template_params, args) {
                                self.scope.pop();
                                return Err(e);
                            }
                            let result = self.type_size(&TypeExpr {
                                kind: TypeExprKind::Named(path.clone()),
                                span: ty.span,
                            });
                            self.scope.pop();
                            result
                        }
                        _ => Err(EvalError::new(format!(
                            "cannot determine size of template type '{}'",
                            name
                        ))),
                    }
                } else {
                    Err(EvalError::new(format!(
                        "cannot determine size of template type '{}'",
                        name
                    )))
                }
            }
            TypeExprKind::Padding => Ok(1),
            TypeExprKind::Array(elem_ty, ArraySize::Fixed(size_expr)) => {
                let elem_size = self.type_size(elem_ty)?;
                let count_val = self.eval_expr(size_expr)?;
                let count = count_val.to_unsigned().map_err(|e| self.resolve_error(e))? as u64;
                Ok(elem_size * count)
            }
            _ => Err(EvalError::new("cannot determine size of non-builtin type")),
        }
    }

    /// Get the size of a runtime value (for sizeof(variable) support)
    pub(crate) fn value_size(&self, val: &Value) -> Result<u64, EvalError> {
        match val {
            Value::Unsigned(v) => {
                // Determine minimal size based on value magnitude
                if *v <= u8::MAX as u128 {
                    Ok(1)
                } else if *v <= u16::MAX as u128 {
                    Ok(2)
                } else if *v <= u32::MAX as u128 {
                    Ok(4)
                } else if *v <= u64::MAX as u128 {
                    Ok(8)
                } else {
                    Ok(16)
                }
            }
            Value::Signed(_) => Ok(8),
            Value::Float(_) => Ok(8),
            Value::Bool(_) => Ok(1),
            Value::Char(_) => Ok(1),
            Value::String(s) => Ok(s.chars().count() as u64),
            Value::Null => Ok(0),
            Value::Array(items) => Ok(items.len() as u64),
            Value::SizedRef { size, .. } | Value::BulkRef { size, .. } => Ok(*size),
        }
    }

    /// Cast a value to a given type
    pub(crate) fn cast_value(&self, val: &Value, ty: &TypeExpr) -> Result<Value, EvalError> {
        match &ty.kind {
            TypeExprKind::Builtin(builtin) => match builtin {
                BuiltinType::U8 => Ok(Value::Unsigned(val.to_unsigned()? & 0xFF)),
                BuiltinType::U16 => Ok(Value::Unsigned(val.to_unsigned()? & 0xFFFF)),
                BuiltinType::U32 => Ok(Value::Unsigned(val.to_unsigned()? & 0xFFFF_FFFF)),
                BuiltinType::U64 => Ok(Value::Unsigned(val.to_unsigned()? & 0xFFFF_FFFF_FFFF_FFFF)),
                BuiltinType::U128 => Ok(Value::Unsigned(val.to_unsigned()?)),
                BuiltinType::S8 => Ok(Value::Signed(val.to_signed()? as i8 as i128)),
                BuiltinType::S16 => Ok(Value::Signed(val.to_signed()? as i16 as i128)),
                BuiltinType::S32 => Ok(Value::Signed(val.to_signed()? as i32 as i128)),
                BuiltinType::S64 => Ok(Value::Signed(val.to_signed()? as i64 as i128)),
                BuiltinType::S128 => Ok(Value::Signed(val.to_signed()?)),
                BuiltinType::Float | BuiltinType::Double => Ok(Value::Float(val.to_float()?)),
                BuiltinType::Bool => Ok(Value::Bool(val.to_bool()?)),
                BuiltinType::Char => match val {
                    Value::Unsigned(v) => {
                        Ok(Value::Char(char::from_u32(*v as u32).unwrap_or('\0')))
                    }
                    Value::Signed(v) => Ok(Value::Char(char::from_u32(*v as u32).unwrap_or('\0'))),
                    Value::Char(c) => Ok(Value::Char(*c)),
                    _ => Ok(Value::Char('\0')),
                },
                BuiltinType::Char16 => match val {
                    Value::Unsigned(v) => {
                        Ok(Value::Char(char::from_u32(*v as u32).unwrap_or('\0')))
                    }
                    Value::Signed(v) => Ok(Value::Char(char::from_u32(*v as u32).unwrap_or('\0'))),
                    _ => Ok(Value::Char('\0')),
                },
                BuiltinType::Str => Ok(Value::String(val.to_display_string())),
            },
            TypeExprKind::Endian(_, inner) => self.cast_value(val, inner),
            _ => Err(EvalError::new("can only cast to builtin types")),
        }
    }

    /// Resolve an enum member by type name and member name
    pub(crate) fn resolve_enum_member(
        &mut self,
        type_name: &str,
        member_name: &str,
        span: Span,
    ) -> Result<Value, EvalError> {
        let type_name_key = self.interner.intern(type_name);
        let member_name_key = self.interner.intern(member_name);
        if let Some(td) = self.scope.get_type(type_name_key) {
            if let TypeDef::Enum { members, .. } = &*td {
                for (name, val) in members {
                    if name == &member_name_key {
                        return val.clone().ok_or_else(|| {
                            self.make_error(
                                format!(
                                    "enum '{}' member '{}' has no value",
                                    type_name, member_name
                                ),
                                span,
                            )
                        });
                    }
                }
                Err(self.make_error(
                    format!("enum '{}' has no member '{}'", type_name, member_name),
                    span,
                ))
            } else {
                Err(self.make_error(
                    format!("undefined '{}::{}'", type_name, member_name),
                    span,
                ))
            }
        } else {
            Err(self.make_error(format!("undefined '{}::{}'", type_name, member_name), span))
        }
    }
}
