// Recursive descent parser for the pattern language

pub mod ast;

use crate::error::ParseError;
use crate::lexer::token::{Token, TokenKind};
use crate::name::{Name, StringInterner};
use crate::span::Span;
use ast::*;

/// Parser that converts a token stream into an AST
pub struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    errors: Vec<ParseError>,
    /// When true, `>` and `>=` are not consumed as comparison operators in expressions.
    /// Used inside template argument parsing where `>` closes the template.
    in_template_args: bool,
    /// When true, single-statement bodies (in if/while/else) should check for bitfield
    /// field syntax (`name : width;`) before parsing as a normal statement.
    in_bitfield: bool,
    /// When true, `|` is not consumed as bitwise-or in expressions.
    /// Used inside match pattern parsing where `|` separates alternative patterns.
    in_match_pattern: bool,
    /// Nesting depth for constructs where `namespace` is not allowed (fn, if, while, for, etc.).
    /// When > 0, namespace definitions produce an error.
    block_depth: u32,
    interner: &'a mut StringInterner,
}

/// Maximum lookahead distance when searching for bitfield colon (`name : width`).
const MAX_BITFIELD_LOOKAHEAD: usize = 10;

/// Maximum lookahead distance when scanning template/array brackets in anonymous placement detection.
const MAX_BRACKET_LOOKAHEAD: usize = 100;

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, interner: &'a mut StringInterner) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
            in_template_args: false,
            in_bitfield: false,
            in_match_pattern: false,
            block_depth: 0,
            interner,
        }
    }

    /// Parse the entire token stream into an AST
    pub fn parse(mut self) -> Result<Ast, Vec<ParseError>> {
        let mut stmts = Vec::new();
        while !self.at_eof() {
            // Skip empty statements (trailing semicolons)
            if self.eat(&TokenKind::Semicolon) {
                continue;
            }
            match self.parse_top_level_stmt() {
                Ok(parsed) => {
                    stmts.extend(parsed);
                }
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }
        if self.errors.is_empty() {
            Ok(Ast { stmts })
        } else {
            Err(self.errors)
        }
    }

    // ========== Token helpers ==========

    fn peek(&self) -> &TokenKind {
        self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::Eof)
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span)
            .unwrap_or(Span::dummy())
    }

    fn peek_ahead(&self, offset: usize) -> &TokenKind {
        self.tokens
            .get(self.pos + offset)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::Eof)
    }

    fn advance(&mut self) -> Token {
        let tok = self
            .tokens
            .get(self.pos)
            .cloned()
            .unwrap_or_else(|| Token::new(TokenKind::Eof, Span::dummy()));
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    fn at_eof(&self) -> bool {
        matches!(self.peek(), TokenKind::Eof)
    }

    /// Attempt a parse function, always backtracking regardless of success or failure.
    /// Returns Some(result) if parsing succeeded, None if it failed.
    /// The parser position is always restored to the original position.
    fn try_parse<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T, ParseError>) -> Option<T> {
        let saved_pos = self.pos;
        let saved_errors = self.errors.len();
        match f(self) {
            Ok(result) => {
                self.pos = saved_pos;
                self.errors.truncate(saved_errors);
                Some(result)
            }
            Err(_) => {
                self.pos = saved_pos;
                self.errors.truncate(saved_errors);
                None
            }
        }
    }

    fn expect(&mut self, expected: &TokenKind) -> Result<Token, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(expected) {
            Ok(self.advance())
        } else {
            Err(ParseError::expected(
                format!("unexpected token {}", self.peek()),
                format!("{}", expected),
                self.peek_span(),
            ))
        }
    }

    fn expect_ident(&mut self) -> Result<(Name, Span), ParseError> {
        match self.peek().clone() {
            TokenKind::Ident(name) => {
                let span = self.peek_span();
                self.advance();
                Ok((name, span))
            }
            _ => Err(ParseError::expected(
                format!("unexpected token {}", self.peek()),
                "identifier",
                self.peek_span(),
            )),
        }
    }

    fn expect_semicolon(&mut self) -> Result<(), ParseError> {
        self.expect(&TokenKind::Semicolon)?;
        Ok(())
    }

    /// Accept either comma or semicolon as separator (for `for` statement)
    fn expect_comma_or_semicolon(&mut self) -> Result<(), ParseError> {
        if matches!(self.peek(), TokenKind::Comma | TokenKind::Semicolon) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::expected(
                format!("unexpected token {}", self.peek()),
                "';' or ','",
                self.peek_span(),
            ))
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(kind)
    }

    /// Check if current position is at `[[` (attribute open)
    fn check_lattr(&self) -> bool {
        matches!(self.peek(), TokenKind::LBracket)
            && matches!(self.peek_ahead(1), TokenKind::LBracket)
    }

    fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Skip tokens until we find a synchronization point
    fn synchronize(&mut self) {
        while !self.at_eof() {
            if matches!(self.peek(), TokenKind::Semicolon | TokenKind::RBrace) {
                self.advance();
                return;
            }
            if matches!(
                self.peek(),
                TokenKind::KwStruct
                    | TokenKind::KwUnion
                    | TokenKind::KwEnum
                    | TokenKind::KwBitfield
                    | TokenKind::KwFn
                    | TokenKind::KwNamespace
                    | TokenKind::KwIf
                    | TokenKind::KwWhile
                    | TokenKind::KwFor
                    | TokenKind::KwReturn
                    | TokenKind::KwUsing
                    | TokenKind::KwImport
                    | TokenKind::KwTry
            ) {
                return;
            }
            self.advance();
        }
    }

    // ========== Top-level parsing ==========

    fn parse_top_level_stmt(&mut self) -> Result<Vec<Stmt>, ParseError> {
        // Parse optional attributes
        let attrs = self.parse_attributes()?;

        let start = self.peek_span();

        // Inside bitfield, check for bitfield field syntax before other parsing
        if self.in_bitfield {
            if let Some(colon_offset) = self.find_bitfield_colon() {
                let field_start = self.peek_span();
                for _ in 0..colon_offset {
                    self.advance();
                }
                let (field_name, _) = self.expect_ident()?;
                self.expect(&TokenKind::Colon)?;
                let width = self.parse_expr()?;
                let field_attrs = self.parse_attributes()?;
                self.expect_semicolon()?;
                let mut all_attrs = attrs;
                all_attrs.extend(field_attrs);
                return Ok(vec![Stmt {
                    kind: StmtKind::BitfieldFieldStmt {
                        name: field_name,
                        width,
                        attrs: all_attrs,
                    },
                    span: field_start,
                }]);
            }
        }

        match self.peek() {
            TokenKind::KwStruct => self.parse_struct_def(attrs).map(|s| vec![s]),
            TokenKind::KwUnion => self.parse_union_def(attrs).map(|s| vec![s]),
            TokenKind::KwEnum => self.parse_enum_def(attrs).map(|s| vec![s]),
            TokenKind::KwBitfield => self.parse_bitfield_def(attrs).map(|s| vec![s]),
            TokenKind::KwFn => self.parse_fn_def().map(|s| vec![s]),
            TokenKind::KwNamespace => self.parse_namespace().map(|s| vec![s]),
            TokenKind::KwUsing => self.parse_using_stmt().map(|s| vec![s]),
            TokenKind::KwIf => self.parse_if_stmt().map(|s| vec![s]),
            TokenKind::KwMatch => self.parse_match_stmt().map(|s| vec![s]),
            TokenKind::KwWhile => self.parse_while_stmt().map(|s| vec![s]),
            TokenKind::KwFor => self.parse_for_stmt().map(|s| vec![s]),
            TokenKind::KwTry => self.parse_try_catch().map(|s| vec![s]),
            TokenKind::KwBreak => {
                self.advance();
                self.expect_semicolon()?;
                Ok(vec![Stmt {
                    kind: StmtKind::Break,
                    span: start,
                }])
            }
            TokenKind::KwContinue => {
                self.advance();
                self.expect_semicolon()?;
                Ok(vec![Stmt {
                    kind: StmtKind::Continue,
                    span: start,
                }])
            }
            TokenKind::KwReturn => self.parse_return_stmt().map(|s| vec![s]),
            TokenKind::KwImport => self.parse_import_stmt().map(|s| vec![s]),
            _ => {
                // Try to parse as variable declaration or expression statement
                // (may return multiple stmts for comma-separated declarations)
                self.parse_var_decl_or_expr_stmt(attrs)
            }
        }
    }

    // ========== Attributes ==========

    fn parse_attributes(&mut self) -> Result<Vec<Attribute>, ParseError> {
        let mut attrs = Vec::new();
        while self.check_lattr() {
            let start = self.peek_span();
            self.advance(); // consume first [
            self.advance(); // consume second [

            loop {
                let (mut name, _) = self.expect_ident()?;
                // Support scoped attribute names like `hex::visualize`
                while self.eat(&TokenKind::ColonColon) {
                    let (part, _) = self.expect_ident()?;
                    name = self.interner.intern(&format!(
                        "{}::{}",
                        self.interner.resolve(name),
                        self.interner.resolve(part)
                    ));
                }
                let mut args = Vec::new();

                if self.eat(&TokenKind::LParen) {
                    while !self.check(&TokenKind::RParen) && !self.at_eof() {
                        args.push(self.parse_expr()?);
                        if !self.eat(&TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect(&TokenKind::RParen)?;
                }

                let end = self.peek_span();
                attrs.push(Attribute {
                    name,
                    args,
                    span: start.merge(end),
                });

                if !self.eat(&TokenKind::Comma) {
                    break;
                }
            }

            // Expect ] ] (two RBrackets) to close attribute
            self.expect(&TokenKind::RBracket)?;
            self.expect(&TokenKind::RBracket)?;
        }
        Ok(attrs)
    }

    // ========== Type expression parsing ==========

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let start = self.peek_span();

        // Skip const qualifier (treated as no-op for evaluation)
        self.eat(&TokenKind::KwConst);

        // Check for endianness prefix
        let endianness = match self.peek() {
            TokenKind::KwLe => {
                self.advance();
                Some(Endianness::Little)
            }
            TokenKind::KwBe => {
                self.advance();
                Some(Endianness::Big)
            }
            _ => None,
        };

        // Parse base type
        let mut ty = self.parse_base_type()?;

        // Wrap with endianness if present
        if let Some(endian) = endianness {
            ty = TypeExpr {
                span: start.merge(ty.span),
                kind: TypeExprKind::Endian(endian, Box::new(ty)),
            };
        }

        // Check for array dimensions
        ty = self.parse_array_dims(ty)?;

        Ok(ty)
    }

    fn parse_base_type(&mut self) -> Result<TypeExpr, ParseError> {
        let start = self.peek_span();

        match self.peek().clone() {
            TokenKind::KwAuto => {
                self.advance();
                Ok(TypeExpr {
                    kind: TypeExprKind::Auto,
                    span: start,
                })
            }
            TokenKind::Ident(name) => {
                let name_str = self.interner.resolve(name);
                // Check for padding type
                if name_str == "padding" {
                    self.advance();
                    return Ok(TypeExpr {
                        kind: TypeExprKind::Padding,
                        span: start,
                    });
                }
                // Check for builtin type
                if let Some(builtin) = BuiltinType::from_str(name_str) {
                    self.advance();
                    Ok(TypeExpr {
                        kind: TypeExprKind::Builtin(builtin),
                        span: start,
                    })
                } else if let Some((signed, bits)) = Self::parse_arbitrary_int_name(name_str) {
                    self.advance();
                    Ok(TypeExpr {
                        kind: TypeExprKind::ArbitraryInt { signed, bits },
                        span: start,
                    })
                } else {
                    // Named type, possibly with namespace and template args
                    let mut path = vec![name];
                    self.advance();

                    while matches!(self.peek(), TokenKind::ColonColon) {
                        self.advance();
                        let (name, _) = self.expect_ident()?;
                        path.push(name);
                    }

                    // Check for template arguments
                    if matches!(self.peek(), TokenKind::Less) {
                        let template_args = self.parse_template_args()?;
                        let end = self.peek_span();
                        Ok(TypeExpr {
                            kind: TypeExprKind::Template(path, template_args),
                            span: start.merge(end),
                        })
                    } else {
                        let end_span = self
                            .tokens
                            .get(self.pos.saturating_sub(1))
                            .map(|t| t.span)
                            .unwrap_or(start);
                        Ok(TypeExpr {
                            kind: TypeExprKind::Named(path),
                            span: start.merge(end_span),
                        })
                    }
                }
            }
            _ => Err(ParseError::expected(
                format!("unexpected token {}", self.peek()),
                "type",
                start,
            )),
        }
    }

    /// Parse post-definition attributes and merge with pre-definition attrs.
    fn parse_and_merge_attrs(
        &mut self,
        mut pre: Vec<Attribute>,
    ) -> Result<Vec<Attribute>, ParseError> {
        let post = self.parse_attributes()?;
        pre.extend(post);
        Ok(pre)
    }

    /// Parse array dimensions: `[N]`, `[while(cond)]`, `[]`.
    /// Stops at `[[` (attribute syntax). Can be called on any TypeExpr.
    fn parse_array_dims(&mut self, mut ty: TypeExpr) -> Result<TypeExpr, ParseError> {
        let start = ty.span;
        while matches!(self.peek(), TokenKind::LBracket) && !self.check_lattr() {
            self.advance();
            let size = if matches!(self.peek(), TokenKind::RBracket) {
                ArraySize::NullTerminated
            } else if matches!(self.peek(), TokenKind::KwWhile) {
                self.advance();
                self.expect(&TokenKind::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(&TokenKind::RParen)?;
                ArraySize::Conditional(Box::new(cond))
            } else {
                let size_expr = self.parse_expr()?;
                ArraySize::Fixed(Box::new(size_expr))
            };
            let end = self.expect(&TokenKind::RBracket)?;
            ty = TypeExpr {
                span: start.merge(end.span),
                kind: TypeExprKind::Array(Box::new(ty), size),
            };
        }
        Ok(ty)
    }

    fn parse_template_args(&mut self) -> Result<Vec<TemplateArg>, ParseError> {
        self.expect(&TokenKind::Less)?;
        let mut args = Vec::new();

        let prev_in_template = self.in_template_args;
        self.in_template_args = true;

        while !self.check(&TokenKind::Greater) && !self.at_eof() {
            // Try to parse as type first, fall back to expression.
            // Save position so we can backtrack if the "type" turns out to be
            // a function call (e.g. `std::mem::size() - $`).
            let saved_pos = self.pos;
            if self.looks_like_type() {
                let ty = self.parse_type_expr()?;
                // If `(` follows a Named type, it was a function call, not a type.
                // Backtrack and re-parse as an expression.
                if matches!(self.peek(), TokenKind::LParen)
                    && matches!(ty.kind, TypeExprKind::Named(_))
                {
                    self.pos = saved_pos;
                    let expr = self.parse_expr()?;
                    args.push(TemplateArg::Expr(expr));
                } else {
                    args.push(TemplateArg::Type(ty));
                }
            } else {
                let expr = self.parse_expr()?;
                args.push(TemplateArg::Expr(expr));
            }

            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }

        self.in_template_args = prev_in_template;

        self.expect(&TokenKind::Greater)?;
        Ok(args)
    }

    fn parse_template_params(&mut self) -> Result<Vec<TemplateParam>, ParseError> {
        if !self.eat(&TokenKind::Less) {
            return Ok(Vec::new());
        }

        let mut params = Vec::new();
        while !self.check(&TokenKind::Greater) && !self.at_eof() {
            // Accept `auto Name` or just `Name`
            let is_auto = if matches!(self.peek(), TokenKind::KwAuto) {
                self.advance();
                true
            } else {
                false
            };
            let (name, span) = self.expect_ident()?;
            params.push(TemplateParam {
                name,
                is_auto,
                span,
            });

            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        self.expect(&TokenKind::Greater)?;
        Ok(params)
    }

    /// Heuristic: does the current token start a type expression?
    fn looks_like_type(&self) -> bool {
        match self.peek() {
            TokenKind::KwLe | TokenKind::KwBe | TokenKind::KwAuto | TokenKind::KwConst => true,
            TokenKind::Ident(name) => {
                let name_str = self.interner.resolve(*name);
                BuiltinType::from_str(name_str).is_some()
                    || Self::is_arbitrary_int_name(name_str)
                    || matches!(self.peek_ahead(1), TokenKind::ColonColon | TokenKind::Less)
                    // If followed by identifier, it could be a type + variable name
                    || matches!(self.peek_ahead(1), TokenKind::Ident(_))
            }
            _ => false,
        }
    }

    /// Parse a name matching the arbitrary-width integer pattern (u24, s48, etc.)
    /// Returns (signed, bits) if it matches, None otherwise.
    fn parse_arbitrary_int_name(name: &str) -> Option<(bool, u64)> {
        let (signed, digits) = if let Some(rest) = name.strip_prefix('u') {
            (false, rest)
        } else if let Some(rest) = name.strip_prefix('s') {
            (true, rest)
        } else {
            return None;
        };
        if digits.is_empty() || !digits.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        let bits: u64 = digits.parse().ok()?;
        if bits == 0 || bits > 128 {
            return None;
        }
        Some((signed, bits))
    }

    /// Check if a name matches the arbitrary-width integer pattern (u24, s48, etc.)
    fn is_arbitrary_int_name(name: &str) -> bool {
        Self::parse_arbitrary_int_name(name).is_some()
    }

    // ========== Statement parsing ==========

    fn parse_struct_def(&mut self, attrs: Vec<Attribute>) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwStruct)?;
        let (name, _) = self.expect_ident()?;

        let template_params = self.parse_template_params()?;

        // Optional inheritance: `: ParentType` or `: Scope::Parent<Args>`
        let parent = if self.eat(&TokenKind::Colon) {
            let (mut parent_name, _) = self.expect_ident()?;
            // Consume scoped names: `A::B::C`
            while self.eat(&TokenKind::ColonColon) {
                let (part, _) = self.expect_ident()?;
                parent_name = self.interner.intern(&format!(
                    "{}::{}",
                    self.interner.resolve(parent_name),
                    self.interner.resolve(part)
                ));
            }
            // Parse template arguments: `<T, U>`
            let parent_template_args = if matches!(self.peek(), TokenKind::Less) {
                self.parse_template_args()?
            } else {
                vec![]
            };
            Some((parent_name, parent_template_args))
        } else {
            None
        };

        self.expect(&TokenKind::LBrace)?;
        let body = self.parse_block_stmts()?;
        let end = self.expect(&TokenKind::RBrace)?;

        let all_attrs = self.parse_and_merge_attrs(attrs)?;

        // Optional semicolon after struct def
        self.eat(&TokenKind::Semicolon);

        Ok(Stmt {
            kind: StmtKind::StructDef(Box::new(StructDefData {
                name,
                parent,
                template_params,
                body,
                attrs: all_attrs,
            })),
            span: start.merge(end.span),
        })
    }

    fn parse_union_def(&mut self, attrs: Vec<Attribute>) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwUnion)?;
        let (name, _) = self.expect_ident()?;

        let template_params = self.parse_template_params()?;

        self.expect(&TokenKind::LBrace)?;
        let body = self.parse_block_stmts()?;
        let end = self.expect(&TokenKind::RBrace)?;

        let all_attrs = self.parse_and_merge_attrs(attrs)?;

        self.eat(&TokenKind::Semicolon);

        Ok(Stmt {
            kind: StmtKind::UnionDef(Box::new(UnionDefData {
                name,
                template_params,
                body,
                attrs: all_attrs,
            })),
            span: start.merge(end.span),
        })
    }

    fn parse_enum_def(&mut self, attrs: Vec<Attribute>) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwEnum)?;
        let (name, _) = self.expect_ident()?;

        // Underlying type: `: u8`
        self.expect(&TokenKind::Colon)?;
        let underlying = self.parse_type_expr()?;

        self.expect(&TokenKind::LBrace)?;

        let mut members = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            let member_start = self.peek_span();
            let (member_name, _) = self.expect_ident()?;
            let value = if self.eat(&TokenKind::Eq) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            let end_value = if self.eat(&TokenKind::DotDotDot) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            members.push(EnumMember {
                name: member_name,
                value,
                end_value,
                span: member_start,
            });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }

        let end = self.expect(&TokenKind::RBrace)?;

        let all_attrs = self.parse_and_merge_attrs(attrs)?;

        self.eat(&TokenKind::Semicolon);

        Ok(Stmt {
            kind: StmtKind::EnumDef(Box::new(EnumDefData {
                name,
                underlying,
                members,
                attrs: all_attrs,
            })),
            span: start.merge(end.span),
        })
    }

    fn parse_bitfield_def(&mut self, attrs: Vec<Attribute>) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwBitfield)?;
        let (name, _) = self.expect_ident()?;

        // Parse optional template params
        let template_params = if matches!(self.peek(), TokenKind::Less) {
            self.parse_template_params()?
        } else {
            Vec::new()
        };

        self.expect(&TokenKind::LBrace)?;

        let prev_in_bitfield = self.in_bitfield;
        self.in_bitfield = true;

        let mut items = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            // Skip empty statements
            if self.eat(&TokenKind::Semicolon) {
                continue;
            }
            // Check if this looks like a bitfield field pattern.
            // Patterns: `name : width;`, `Type name : width;`, `bool name : width;`
            if let Some(colon_offset) = self.find_bitfield_colon() {
                let field_start = self.peek_span();
                // Skip type tokens before the field name (everything before the ident before colon)
                for _ in 0..colon_offset {
                    self.advance();
                }
                let (field_name, _) = self.expect_ident()?;
                self.expect(&TokenKind::Colon)?;
                let width = self.parse_expr()?;
                let field_attrs = self.parse_attributes()?;
                self.expect_semicolon()?;
                items.push(Stmt {
                    kind: StmtKind::BitfieldFieldStmt {
                        name: field_name,
                        width,
                        attrs: field_attrs,
                    },
                    span: field_start,
                });
            } else {
                // General statement (if, padding, var decl, etc.)
                match self.parse_top_level_stmt() {
                    Ok(parsed) => {
                        items.extend(parsed);
                    }
                    Err(e) => {
                        self.errors.push(e);
                        self.synchronize();
                    }
                }
            }
        }

        self.in_bitfield = prev_in_bitfield;

        let end = self.expect(&TokenKind::RBrace)?;

        let all_attrs = self.parse_and_merge_attrs(attrs)?;

        self.eat(&TokenKind::Semicolon);

        Ok(Stmt {
            kind: StmtKind::BitfieldDef(Box::new(BitfieldDefData {
                name,
                template_params,
                body: items,
                attrs: all_attrs,
            })),
            span: start.merge(end.span),
        })
    }

    fn parse_fn_def(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwFn)?;
        let (name, _) = self.expect_ident()?;

        self.expect(&TokenKind::LParen)?;
        let mut params = Vec::new();
        while !self.check(&TokenKind::RParen) && !self.at_eof() {
            let param_start = self.peek_span();
            let direction = match self.peek() {
                TokenKind::KwRef => {
                    self.advance();
                    ParamDirection::Ref
                }
                TokenKind::KwOut => {
                    self.advance();
                    ParamDirection::Out
                }
                TokenKind::KwIn => {
                    self.advance();
                    ParamDirection::In
                }
                _ => ParamDirection::In,
            };
            let ty = self.parse_type_expr()?;
            // Handle variadic: `auto ... args`
            let is_variadic = self.eat(&TokenKind::DotDotDot);
            let (param_name, _) = self.expect_ident()?;
            // Handle default value: `= expr`
            let default = if self.eat(&TokenKind::Eq) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            params.push(Param {
                ty,
                name: param_name,
                direction,
                is_variadic,
                default,
                span: param_start,
            });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        self.expect(&TokenKind::RParen)?;

        // Optional return type
        let return_ty = if self.eat(&TokenKind::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(&TokenKind::LBrace)?;
        let body = self.parse_block_stmts()?;
        let end = self.expect(&TokenKind::RBrace)?;

        // No semicolon required after fn def
        self.eat(&TokenKind::Semicolon);

        Ok(Stmt {
            kind: StmtKind::FnDef(Box::new(FnDefData {
                name,
                params,
                return_ty,
                body,
            })),
            span: start.merge(end.span),
        })
    }

    fn parse_namespace(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwNamespace)?;
        // Skip optional 'auto' modifier
        self.eat(&TokenKind::KwAuto);
        let (first, _) = self.expect_ident()?;
        let mut name = first;
        while self.eat(&TokenKind::ColonColon) {
            let (segment, _) = self.expect_ident()?;
            name = self.interner.intern(&format!(
                "{}::{}",
                self.interner.resolve(name),
                self.interner.resolve(segment)
            ));
        }

        self.expect(&TokenKind::LBrace)?;
        let body = self.parse_block_stmts()?;
        let end = self.expect(&TokenKind::RBrace)?;

        Ok(Stmt {
            kind: StmtKind::Namespace(Box::new(NamespaceData { name, body })),
            span: start.merge(end.span),
        })
    }

    fn parse_using_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwUsing)?;
        let (name, _) = self.expect_ident()?;

        // Check for template params
        let template_params = self.parse_template_params()?;

        // Forward declaration: `using Name;` (no `= Type`)
        if self.check(&TokenKind::Semicolon) {
            self.advance();
            return Ok(Stmt {
                kind: StmtKind::TypeAlias(Box::new(TypeAliasData {
                    name,
                    template_params,
                    ty: None,
                    attrs: Vec::new(),
                })),
                span: start,
            });
        }

        self.expect(&TokenKind::Eq)?;
        let ty = self.parse_type_expr()?;
        let attrs = self.parse_attributes()?;
        self.expect_semicolon()?;

        Ok(Stmt {
            kind: StmtKind::TypeAlias(Box::new(TypeAliasData {
                name,
                template_params,
                ty: Some(ty),
                attrs,
            })),
            span: start,
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwIf)?;
        self.expect(&TokenKind::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;

        let then_body = if self.check(&TokenKind::LBrace) {
            self.expect(&TokenKind::LBrace)?;
            let stmts = self.parse_block_stmts()?;
            self.expect(&TokenKind::RBrace)?;
            stmts
        } else {
            self.parse_top_level_stmt()?
        };

        let else_body = if self.eat(&TokenKind::KwElse) {
            if self.check(&TokenKind::KwIf) {
                // else if
                Some(vec![self.parse_if_stmt()?])
            } else if self.check(&TokenKind::LBrace) {
                self.expect(&TokenKind::LBrace)?;
                let stmts = self.parse_block_stmts()?;
                self.expect(&TokenKind::RBrace)?;
                Some(stmts)
            } else {
                Some(self.parse_top_level_stmt()?)
            }
        } else {
            None
        };

        Ok(Stmt {
            kind: StmtKind::If(Box::new(IfData {
                cond,
                then_body,
                else_body,
            })),
            span: start,
        })
    }

    fn parse_match_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwMatch)?;
        self.expect(&TokenKind::LParen)?;

        // Parse comma-separated expressions for tuple matching
        let mut exprs = vec![self.parse_expr()?];
        while self.eat(&TokenKind::Comma) {
            exprs.push(self.parse_expr()?);
        }

        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            let arm_start = self.peek_span();
            let mut patterns = Vec::new();

            loop {
                let parsed = self.parse_match_patterns()?;
                patterns.extend(parsed);

                if !self.eat(&TokenKind::Pipe) {
                    break;
                }
            }

            self.expect(&TokenKind::Colon)?;

            // Parse arm body
            let body = if self.check(&TokenKind::LBrace) {
                self.expect(&TokenKind::LBrace)?;
                let stmts = self.parse_block_stmts()?;
                self.expect(&TokenKind::RBrace)?;
                // Optional comma after braced block
                self.eat(&TokenKind::Comma);
                stmts
            } else {
                let stmts = self.parse_top_level_stmt()?;
                // Optional comma
                self.eat(&TokenKind::Comma);
                stmts
            };

            arms.push(MatchArm {
                patterns,
                body,
                span: arm_start,
            });
        }

        self.expect(&TokenKind::RBrace)?;

        Ok(Stmt {
            kind: StmtKind::Match(Box::new(MatchData { exprs, arms })),
            span: start,
        })
    }

    /// Parse a match pattern: `(expr)`, `(expr ... expr)`, `(_)`, `(a, b)` (tuple)
    /// ImHex match patterns are parenthesized.
    /// Parse one or more match patterns. Parenthesized patterns may contain `|`
    /// alternatives: `(0 ... 15 | 26 ... 53)` returns multiple patterns.
    fn parse_match_patterns(&mut self) -> Result<Vec<MatchPattern>, ParseError> {
        let prev_in_match = self.in_match_pattern;
        self.in_match_pattern = true;

        let result = self.parse_match_patterns_inner();

        self.in_match_pattern = prev_in_match;
        result
    }

    fn parse_match_patterns_inner(&mut self) -> Result<Vec<MatchPattern>, ParseError> {
        // Check for parenthesized pattern (could be single, tuple, or alternatives)
        if self.eat(&TokenKind::LParen) {
            let first = self.parse_single_match_pattern()?;

            if self.check(&TokenKind::Comma) {
                // Tuple pattern: (a, b, ...)
                let mut elements = vec![first];
                while self.eat(&TokenKind::Comma) {
                    elements.push(self.parse_single_match_pattern()?);
                }
                self.expect(&TokenKind::RParen)?;
                Ok(vec![MatchPattern::Tuple(elements)])
            } else if self.check(&TokenKind::Pipe) {
                // Alternative patterns: (a | b | c)
                let mut patterns = vec![first];
                while self.eat(&TokenKind::Pipe) {
                    patterns.push(self.parse_single_match_pattern()?);
                }
                self.expect(&TokenKind::RParen)?;
                Ok(patterns)
            } else {
                self.expect(&TokenKind::RParen)?;
                Ok(vec![first])
            }
        } else if matches!(self.peek(), TokenKind::Ident(ref s) if self.interner.resolve(*s) == "_")
        {
            self.advance();
            Ok(vec![MatchPattern::Wildcard])
        } else {
            let val = self.parse_expr()?;
            if matches!(self.peek(), TokenKind::DotDotDot) {
                self.advance();
                let end_val = self.parse_expr()?;
                Ok(vec![MatchPattern::Range(val, end_val)])
            } else {
                Ok(vec![MatchPattern::Value(val)])
            }
        }
    }

    /// Parse a single (non-tuple) match pattern element
    fn parse_single_match_pattern(&mut self) -> Result<MatchPattern, ParseError> {
        if matches!(self.peek(), TokenKind::Ident(ref s) if self.interner.resolve(*s) == "_") {
            self.advance();
            Ok(MatchPattern::Wildcard)
        } else {
            let val = self.parse_expr()?;
            if matches!(self.peek(), TokenKind::DotDotDot) {
                self.advance();
                let end_val = self.parse_expr()?;
                Ok(MatchPattern::Range(val, end_val))
            } else {
                Ok(MatchPattern::Value(val))
            }
        }
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwWhile)?;
        self.expect(&TokenKind::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;

        // Support both braced and single-statement while body
        let body = if self.check(&TokenKind::LBrace) {
            self.expect(&TokenKind::LBrace)?;
            let stmts = self.parse_block_stmts()?;
            self.expect(&TokenKind::RBrace)?;
            stmts
        } else {
            self.parse_top_level_stmt()?
        };

        Ok(Stmt {
            kind: StmtKind::While(Box::new(WhileData { cond, body })),
            span: start,
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwFor)?;
        self.expect(&TokenKind::LParen)?;

        // Parse init (may produce multiple stmts for comma-separated decls like `u8 a, b, c`)
        let init_stmts = self.parse_var_decl_or_expr_stmt_no_semi(Vec::new())?;
        // Accept comma or semicolon as separator
        self.expect_comma_or_semicolon()?;

        let cond = self.parse_expr()?;
        // Accept comma or semicolon as separator
        self.expect_comma_or_semicolon()?;

        // Step expression (assignment is now part of expression grammar)
        let step_expr = self.parse_expr()?;
        let step = Stmt {
            span: step_expr.span,
            kind: StmtKind::ExprStmt(step_expr),
        };

        self.expect(&TokenKind::RParen)?;

        // Support both braced and single-statement for body
        let body = if self.check(&TokenKind::LBrace) {
            self.expect(&TokenKind::LBrace)?;
            let stmts = self.parse_block_stmts()?;
            self.expect(&TokenKind::RBrace)?;
            stmts
        } else {
            self.parse_top_level_stmt()?
        };

        Ok(Stmt {
            kind: StmtKind::For(Box::new(ForData {
                init: init_stmts,
                cond,
                step: Box::new(step),
                body,
            })),
            span: start,
        })
    }

    fn parse_try_catch(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwTry)?;
        self.expect(&TokenKind::LBrace)?;
        let try_body = self.parse_block_stmts()?;
        self.expect(&TokenKind::RBrace)?;

        let catch_body = if self.eat(&TokenKind::KwCatch) {
            self.expect(&TokenKind::LBrace)?;
            let body = self.parse_block_stmts()?;
            self.expect(&TokenKind::RBrace)?;
            Some(body)
        } else {
            None
        };

        Ok(Stmt {
            kind: StmtKind::TryCatch(Box::new(TryCatchData {
                try_body,
                catch_body,
            })),
            span: start,
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwReturn)?;
        let value = if !self.check(&TokenKind::Semicolon) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect_semicolon()?;
        Ok(Stmt {
            kind: StmtKind::Return(value),
            span: start,
        })
    }

    fn parse_import_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::KwImport)?;

        // Handle `import * from X as Y;` (wildcard import)
        if self.eat(&TokenKind::Star) {
            // Skip `from X as Y`
            while !self.check(&TokenKind::Semicolon) && !self.at_eof() {
                self.advance();
            }
            self.expect_semicolon()?;
            return Ok(Stmt {
                kind: StmtKind::Import {
                    path: vec![self.interner.intern("*")],
                },
                span: start,
            });
        }

        let mut path = Vec::new();
        let (first, _) = self.expect_ident()?;
        path.push(first);
        while self.eat(&TokenKind::Dot) {
            let (segment, _) = self.expect_ident()?;
            path.push(segment);
        }
        self.expect_semicolon()?;
        Ok(Stmt {
            kind: StmtKind::Import { path },
            span: start,
        })
    }

    /// Parse a variable declaration or expression statement
    /// This is the trickiest part because we need to look ahead to
    /// determine if we're looking at a type followed by a name.
    fn parse_var_decl_or_expr_stmt(
        &mut self,
        attrs: Vec<Attribute>,
    ) -> Result<Vec<Stmt>, ParseError> {
        let stmts = self.parse_var_decl_or_expr_stmt_no_semi(attrs)?;
        self.expect_semicolon()?;
        Ok(stmts)
    }

    /// Same as `parse_var_decl_or_expr_stmt` but does NOT consume the trailing semicolon.
    /// Used by `parse_for_stmt` where the separator can be comma or semicolon.
    /// Returns a Vec because comma-separated declarations (e.g. `u8 r, g, b`) produce multiple stmts.
    fn parse_var_decl_or_expr_stmt_no_semi(
        &mut self,
        attrs: Vec<Attribute>,
    ) -> Result<Vec<Stmt>, ParseError> {
        let start = self.peek_span();
        if self.looks_like_var_decl_fast() {
            self.parse_typed_decl(attrs, start)
        } else {
            let expr = self.parse_expr()?;
            Ok(vec![Stmt {
                kind: StmtKind::ExprStmt(expr),
                span: start,
            }])
        }
    }

    /// Parse a typed declaration after determining it starts with a type.
    /// Handles: variable declarations, pointer declarations, anonymous placements,
    /// comma-separated declarations, and placement/initializer modifiers.
    fn parse_typed_decl(
        &mut self,
        attrs: Vec<Attribute>,
        start: Span,
    ) -> Result<Vec<Stmt>, ParseError> {
        let mut ty = self.parse_type_expr()?;

        // Check for pointer declaration: `Type *name : SizeType;`
        let is_pointer = self.eat(&TokenKind::Star);

        // Anonymous placement: `Type @ expr;`, `Type;`, `Type<args>;`, `Type[[attr]];`
        let name = if !is_pointer
            && (matches!(
                self.peek(),
                TokenKind::At | TokenKind::Semicolon | TokenKind::Comma
            ) || self.check_lattr())
        {
            self.interner.intern("")
        } else {
            self.expect_ident()?.0
        };

        // Handle C-style array declaration: `Type name[size]`
        ty = self.parse_array_dims(ty)?;

        // For pointer declarations, parse `: SizeType`
        let pointer_size = if is_pointer && self.eat(&TokenKind::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        // Check for placement: `@ expr`
        let placement = if self.eat(&TokenKind::At) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        // Check for initializer: `= expr`
        let init = if self.eat(&TokenKind::Eq) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        // Parse `in section` or skip `out` specifier
        let section = self.parse_section_specifier()?;

        // Handle comma-separated variable declarations: `u8 r, g, b;`
        let extra_stmts = if placement.is_none() && init.is_none() {
            self.parse_comma_separated_decls(&ty)?
        } else {
            Vec::new()
        };

        // Parse post-declaration attributes: `Type name [[attr]];`
        let all_attrs = self.parse_and_merge_attrs(attrs)?;

        let stmt = if self.interner.resolve(name).is_empty() {
            Stmt {
                kind: StmtKind::AnonymousPlacement(Box::new(AnonymousPlacementData {
                    ty,
                    placement,
                    attrs: all_attrs,
                    section,
                })),
                span: start,
            }
        } else if let Some(init_expr) = init {
            Stmt {
                kind: StmtKind::LocalVar {
                    ty,
                    name,
                    init: init_expr,
                },
                span: start,
            }
        } else {
            Stmt {
                kind: StmtKind::DataPlacement(Box::new(DataPlacementData {
                    ty,
                    name,
                    offset: placement,
                    attrs: all_attrs,
                    pointer_size,
                    section,
                })),
                span: start,
            }
        };
        let mut result = vec![stmt];
        result.extend(extra_stmts);
        Ok(result)
    }

    /// Parse `in expr` section specifier. Bare `in` / `out` specifiers are UI-only (skipped).
    fn parse_section_specifier(&mut self) -> Result<Option<Expr>, ParseError> {
        if matches!(self.peek(), TokenKind::KwIn) {
            // Bare `in;` or `in,` or `in [[` is a UI input specifier (no section).
            // `in <expr>` with an expression following is a section specifier.
            if matches!(
                self.peek_ahead(1),
                TokenKind::Semicolon | TokenKind::Comma | TokenKind::LBracket
            ) {
                self.advance(); // consume bare `in`
                Ok(None)
            } else {
                self.advance(); // consume `in`
                Ok(Some(self.parse_expr()?))
            }
        } else if self.eat(&TokenKind::KwOut) {
            // `out` is a UI-only specifier, skip its argument
            if matches!(self.peek(), TokenKind::Ident(_) | TokenKind::IntLiteral(_)) {
                self.advance();
            }
            Ok(None)
        } else {
            Ok(None)
        }
    }

    /// Parse comma-separated extra declarations: `, b, c[2], d`
    /// Each extra variable shares the base type but can have its own array dimensions.
    fn parse_comma_separated_decls(&mut self, base_ty: &TypeExpr) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while matches!(self.peek(), TokenKind::Comma) {
            self.advance();
            let (extra_name, extra_span) = self.expect_ident()?;
            let extra_ty = self.parse_array_dims(base_ty.clone())?;
            stmts.push(Stmt {
                kind: StmtKind::DataPlacement(Box::new(DataPlacementData {
                    ty: extra_ty,
                    name: extra_name,
                    offset: None,
                    attrs: Vec::new(),
                    pointer_size: None,
                    section: None,
                })),
                span: extra_span,
            });
        }
        Ok(stmts)
    }

    fn try_parse_assign_op(&mut self) -> Option<AssignOp> {
        let op = match self.peek() {
            TokenKind::Eq => AssignOp::Assign,
            TokenKind::PlusEq => AssignOp::AddAssign,
            TokenKind::MinusEq => AssignOp::SubAssign,
            TokenKind::StarEq => AssignOp::MulAssign,
            TokenKind::SlashEq => AssignOp::DivAssign,
            TokenKind::PercentEq => AssignOp::ModAssign,
            TokenKind::AmpEq => AssignOp::BitAndAssign,
            TokenKind::PipeEq => AssignOp::BitOrAssign,
            TokenKind::CaretEq => AssignOp::BitXorAssign,
            TokenKind::LShiftEq => AssignOp::ShlAssign,
            TokenKind::RShiftEq => AssignOp::ShrAssign,
            _ => return None,
        };
        self.advance();
        Some(op)
    }

    /// Heuristic to detect variable declarations
    /// Fast check for unambiguous var decl patterns (builtin types, endian prefixes, etc.).
    /// For user-defined types, uses try_parse to attempt type + name parsing.
    fn looks_like_var_decl_fast(&mut self) -> bool {
        match self.peek() {
            // Endian prefix always starts a type
            TokenKind::KwLe | TokenKind::KwBe | TokenKind::KwAuto | TokenKind::KwConst => true,
            TokenKind::Ident(name) => {
                let name_str = self.interner.resolve(*name);
                // `padding[...]` is a type-as-statement (anonymous var decl)
                if name_str == "padding" && matches!(self.peek_ahead(1), TokenKind::LBracket) {
                    return true;
                }
                // If it's a builtin type or arbitrary int, it's a var decl
                if BuiltinType::from_str(name_str).is_some()
                    || Self::is_arbitrary_int_name(name_str)
                {
                    // But only if followed by identifier, array notation, or pointer star
                    if matches!(self.peek_ahead(1), TokenKind::Star) {
                        // Pointer: `u32 *name`
                        return matches!(self.peek_ahead(2), TokenKind::Ident(_));
                    }
                    matches!(
                        self.peek_ahead(1),
                        TokenKind::Ident(_) | TokenKind::LBracket
                    )
                } else {
                    // User-defined type: use try_parse to verify.
                    // Attempt to parse `type name` or `type *name`; backtrack on failure.
                    self.try_parse(|p| {
                        let ty = p.parse_type_expr()?;
                        // After type, must see identifier, `*`, or `@`
                        match p.peek() {
                            TokenKind::Ident(_) | TokenKind::Star | TokenKind::At => Ok(()),
                            // `;` or `,` only count as anonymous placement if the type
                            // has scope (::) or template args (<>), to avoid misinterpreting
                            // `arr[0];` or `foo;` as anonymous type placement.
                            TokenKind::Semicolon | TokenKind::Comma => {
                                if has_scope_or_template_type(&ty) {
                                    Ok(())
                                } else {
                                    Err(ParseError::expected(
                                        "ambiguous: could be expression",
                                        "identifier or placement",
                                        p.peek_span(),
                                    ))
                                }
                            }
                            TokenKind::LBracket
                                if matches!(p.peek_ahead(1), TokenKind::LBracket) =>
                            {
                                Ok(())
                            }
                            _ => Err(ParseError::expected(
                                "not a variable declaration",
                                "identifier or placement",
                                p.peek_span(),
                            )),
                        }
                    })
                    .is_some()
                }
            }
            _ => false,
        }
    }

    /// Find the colon in a bitfield field pattern.
    /// Returns the offset of the identifier before the colon (0-based from current pos),
    /// or None if this doesn't look like a bitfield field.
    /// Patterns: `name : width;` (offset=0), `Type name : width;` (offset=1),
    /// `ns::Type name : width;` (offset=3, skipping ::), etc.
    fn find_bitfield_colon(&self) -> Option<usize> {
        // Look ahead for a colon within a reasonable range
        for i in 0..MAX_BITFIELD_LOOKAHEAD {
            match self.peek_ahead(i) {
                TokenKind::Colon => {
                    // The token before the colon should be an identifier (the field name)
                    if i == 0 {
                        return None; // colon with nothing before it
                    }
                    if matches!(self.peek_ahead(i - 1), TokenKind::Ident(_)) {
                        return Some(i - 1);
                    }
                    return None;
                }
                TokenKind::Semicolon | TokenKind::RBrace | TokenKind::LBrace | TokenKind::Eof => {
                    return None
                }
                // Keywords/tokens that indicate this is NOT a bitfield field
                TokenKind::KwIf
                | TokenKind::KwFor
                | TokenKind::KwWhile
                | TokenKind::KwMatch
                | TokenKind::KwReturn
                | TokenKind::KwBreak
                | TokenKind::KwContinue
                | TokenKind::KwTry => return None,
                // Allow: identifiers, ::, keywords that might be type names (bool, etc.)
                _ => continue,
            }
        }
        None
    }

    fn parse_block_stmts(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.block_depth += 1;
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            // Skip empty statements
            if self.eat(&TokenKind::Semicolon) {
                continue;
            }
            match self.parse_top_level_stmt() {
                Ok(parsed) => {
                    stmts.extend(parsed);
                }
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }
        self.block_depth -= 1;
        Ok(stmts)
    }

    // ========== Expression parsing (Pratt parser) ==========

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    /// Assignment is the lowest-precedence expression (right-associative).
    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_ternary()?;
        if let Some(op) = self.try_parse_assign_op() {
            let rhs = self.parse_assignment()?;
            Ok(Expr {
                span: lhs.span.merge(rhs.span),
                kind: ExprKind::Assign {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            })
        } else {
            Ok(lhs)
        }
    }

    fn parse_ternary(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_logical_or()?;

        if self.eat(&TokenKind::Question) {
            let then_expr = self.parse_expr()?;
            self.expect(&TokenKind::Colon)?;
            let else_expr = self.parse_expr()?;
            Ok(Expr {
                span: expr.span.merge(else_expr.span),
                kind: ExprKind::Ternary {
                    cond: Box::new(expr),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                },
            })
        } else {
            Ok(expr)
        }
    }

    /// Helper: parse left-associative binary operators.
    /// Calls `next_fn` for operands, `map_op` to convert current token to BinOp.
    fn parse_binary_op(
        &mut self,
        mut next_fn: impl FnMut(&mut Self) -> Result<Expr, ParseError>,
        map_op: impl Fn(&TokenKind) -> Option<BinOp>,
    ) -> Result<Expr, ParseError> {
        let mut lhs = next_fn(self)?;
        while let Some(op) = map_op(&self.peek()) {
            self.advance();
            let rhs = next_fn(self)?;
            lhs = Expr {
                span: lhs.span.merge(rhs.span),
                kind: ExprKind::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_logical_and, |tok| match tok {
            TokenKind::PipePipe => Some(BinOp::LogOr),
            _ => None,
        })
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_bitwise_or, |tok| match tok {
            TokenKind::AmpAmp => Some(BinOp::LogAnd),
            _ => None,
        })
    }

    fn parse_bitwise_or(&mut self) -> Result<Expr, ParseError> {
        let in_match = self.in_match_pattern;
        self.parse_binary_op(Self::parse_bitwise_xor, |tok| match tok {
            TokenKind::Pipe if !in_match => Some(BinOp::BitOr),
            _ => None,
        })
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_bitwise_and, |tok| match tok {
            TokenKind::Caret => Some(BinOp::BitXor),
            _ => None,
        })
    }

    fn parse_bitwise_and(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_equality, |tok| match tok {
            TokenKind::Ampersand => Some(BinOp::BitAnd),
            _ => None,
        })
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_comparison, |tok| match tok {
            TokenKind::EqEq => Some(BinOp::Eq),
            TokenKind::BangEq => Some(BinOp::Ne),
            _ => None,
        })
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let in_template = self.in_template_args;
        self.parse_binary_op(Self::parse_shift, |tok| match tok {
            TokenKind::Less => Some(BinOp::Lt),
            TokenKind::LessEq => Some(BinOp::Le),
            // Inside template args, `>` and `>=` close the template — not comparison
            TokenKind::Greater if !in_template => Some(BinOp::Gt),
            TokenKind::GreaterEq if !in_template => Some(BinOp::Ge),
            _ => None,
        })
    }

    fn parse_shift(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_additive, |tok| match tok {
            TokenKind::LShift => Some(BinOp::Shl),
            TokenKind::RShift => Some(BinOp::Shr),
            _ => None,
        })
    }

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_multiplicative, |tok| match tok {
            TokenKind::Plus => Some(BinOp::Add),
            TokenKind::Minus => Some(BinOp::Sub),
            _ => None,
        })
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_op(Self::parse_unary, |tok| match tok {
            TokenKind::Star => Some(BinOp::Mul),
            TokenKind::Slash => Some(BinOp::Div),
            TokenKind::Percent => Some(BinOp::Mod),
            _ => None,
        })
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let start = self.peek_span();
        match self.peek() {
            TokenKind::Minus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr {
                    span: start.merge(expr.span),
                    kind: ExprKind::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(expr),
                    },
                })
            }
            TokenKind::Bang => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr {
                    span: start.merge(expr.span),
                    kind: ExprKind::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    },
                })
            }
            TokenKind::Tilde => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr {
                    span: start.merge(expr.span),
                    kind: ExprKind::Unary {
                        op: UnaryOp::BitNot,
                        expr: Box::new(expr),
                    },
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek() {
                TokenKind::LParen => {
                    // Function call
                    self.advance();
                    let mut args = Vec::new();
                    while !self.check(&TokenKind::RParen) && !self.at_eof() {
                        args.push(self.parse_expr()?);
                        if !self.eat(&TokenKind::Comma) {
                            break;
                        }
                    }
                    let end = self.expect(&TokenKind::RParen)?;
                    expr = Expr {
                        span: expr.span.merge(end.span),
                        kind: ExprKind::Call {
                            func: Box::new(expr),
                            args,
                        },
                    };
                }
                TokenKind::LBracket if !self.check_lattr() => {
                    // Array index (not `[[` attribute syntax)
                    self.advance();
                    let index = self.parse_expr()?;
                    let end = self.expect(&TokenKind::RBracket)?;
                    expr = Expr {
                        span: expr.span.merge(end.span),
                        kind: ExprKind::Index {
                            expr: Box::new(expr),
                            index: Box::new(index),
                        },
                    };
                }
                TokenKind::Dot => {
                    // Member access
                    self.advance();
                    let member_span = self.peek_span();
                    let member = self.expect_ident()?.0;
                    expr = Expr {
                        span: expr.span.merge(member_span),
                        kind: ExprKind::MemberAccess {
                            expr: Box::new(expr),
                            member,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let start = self.peek_span();

        match self.peek().clone() {
            TokenKind::IntLiteral(val) => self.parse_literal(ExprKind::IntLiteral(val), start),
            TokenKind::FloatLiteral(val) => self.parse_literal(ExprKind::FloatLiteral(val), start),
            TokenKind::StringLiteral(val) => {
                self.parse_literal(ExprKind::StringLiteral(val), start)
            }
            TokenKind::CharLiteral(val) => self.parse_literal(ExprKind::CharLiteral(val), start),
            TokenKind::KwTrue => self.parse_literal(ExprKind::BoolLiteral(true), start),
            TokenKind::KwFalse => self.parse_literal(ExprKind::BoolLiteral(false), start),
            TokenKind::KwNull => self.parse_literal(ExprKind::Null, start),
            TokenKind::Dollar => self.parse_literal(ExprKind::Dollar, start),
            TokenKind::KwSizeof => self.parse_sizeof(start),
            TokenKind::KwAddressof => self.parse_addressof(start),
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::KwBe | TokenKind::KwLe => self.parse_endian_cast(start),
            TokenKind::Ident(name) => self.parse_ident_or_scoped(name, start),
            TokenKind::LBrace => self.parse_initializer_list(start),
            _ => Err(ParseError::expected(
                format!("unexpected token {}", self.peek()),
                "expression",
                start,
            )),
        }
    }

    fn parse_literal(&mut self, kind: ExprKind, span: Span) -> Result<Expr, ParseError> {
        self.advance();
        Ok(Expr { kind, span })
    }

    fn parse_sizeof(&mut self, start: Span) -> Result<Expr, ParseError> {
        self.advance();
        self.expect(&TokenKind::LParen)?;
        // In sizeof context, an identifier followed by ')' is a type name
        // (handles user-defined aliases like DWORD, MyType, etc.)
        let is_type = self.looks_like_type()
            || matches!(
                (&self.peek(), &self.peek_ahead(1)),
                (TokenKind::Ident(_), TokenKind::RParen)
            );
        let inner = if is_type {
            let ty = self.parse_type_expr()?;
            TypeExprOrExpr::Type(ty)
        } else {
            let expr = self.parse_expr()?;
            TypeExprOrExpr::Expr(expr)
        };
        self.expect(&TokenKind::RParen)?;
        Ok(Expr {
            kind: ExprKind::Sizeof(Box::new(inner)),
            span: start,
        })
    }

    fn parse_addressof(&mut self, start: Span) -> Result<Expr, ParseError> {
        self.advance();
        self.expect(&TokenKind::LParen)?;
        let inner_expr = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        Ok(Expr {
            kind: ExprKind::Addressof(Box::new(inner_expr)),
            span: start,
        })
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, ParseError> {
        self.advance();
        let expr = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        Ok(expr)
    }

    fn parse_endian_cast(&mut self, start: Span) -> Result<Expr, ParseError> {
        let endian = if matches!(self.peek(), TokenKind::KwBe) {
            Endianness::Big
        } else {
            Endianness::Little
        };
        self.advance();
        let base_ty = self.parse_base_type()?;
        let ty = TypeExpr {
            span: start.merge(base_ty.span),
            kind: TypeExprKind::Endian(endian, Box::new(base_ty)),
        };
        self.expect(&TokenKind::LParen)?;
        let expr = self.parse_expr()?;
        let end = self.expect(&TokenKind::RParen)?;
        Ok(Expr {
            kind: ExprKind::Cast {
                ty: Box::new(ty),
                expr: Box::new(expr),
            },
            span: start.merge(end.span),
        })
    }

    fn parse_ident_or_scoped(&mut self, name: Name, start: Span) -> Result<Expr, ParseError> {
        self.advance();
        if matches!(self.peek(), TokenKind::ColonColon) {
            let mut path = vec![name];
            while self.eat(&TokenKind::ColonColon) {
                let (segment, _) = self.expect_ident()?;
                path.push(segment);
            }
            Ok(Expr {
                kind: ExprKind::ScopedIdent(path),
                span: start,
            })
        } else {
            Ok(Expr {
                kind: ExprKind::Ident(name),
                span: start,
            })
        }
    }

    fn parse_initializer_list(&mut self, start: Span) -> Result<Expr, ParseError> {
        self.advance();
        let mut elements = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.at_eof() {
            elements.push(self.parse_expr()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        let end = self.expect(&TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::InitializerList(elements),
            span: start.merge(end.span),
        })
    }
}

/// Check if a type expression uses scope resolution (::), template args (<>), or endian prefix.
/// Used to disambiguate `Type;` (anonymous placement) from `expr;` (expression statement).
fn has_scope_or_template_type(ty: &TypeExpr) -> bool {
    match &ty.kind {
        TypeExprKind::Template(..) | TypeExprKind::Endian(..) => true,
        TypeExprKind::Named(path) if path.len() > 1 => true,
        TypeExprKind::Array(inner, _) => has_scope_or_template_type(inner),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::span::SourceId;

    fn parse(input: &str) -> Ast {
        let mut interner = crate::name::StringInterner::new();
        let lexer = Lexer::new(input, SourceId(0), &mut interner);
        let (tokens, lex_errors) = lexer.tokenize();
        assert!(lex_errors.is_empty(), "lex errors: {:?}", lex_errors);
        let parser = Parser::new(tokens, &mut interner);
        parser.parse().expect("parse failed")
    }

    fn parse_with_interner(input: &str) -> (Ast, crate::name::StringInterner) {
        let mut interner = crate::name::StringInterner::new();
        let lexer = Lexer::new(input, SourceId(0), &mut interner);
        let (tokens, lex_errors) = lexer.tokenize();
        assert!(lex_errors.is_empty(), "lex errors: {:?}", lex_errors);
        let parser = Parser::new(tokens, &mut interner);
        let ast = parser.parse().expect("parse failed");
        (ast, interner)
    }

    fn parse_err(input: &str) -> Vec<ParseError> {
        let mut interner = crate::name::StringInterner::new();
        let lexer = Lexer::new(input, SourceId(0), &mut interner);
        let (tokens, _) = lexer.tokenize();
        let parser = Parser::new(tokens, &mut interner);
        parser.parse().unwrap_err()
    }

    // ========== Variable declarations ==========

    #[test]
    fn test_var_decl_simple() {
        let (ast, interner) = parse_with_interner("u32 magic;");
        assert_eq!(ast.stmts.len(), 1);
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(interner.resolve(d.name), "magic");
                assert!(d.offset.is_none());
                matches!(&d.ty.kind, TypeExprKind::Builtin(BuiltinType::U32));
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    #[test]
    fn test_var_decl_with_placement() {
        let (ast, interner) = parse_with_interner("u8 x @ 0x10;");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(interner.resolve(d.name), "x");
                assert!(d.offset.is_some());
                match &d.offset.as_ref().unwrap().kind {
                    ExprKind::IntLiteral(16) => {}
                    other => panic!("expected IntLiteral(16), got {:?}", other),
                }
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    #[test]
    fn test_var_decl_with_init() {
        let (ast, interner) = parse_with_interner("u32 x = 42;");
        match &ast.stmts[0].kind {
            StmtKind::LocalVar { name, init, .. } => {
                assert_eq!(interner.resolve(*name), "x");
                // init is always present in LocalVar, just verify the name
                let _ = init;
            }
            other => panic!("expected LocalVar, got {:?}", other),
        }
    }

    #[test]
    fn test_var_decl_endian() {
        let (ast, interner) = parse_with_interner("le u32 value;");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(interner.resolve(d.name), "value");
                match &d.ty.kind {
                    TypeExprKind::Endian(Endianness::Little, inner) => {
                        matches!(&inner.kind, TypeExprKind::Builtin(BuiltinType::U32));
                    }
                    other => panic!("expected Endian type, got {:?}", other),
                }
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    #[test]
    fn test_var_decl_array() {
        let (ast, interner) = parse_with_interner("u8 data[16];");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(interner.resolve(d.name), "data");
                matches!(&d.ty.kind, TypeExprKind::Array(_, ArraySize::Fixed(_)));
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    // ========== Struct definitions ==========

    #[test]
    fn test_struct_def() {
        let (ast, interner) = parse_with_interner("struct Header { u32 magic; u16 version; };");
        match &ast.stmts[0].kind {
            StmtKind::StructDef(d) => {
                assert_eq!(interner.resolve(d.name), "Header");
                assert_eq!(d.body.len(), 2);
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_struct_with_inheritance() {
        let (ast, interner) = parse_with_interner("struct Child : Parent { u8 extra; };");
        match &ast.stmts[0].kind {
            StmtKind::StructDef(d) => {
                assert_eq!(interner.resolve(d.name), "Child");
                assert_eq!(
                    d.parent.as_ref().map(|(n, _)| interner.resolve(*n)),
                    Some("Parent")
                );
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_struct_with_template() {
        let (ast, interner) = parse_with_interner("struct Container<T> { T value; };");
        match &ast.stmts[0].kind {
            StmtKind::StructDef(d) => {
                assert_eq!(interner.resolve(d.name), "Container");
                assert_eq!(d.template_params.len(), 1);
                assert_eq!(interner.resolve(d.template_params[0].name), "T");
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    // ========== Union definitions ==========

    #[test]
    fn test_union_def() {
        let (ast, interner) = parse_with_interner("union Data { u32 as_int; float as_float; };");
        match &ast.stmts[0].kind {
            StmtKind::UnionDef(d) => {
                assert_eq!(interner.resolve(d.name), "Data");
                assert_eq!(d.body.len(), 2);
            }
            other => panic!("expected UnionDef, got {:?}", other),
        }
    }

    // ========== Enum definitions ==========

    #[test]
    fn test_enum_def() {
        let (ast, interner) =
            parse_with_interner("enum Color : u8 { Red = 0, Green = 1, Blue = 2 };");
        match &ast.stmts[0].kind {
            StmtKind::EnumDef(d) => {
                assert_eq!(interner.resolve(d.name), "Color");
                assert_eq!(d.members.len(), 3);
                assert_eq!(interner.resolve(d.members[0].name), "Red");
                assert_eq!(interner.resolve(d.members[1].name), "Green");
                assert_eq!(interner.resolve(d.members[2].name), "Blue");
            }
            other => panic!("expected EnumDef, got {:?}", other),
        }
    }

    // ========== Bitfield definitions ==========

    #[test]
    fn test_bitfield_def() {
        let (ast, interner) = parse_with_interner("bitfield Flags { a : 1; b : 3; c : 4; };");
        match &ast.stmts[0].kind {
            StmtKind::BitfieldDef(d) => {
                assert_eq!(interner.resolve(d.name), "Flags");
                assert_eq!(d.body.len(), 3);
                match &d.body[0].kind {
                    StmtKind::BitfieldFieldStmt { name, .. } => {
                        assert_eq!(interner.resolve(*name), "a")
                    }
                    other => panic!("expected BitfieldFieldStmt, got {:?}", other),
                }
            }
            other => panic!("expected BitfieldDef, got {:?}", other),
        }
    }

    // ========== Function definitions ==========

    #[test]
    fn test_fn_def() {
        let (ast, interner) = parse_with_interner("fn add(u32 a, u32 b) -> u32 { return a + b; }");
        match &ast.stmts[0].kind {
            StmtKind::FnDef(d) => {
                assert_eq!(interner.resolve(d.name), "add");
                assert_eq!(d.params.len(), 2);
                assert!(d.return_ty.is_some());
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected FnDef, got {:?}", other),
        }
    }

    #[test]
    fn test_fn_def_no_return_type() {
        let (ast, interner) = parse_with_interner("fn hello() { }");
        match &ast.stmts[0].kind {
            StmtKind::FnDef(d) => {
                assert_eq!(interner.resolve(d.name), "hello");
                assert!(d.return_ty.is_none());
            }
            other => panic!("expected FnDef, got {:?}", other),
        }
    }

    // ========== Using / Type alias ==========

    #[test]
    fn test_using_alias() {
        let (ast, interner) = parse_with_interner("using DWORD = u32;");
        match &ast.stmts[0].kind {
            StmtKind::TypeAlias(d) => {
                assert_eq!(interner.resolve(d.name), "DWORD");
                assert!(d.ty.is_some());
                matches!(
                    &d.ty.as_ref().unwrap().kind,
                    TypeExprKind::Builtin(BuiltinType::U32)
                );
            }
            other => panic!("expected TypeAlias, got {:?}", other),
        }
    }

    // ========== Namespace ==========

    #[test]
    fn test_namespace() {
        let (ast, interner) = parse_with_interner("namespace my_ns { struct Foo { u8 x; }; }");
        match &ast.stmts[0].kind {
            StmtKind::Namespace(d) => {
                assert_eq!(interner.resolve(d.name), "my_ns");
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected Namespace, got {:?}", other),
        }
    }

    #[test]
    fn test_namespace_nested() {
        let (ast, interner) = parse_with_interner("namespace a::b { fn foo() { return 1; }; }");
        match &ast.stmts[0].kind {
            StmtKind::Namespace(d) => {
                assert_eq!(interner.resolve(d.name), "a::b");
                assert_eq!(d.body.len(), 1);
                assert!(matches!(&d.body[0].kind, StmtKind::FnDef(_)));
            }
            other => panic!("expected Namespace, got {:?}", other),
        }
    }

    #[test]
    fn test_namespace_auto() {
        let (ast, interner) =
            parse_with_interner("namespace auto my_ns { struct Bar { u16 y; }; }");
        match &ast.stmts[0].kind {
            StmtKind::Namespace(d) => {
                assert_eq!(interner.resolve(d.name), "my_ns");
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected Namespace, got {:?}", other),
        }
    }

    #[test]
    fn test_namespace_multiple_items() {
        let source = r#"
            namespace ns {
                struct A { u8 x; };
                struct B { u16 y; };
                fn helper() { return 0; };
            }
        "#;
        let (ast, interner) = parse_with_interner(source);
        match &ast.stmts[0].kind {
            StmtKind::Namespace(d) => {
                assert_eq!(interner.resolve(d.name), "ns");
                assert_eq!(d.body.len(), 3);
            }
            other => panic!("expected Namespace, got {:?}", other),
        }
    }

    #[test]
    fn test_namespace_inside_struct() {
        let source = r#"
            struct Outer {
                namespace inner {
                    struct Nested { u8 a; };
                };
            };
        "#;
        let (ast, interner) = parse_with_interner(source);
        match &ast.stmts[0].kind {
            StmtKind::StructDef(d) => {
                assert_eq!(interner.resolve(d.name), "Outer");
                assert_eq!(d.body.len(), 1);
                match &d.body[0].kind {
                    StmtKind::Namespace(nd) => {
                        assert_eq!(interner.resolve(nd.name), "inner");
                        assert_eq!(nd.body.len(), 1);
                    }
                    other => panic!("expected Namespace inside struct, got {:?}", other),
                }
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_namespace_inside_function() {
        let source = r#"
            fn test() {
                namespace local {
                    struct Helper { u8 v; };
                };
            };
        "#;
        let (ast, interner) = parse_with_interner(source);
        match &ast.stmts[0].kind {
            StmtKind::FnDef(d) => {
                assert_eq!(d.body.len(), 1);
                match &d.body[0].kind {
                    StmtKind::Namespace(nd) => {
                        assert_eq!(interner.resolve(nd.name), "local");
                        assert_eq!(nd.body.len(), 1);
                    }
                    other => panic!("expected Namespace inside fn, got {:?}", other),
                }
            }
            other => panic!("expected FnDef, got {:?}", other),
        }
    }

    #[test]
    fn test_namespace_inside_if() {
        let source = r#"
            if (true) {
                namespace cond_ns {
                    struct X { u8 a; };
                };
            }
        "#;
        let (ast, interner) = parse_with_interner(source);
        match &ast.stmts[0].kind {
            StmtKind::If(d) => {
                assert_eq!(d.then_body.len(), 1);
                match &d.then_body[0].kind {
                    StmtKind::Namespace(nd) => {
                        assert_eq!(interner.resolve(nd.name), "cond_ns");
                    }
                    other => panic!("expected Namespace inside if, got {:?}", other),
                }
            }
            other => panic!("expected If, got {:?}", other),
        }
    }

    #[test]
    fn test_namespace_deeply_nested_path() {
        let (ast, interner) =
            parse_with_interner("namespace a::b::c::d { struct Deep { u8 v; }; }");
        match &ast.stmts[0].kind {
            StmtKind::Namespace(d) => {
                assert_eq!(interner.resolve(d.name), "a::b::c::d");
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected Namespace, got {:?}", other),
        }
    }

    // ========== Control flow ==========

    #[test]
    fn test_if_stmt() {
        let ast = parse("if (x == 1) { u8 a; }");
        match &ast.stmts[0].kind {
            StmtKind::If(d) => {
                assert_eq!(d.then_body.len(), 1);
                assert!(d.else_body.is_none());
            }
            other => panic!("expected If, got {:?}", other),
        }
    }

    #[test]
    fn test_if_else_stmt() {
        let ast = parse("if (x == 1) { u8 a; } else { u8 b; }");
        match &ast.stmts[0].kind {
            StmtKind::If(d) => {
                assert_eq!(d.then_body.len(), 1);
                assert!(d.else_body.is_some());
                assert_eq!(d.else_body.as_ref().unwrap().len(), 1);
            }
            other => panic!("expected If, got {:?}", other),
        }
    }

    #[test]
    fn test_while_stmt() {
        let ast = parse("while (i < 10) { i = i + 1; }");
        match &ast.stmts[0].kind {
            StmtKind::While(d) => {
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected While, got {:?}", other),
        }
    }

    #[test]
    fn test_for_stmt() {
        let ast = parse("for (u32 i = 0; i < 10; i = i + 1) { }");
        match &ast.stmts[0].kind {
            StmtKind::For(d) => {
                assert!(d.body.is_empty());
            }
            other => panic!("expected For, got {:?}", other),
        }
    }

    #[test]
    fn test_for_stmt_comma_separator() {
        // ImHex pattern language supports comma as separator in for statements
        let ast = parse("for (s32 i = 0, i <= 10, i += 1) { }");
        match &ast.stmts[0].kind {
            StmtKind::For(d) => {
                assert!(d.body.is_empty());
            }
            other => panic!("expected For, got {:?}", other),
        }
    }

    #[test]
    fn test_match_stmt() {
        let ast = parse("match (x) { (0): { } (1 ... 5): { } (_): { } }");
        match &ast.stmts[0].kind {
            StmtKind::Match(d) => {
                assert_eq!(d.exprs.len(), 1);
                assert_eq!(d.arms.len(), 3);
                assert!(matches!(&d.arms[0].patterns[0], MatchPattern::Value(_)));
                assert!(matches!(&d.arms[1].patterns[0], MatchPattern::Range(_, _)));
                assert!(matches!(&d.arms[2].patterns[0], MatchPattern::Wildcard));
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_match_tuple() {
        let ast = parse("match (a, b) { (0, 1): { } (_, _): { } }");
        match &ast.stmts[0].kind {
            StmtKind::Match(d) => {
                assert_eq!(d.exprs.len(), 2);
                assert_eq!(d.arms.len(), 2);
                assert!(
                    matches!(&d.arms[0].patterns[0], MatchPattern::Tuple(elems) if elems.len() == 2)
                );
                assert!(
                    matches!(&d.arms[1].patterns[0], MatchPattern::Tuple(elems) if elems.len() == 2)
                );
            }
            other => panic!("expected Match, got {:?}", other),
        }
    }

    #[test]
    fn test_try_catch() {
        let ast = parse("try { u8 x; } catch { u8 y; }");
        match &ast.stmts[0].kind {
            StmtKind::TryCatch(d) => {
                assert_eq!(d.try_body.len(), 1);
                assert_eq!(d.catch_body.as_ref().unwrap().len(), 1);
            }
            other => panic!("expected TryCatch, got {:?}", other),
        }
    }

    #[test]
    fn test_try_without_catch() {
        let ast = parse("try { u8 x; }");
        match &ast.stmts[0].kind {
            StmtKind::TryCatch(d) => {
                assert_eq!(d.try_body.len(), 1);
                assert!(d.catch_body.is_none());
            }
            other => panic!("expected TryCatch, got {:?}", other),
        }
    }

    #[test]
    fn test_break_continue() {
        let ast = parse("break; continue;");
        assert!(matches!(&ast.stmts[0].kind, StmtKind::Break));
        assert!(matches!(&ast.stmts[1].kind, StmtKind::Continue));
    }

    #[test]
    fn test_return_value() {
        let ast = parse("return 42;");
        match &ast.stmts[0].kind {
            StmtKind::Return(Some(expr)) => {
                matches!(&expr.kind, ExprKind::IntLiteral(42));
            }
            other => panic!("expected Return, got {:?}", other),
        }
    }

    #[test]
    fn test_return_void() {
        let ast = parse("return;");
        matches!(&ast.stmts[0].kind, StmtKind::Return(None));
    }

    // ========== Import ==========

    #[test]
    fn test_import() {
        let (ast, mut i) = parse_with_interner("import std.mem;");
        match &ast.stmts[0].kind {
            StmtKind::Import { path } => {
                assert_eq!(path, &vec![i.intern("std"), i.intern("mem")]);
            }
            other => panic!("expected Import, got {:?}", other),
        }
    }

    // ========== Expressions ==========

    #[test]
    fn test_expr_binary_add() {
        let ast = parse("x = 1 + 2;");
        // This parses as an expression statement with assignment
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { rhs, .. } => match &rhs.kind {
                    ExprKind::Binary { op: BinOp::Add, .. } => {}
                    other => panic!("expected Binary Add, got {:?}", other),
                },
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_precedence() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let ast = parse("x = 1 + 2 * 3;");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { rhs, .. } => match &rhs.kind {
                    ExprKind::Binary {
                        op: BinOp::Add,
                        rhs: inner_rhs,
                        ..
                    } => {
                        assert!(matches!(
                            &inner_rhs.kind,
                            ExprKind::Binary { op: BinOp::Mul, .. }
                        ));
                    }
                    other => panic!("expected Binary Add, got {:?}", other),
                },
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_unary() {
        let ast = parse("x = -42;");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { rhs, .. } => {
                    assert!(matches!(
                        &rhs.kind,
                        ExprKind::Unary {
                            op: UnaryOp::Neg,
                            ..
                        }
                    ));
                }
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_ternary() {
        let ast = parse("x = a ? b : c;");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { rhs, .. } => {
                    assert!(matches!(&rhs.kind, ExprKind::Ternary { .. }));
                }
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_function_call() {
        let (ast, interner) = parse_with_interner("foo(1, 2, 3);");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Call { func, args } => {
                    match &func.kind {
                        ExprKind::Ident(s) => assert_eq!(interner.resolve(*s), "foo"),
                        other => panic!("expected Ident, got {:?}", other),
                    }
                    assert_eq!(args.len(), 3);
                }
                other => panic!("expected Call, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_array_index() {
        let ast = parse("arr[0];");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => {
                assert!(matches!(&expr.kind, ExprKind::Index { .. }));
            }
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_member_access() {
        let ast = parse("obj.field;");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => {
                assert!(matches!(&expr.kind, ExprKind::MemberAccess { .. }));
            }
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_scoped_ident() {
        let (ast, mut i) = parse_with_interner("std::mem::size();");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Call { func, .. } => match &func.kind {
                    ExprKind::ScopedIdent(path) => {
                        assert_eq!(
                            path,
                            &vec![i.intern("std"), i.intern("mem"), i.intern("size")]
                        );
                    }
                    other => panic!("expected ScopedIdent, got {:?}", other),
                },
                other => panic!("expected Call, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_dollar() {
        let ast = parse("u8 x @ $;");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert!(matches!(&d.offset.as_ref().unwrap().kind, ExprKind::Dollar));
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_sizeof() {
        let ast = parse("x = sizeof(u32);");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { rhs, .. } => {
                    assert!(matches!(&rhs.kind, ExprKind::Sizeof(_)));
                }
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_addressof() {
        let (ast, interner) = parse_with_interner("x = addressof(myVar);");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { rhs, .. } => match &rhs.kind {
                    ExprKind::Addressof(expr) => match &expr.kind {
                        ExprKind::Ident(name) => assert_eq!(interner.resolve(*name), "myVar"),
                        other => panic!("expected Ident, got {:?}", other),
                    },
                    other => panic!("expected Addressof, got {:?}", other),
                },
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_expr_addressof_member_access() {
        let (ast, interner) = parse_with_interner("x = addressof(parent.keyword);");
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { rhs, .. } => match &rhs.kind {
                    ExprKind::Addressof(inner) => match &inner.kind {
                        ExprKind::MemberAccess { expr, member } => {
                            match &expr.kind {
                                ExprKind::Ident(name) => {
                                    assert_eq!(interner.resolve(*name), "parent")
                                }
                                other => panic!("expected Ident, got {:?}", other),
                            }
                            assert_eq!(interner.resolve(*member), "keyword");
                        }
                        other => panic!("expected MemberAccess, got {:?}", other),
                    },
                    other => panic!("expected Addressof, got {:?}", other),
                },
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    // ========== Attributes ==========

    #[test]
    fn test_attribute() {
        let (ast, interner) = parse_with_interner("[[color(\"FF0000\")]] u8 x;");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(d.attrs.len(), 1);
                assert_eq!(interner.resolve(d.attrs[0].name), "color");
                assert_eq!(d.attrs[0].args.len(), 1);
            }
            other => panic!("expected DataPlacement with attrs, got {:?}", other),
        }
    }

    #[test]
    fn test_multiple_attributes() {
        let (ast, interner) = parse_with_interner("[[color(\"FF0000\"), hidden]] u8 x;");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(d.attrs.len(), 2);
                assert_eq!(interner.resolve(d.attrs[0].name), "color");
                assert_eq!(interner.resolve(d.attrs[1].name), "hidden");
            }
            other => panic!("expected DataPlacement with attrs, got {:?}", other),
        }
    }

    #[test]
    fn test_attribute_with_space_in_closing() {
        // 7z.hexpat style: [[color("FF0000")] ]
        let (ast, interner) = parse_with_interner("[[color(\"FF0000\")] ] u8 x;");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(d.attrs.len(), 1);
                assert_eq!(interner.resolve(d.attrs[0].name), "color");
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    // ========== Complex patterns ==========

    #[test]
    fn test_full_struct_with_placement() {
        let ast = parse(
            r#"
            struct Header {
                u32 magic;
                u16 version;
                u16 flags;
            };
            Header header @ 0x00;
        "#,
        );
        assert_eq!(ast.stmts.len(), 2);
        assert!(matches!(&ast.stmts[0].kind, StmtKind::StructDef(_)));
        assert!(matches!(&ast.stmts[1].kind, StmtKind::DataPlacement(_)));
    }

    #[test]
    fn test_named_type_var_decl() {
        let (ast, mut i) = parse_with_interner("Header h @ 0;");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(d.name, i.intern("h"));
                match &d.ty.kind {
                    TypeExprKind::Named(path) => assert_eq!(path, &vec![i.intern("Header")]),
                    other => panic!("expected Named type, got {:?}", other),
                }
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    // ========== Error recovery ==========

    #[test]
    fn test_error_recovery() {
        // Should recover after the missing semicolon
        let errors = parse_err("u32 x u16 y;");
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_while_array_in_struct() {
        // Simulates patterns from ImHex std library
        let (ast, interner) = parse_with_interner("struct Foo { u8 data[while(x < 10)]; };");
        match &ast.stmts[0].kind {
            StmtKind::StructDef(d) => {
                assert_eq!(interner.resolve(d.name), "Foo");
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_while_array_with_template_type() {
        // Pattern: NamespacedType<T> varname[while(cond)]
        let (ast, interner) = parse_with_interner(
            r#"struct Bar<T> {
                T items[while(x != 0)] [[inline]];
            };"#,
        );
        match &ast.stmts[0].kind {
            StmtKind::StructDef(d) => {
                assert_eq!(interner.resolve(d.name), "Bar");
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    #[test]
    fn test_template_type_with_while_array() {
        // Template type with while-sized array (from ImHex std library patterns)
        let (ast, interner) = parse_with_interner("Foo<A> items[while(x != 0)];");
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(interner.resolve(d.name), "items");
                assert!(matches!(
                    &d.ty.kind,
                    TypeExprKind::Array(_, ArraySize::Conditional(_))
                ));
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    #[test]
    fn test_namespaced_template_type_with_while_array() {
        // Pattern from std::mem.pat: ns::Type<A, B> name[while(cond)] [[attr]];
        let (ast, interner) = parse_with_interner(
            r#"struct S {
                Foo::Bar<A, B> items[while(x != 0)] [[inline]];
            };"#,
        );
        match &ast.stmts[0].kind {
            StmtKind::StructDef(d) => {
                assert_eq!(interner.resolve(d.name), "S");
                assert_eq!(d.body.len(), 1);
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    // ========== Comma-separated variable declarations ==========

    #[test]
    fn test_comma_separated_var_decl() {
        let (ast, interner) = parse_with_interner("u8 r, g, b;");
        assert_eq!(ast.stmts.len(), 3);
        for (i, expected_name) in ["r", "g", "b"].iter().enumerate() {
            match &ast.stmts[i].kind {
                StmtKind::DataPlacement(d) => {
                    assert_eq!(interner.resolve(d.name), *expected_name);
                    assert!(matches!(&d.ty.kind, TypeExprKind::Builtin(BuiltinType::U8)));
                }
                other => panic!(
                    "expected DataPlacement for '{}', got {:?}",
                    expected_name, other
                ),
            }
        }
    }

    #[test]
    fn test_comma_separated_var_decl_with_array() {
        let (ast, interner) = parse_with_interner("u8 a[2], b[3];");
        assert_eq!(ast.stmts.len(), 2);
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(interner.resolve(d.name), "a");
                assert!(matches!(
                    &d.ty.kind,
                    TypeExprKind::Array(_, ArraySize::Fixed(_))
                ));
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
        match &ast.stmts[1].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(interner.resolve(d.name), "b");
                assert!(matches!(
                    &d.ty.kind,
                    TypeExprKind::Array(_, ArraySize::Fixed(_))
                ));
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    #[test]
    fn test_comma_separated_var_decl_in_struct() {
        let (ast, interner) = parse_with_interner("struct Foo { u8 r, g, b; };");
        match &ast.stmts[0].kind {
            StmtKind::StructDef(sd) => {
                assert_eq!(sd.body.len(), 3);
                for (i, expected_name) in ["r", "g", "b"].iter().enumerate() {
                    match &sd.body[i].kind {
                        StmtKind::DataPlacement(d) => {
                            assert_eq!(interner.resolve(d.name), *expected_name);
                        }
                        other => panic!("expected DataPlacement, got {:?}", other),
                    }
                }
            }
            other => panic!("expected StructDef, got {:?}", other),
        }
    }

    // ========== Scoped attribute names ==========

    #[test]
    fn test_scoped_attribute_name() {
        let (ast, interner) = parse_with_interner(r#"u8 x [[hex::visualize("image", this)]];"#);
        match &ast.stmts[0].kind {
            StmtKind::DataPlacement(d) => {
                assert_eq!(d.attrs.len(), 1);
                assert_eq!(interner.resolve(d.attrs[0].name), "hex::visualize");
                assert_eq!(d.attrs[0].args.len(), 2);
            }
            other => panic!("expected DataPlacement, got {:?}", other),
        }
    }

    // ========== Member access with parent/this keywords ==========

    #[test]
    fn test_member_access_parent_parent() {
        // parent.parent.length should parse correctly
        let (ast, interner) = parse_with_interner("u32 x = parent.parent.length;");
        match &ast.stmts[0].kind {
            StmtKind::LocalVar { name, init, .. } => {
                assert_eq!(interner.resolve(*name), "x");
                // Should be MemberAccess { MemberAccess { parent, parent }, length }
                match &init.kind {
                    ExprKind::MemberAccess { expr, member } => {
                        assert_eq!(interner.resolve(*member), "length");
                        match &expr.kind {
                            ExprKind::MemberAccess {
                                expr: inner,
                                member: inner_member,
                            } => {
                                assert_eq!(interner.resolve(*inner_member), "parent");
                                // parent keyword is parsed as Ident("parent")
                                match &inner.kind {
                                    ExprKind::Ident(s) => {
                                        assert_eq!(interner.resolve(*s), "parent")
                                    }
                                    other => panic!("expected Ident, got {:?}", other),
                                }
                            }
                            other => panic!("expected inner MemberAccess, got {:?}", other),
                        }
                    }
                    other => panic!("expected MemberAccess, got {:?}", other),
                }
            }
            other => panic!("expected LocalVar, got {:?}", other),
        }
    }

    #[test]
    fn test_member_access_this_parent() {
        // this.parent should parse correctly
        let (ast, interner) = parse_with_interner("u32 x = this.parent;");
        match &ast.stmts[0].kind {
            StmtKind::LocalVar { name, init, .. } => {
                assert_eq!(interner.resolve(*name), "x");
                match &init.kind {
                    ExprKind::MemberAccess { expr, member } => {
                        assert_eq!(interner.resolve(*member), "parent");
                        // this keyword is parsed as Ident("this")
                        match &expr.kind {
                            ExprKind::Ident(s) => assert_eq!(interner.resolve(*s), "this"),
                            other => panic!("expected Ident, got {:?}", other),
                        }
                    }
                    other => panic!("expected MemberAccess, got {:?}", other),
                }
            }
            other => panic!("expected LocalVar, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_parent_parent_length() {
        // Test that parent.parent.length parses correctly (part of png.hexpat text_len())
        let (ast, interner) =
            parse_with_interner("u64 len = parent.parent.length - ($ - addressof(this));");
        assert_eq!(ast.stmts.len(), 1);
        match &ast.stmts[0].kind {
            StmtKind::LocalVar { name, init, .. } => {
                assert_eq!(interner.resolve(*name), "len");
                // Just verify it parses, the structure of parent.parent.length is tested above
                let _ = init;
            }
            other => panic!("expected LocalVar, got {:?}", other),
        }
    }

    // ========== Assignment expression tests ==========

    #[test]
    fn test_chained_assignment() {
        // a = b = c should parse as a = (b = c) (right-associative)
        let (ast, interner) = parse_with_interner("a = b = 42;");
        assert_eq!(ast.stmts.len(), 1);
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { op, lhs, rhs } => {
                    assert_eq!(*op, AssignOp::Assign);
                    match &lhs.kind {
                        ExprKind::Ident(s) => assert_eq!(interner.resolve(*s), "a"),
                        other => panic!("expected Ident, got {:?}", other),
                    }
                    match &rhs.kind {
                        ExprKind::Assign {
                            op: inner_op,
                            lhs: inner_lhs,
                            rhs: inner_rhs,
                        } => {
                            assert_eq!(*inner_op, AssignOp::Assign);
                            match &inner_lhs.kind {
                                ExprKind::Ident(s) => assert_eq!(interner.resolve(*s), "b"),
                                other => panic!("expected Ident, got {:?}", other),
                            }
                            assert!(matches!(&inner_rhs.kind, ExprKind::IntLiteral(42)));
                        }
                        other => panic!("expected inner Assign, got {:?}", other),
                    }
                }
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_compound_assignment_in_expr() {
        let (ast, interner) = parse_with_interner("x += 1;");
        assert_eq!(ast.stmts.len(), 1);
        match &ast.stmts[0].kind {
            StmtKind::ExprStmt(expr) => match &expr.kind {
                ExprKind::Assign { op, lhs, rhs } => {
                    assert_eq!(*op, AssignOp::AddAssign);
                    match &lhs.kind {
                        ExprKind::Ident(s) => assert_eq!(interner.resolve(*s), "x"),
                        other => panic!("expected Ident, got {:?}", other),
                    }
                    assert!(matches!(&rhs.kind, ExprKind::IntLiteral(1)));
                }
                other => panic!("expected Assign, got {:?}", other),
            },
            other => panic!("expected ExprStmt, got {:?}", other),
        }
    }

    #[test]
    fn test_for_loop_multi_init() {
        // for (u8 a, b; ...) should produce 2 init stmts
        let (ast, interner) = parse_with_interner("for (u8 a, b; a < 10; a += 1) {}");
        assert_eq!(ast.stmts.len(), 1);
        match &ast.stmts[0].kind {
            StmtKind::For(fd) => {
                assert_eq!(fd.init.len(), 2);
                match &fd.init[0].kind {
                    StmtKind::DataPlacement(d) => {
                        assert_eq!(interner.resolve(d.name), "a")
                    }
                    other => panic!("expected DataPlacement, got {:?}", other),
                }
                match &fd.init[1].kind {
                    StmtKind::DataPlacement(d) => {
                        assert_eq!(interner.resolve(d.name), "b")
                    }
                    other => panic!("expected DataPlacement, got {:?}", other),
                }
                // Step should be parsed as assignment expression
                match &fd.step.kind {
                    StmtKind::ExprStmt(expr) => match &expr.kind {
                        ExprKind::Assign {
                            op: AssignOp::AddAssign,
                            ..
                        } => {}
                        other => panic!("expected AddAssign, got {:?}", other),
                    },
                    other => panic!("expected ExprStmt, got {:?}", other),
                }
            }
            other => panic!("expected For, got {:?}", other),
        }
    }
}
