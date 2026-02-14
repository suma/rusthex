// AST node definitions for the pattern language

use crate::name::Name;
use crate::span::Span;

/// Top-level AST: a list of statements
#[derive(Debug, Clone)]
pub struct Ast {
    pub stmts: Vec<Stmt>,
}

/// A statement with source span
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

// --- Boxed data structs for large StmtKind variants ---

#[derive(Debug, Clone)]
pub struct DataPlacementData {
    pub ty: TypeExpr,
    pub name: Name,
    pub offset: Option<Expr>,
    pub attrs: Vec<Attribute>,
    pub pointer_size: Option<TypeExpr>,
    pub section: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructDefData {
    pub name: Name,
    pub parent: Option<(Name, Vec<TemplateArg>)>,
    pub template_params: Vec<TemplateParam>,
    pub body: Vec<Stmt>,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct UnionDefData {
    pub name: Name,
    pub template_params: Vec<TemplateParam>,
    pub body: Vec<Stmt>,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct EnumDefData {
    pub name: Name,
    pub underlying: TypeExpr,
    pub members: Vec<EnumMember>,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct BitfieldDefData {
    pub name: Name,
    pub template_params: Vec<TemplateParam>,
    pub body: Vec<Stmt>,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct FnDefData {
    pub name: Name,
    pub params: Vec<Param>,
    pub return_ty: Option<TypeExpr>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct TypeAliasData {
    pub name: Name,
    pub template_params: Vec<TemplateParam>,
    pub ty: Option<TypeExpr>,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct NamespaceData {
    pub name: Name,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct IfData {
    pub cond: Expr,
    pub then_body: Vec<Stmt>,
    pub else_body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct MatchData {
    pub exprs: Vec<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub struct WhileData {
    pub cond: Expr,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ForData {
    pub init: Vec<Stmt>,
    pub cond: Expr,
    pub step: Box<Stmt>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct TryCatchData {
    pub try_body: Vec<Stmt>,
    pub catch_body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct AnonymousPlacementData {
    pub ty: TypeExpr,
    pub placement: Option<Expr>,
    pub attrs: Vec<Attribute>,
    pub section: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// Local variable with initializer: `Type name = expr;`
    LocalVar {
        ty: TypeExpr,
        name: Name,
        init: Expr,
    },

    /// Data placement: read type from binary at offset (auto-advance or explicit).
    DataPlacement(Box<DataPlacementData>),

    /// Struct definition
    StructDef(Box<StructDefData>),

    /// Union definition
    UnionDef(Box<UnionDefData>),

    /// Enum definition
    EnumDef(Box<EnumDefData>),

    /// Bitfield definition
    BitfieldDef(Box<BitfieldDefData>),

    /// Function definition
    FnDef(Box<FnDefData>),

    /// Type alias: `using Name = Type;` or forward declaration: `using Name;`
    TypeAlias(Box<TypeAliasData>),

    /// Namespace
    Namespace(Box<NamespaceData>),

    /// If statement
    If(Box<IfData>),

    /// Match statement (supports multiple exprs for tuple matching)
    Match(Box<MatchData>),

    /// While loop
    While(Box<WhileData>),

    /// For loop
    For(Box<ForData>),

    /// Try-catch (catch is optional)
    TryCatch(Box<TryCatchData>),

    Break,
    Continue,
    Return(Option<Expr>),

    /// Bitfield field as a statement (used inside if/while within bitfield bodies)
    BitfieldFieldStmt {
        name: Name,
        width: Expr,
        attrs: Vec<Attribute>,
    },

    /// Anonymous type placement: `Type;` or `Type @ offset;`
    AnonymousPlacement(Box<AnonymousPlacementData>),

    /// Expression statement
    ExprStmt(Expr),

    /// Import: `import std.mem;`
    Import {
        path: Vec<Name>,
    },
}

/// Expression with source span
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    IntLiteral(u128),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    Null,

    /// Simple identifier
    Ident(Name),
    /// Scoped identifier: `Foo::Bar::Baz`
    ScopedIdent(Vec<Name>),

    /// Current offset: `$`
    Dollar,

    /// Binary operation
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    /// Unary operation
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    /// Function call
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    /// Array index: `expr[index]`
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },

    /// Member access: `expr.member`
    MemberAccess {
        expr: Box<Expr>,
        member: Name,
    },

    /// sizeof expression
    Sizeof(Box<TypeExprOrExpr>),

    /// addressof expression
    Addressof(Box<Expr>),

    /// Type cast: `type(expr)` or C-style cast
    Cast {
        ty: Box<TypeExpr>,
        expr: Box<Expr>,
    },

    /// Ternary: `cond ? then : else`
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },

    /// Assignment: `lhs = rhs`
    Assign {
        op: AssignOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    /// Initializer list: `{ expr, expr, ... }`
    InitializerList(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    LogAnd,
    LogOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}

/// Type expression
#[derive(Debug, Clone)]
pub struct TypeExpr {
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeExprKind {
    /// Built-in type (u8, u16, u32, u64, u128, s8, s16, s32, s64, s128, float, double, char, char16, bool, str, auto)
    Builtin(BuiltinType),

    /// Named type (possibly with namespace path)
    Named(Vec<Name>),

    /// Endian-prefixed type: `le Type` or `be Type`
    Endian(Endianness, Box<TypeExpr>),

    /// Array type: `Type[size]` or `Type[while(cond)]`
    Array(Box<TypeExpr>, ArraySize),

    /// Pointer type: `Type *pointerBase : pointerSize`
    Pointer(Box<TypeExpr>, Box<TypeExpr>),

    /// Template instantiation: `Name<T1, T2>`
    Template(Vec<Name>, Vec<TemplateArg>),

    /// Arbitrary-width integer: u24, s48, etc.
    ArbitraryInt { signed: bool, bits: u64 },

    /// Auto type
    Auto,

    /// Unsized type marker (for padding etc)
    Padding,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    U8,
    U16,
    U32,
    U64,
    U128,
    S8,
    S16,
    S32,
    S64,
    S128,
    Float,
    Double,
    Char,
    Char16,
    Bool,
    Str,
}

impl BuiltinType {
    pub fn from_str(s: &str) -> Option<BuiltinType> {
        match s {
            "u8" => Some(BuiltinType::U8),
            "u16" => Some(BuiltinType::U16),
            "u32" => Some(BuiltinType::U32),
            "u64" => Some(BuiltinType::U64),
            "u128" => Some(BuiltinType::U128),
            "s8" => Some(BuiltinType::S8),
            "s16" => Some(BuiltinType::S16),
            "s32" => Some(BuiltinType::S32),
            "s64" => Some(BuiltinType::S64),
            "s128" => Some(BuiltinType::S128),
            "float" => Some(BuiltinType::Float),
            "double" => Some(BuiltinType::Double),
            "char" => Some(BuiltinType::Char),
            "char16" => Some(BuiltinType::Char16),
            "bool" => Some(BuiltinType::Bool),
            "str" => Some(BuiltinType::Str),
            _ => None,
        }
    }

    pub fn size(&self) -> Option<u64> {
        match self {
            BuiltinType::U8 | BuiltinType::S8 | BuiltinType::Char | BuiltinType::Bool => Some(1),
            BuiltinType::U16 | BuiltinType::S16 | BuiltinType::Char16 => Some(2),
            BuiltinType::U32 | BuiltinType::S32 | BuiltinType::Float => Some(4),
            BuiltinType::U64 | BuiltinType::S64 | BuiltinType::Double => Some(8),
            BuiltinType::U128 | BuiltinType::S128 => Some(16),
            BuiltinType::Str => None, // variable size
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endianness {
    Little,
    Big,
}

#[derive(Debug, Clone)]
pub enum ArraySize {
    /// Fixed size: `Type[N]`
    Fixed(Box<Expr>),
    /// Conditional (while loop): `Type[while(cond)]`
    Conditional(Box<Expr>),
    /// Null-terminated / read-to-end: `Type[]`
    NullTerminated,
}

#[derive(Debug, Clone)]
pub enum TemplateArg {
    Type(TypeExpr),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct TemplateParam {
    pub name: Name,
    pub is_auto: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub ty: TypeExpr,
    pub name: Name,
    pub direction: ParamDirection,
    pub is_variadic: bool,
    pub default: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamDirection {
    In,
    Out,
    Ref,
}

#[derive(Debug, Clone)]
pub struct EnumMember {
    pub name: Name,
    pub value: Option<Expr>,
    pub end_value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub patterns: Vec<MatchPattern>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    /// Single value
    Value(Expr),
    /// Range: `a ... b`
    Range(Expr, Expr),
    /// Wildcard: `_`
    Wildcard,
    /// Tuple: `(a, b, ...)` for multi-expression match
    Tuple(Vec<MatchPattern>),
}

/// Attribute: `[[name("value", args...)]]`
#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Name,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Used for sizeof which can take either a type or an expression
#[derive(Debug, Clone)]
pub enum TypeExprOrExpr {
    Type(TypeExpr),
    Expr(Expr),
}
