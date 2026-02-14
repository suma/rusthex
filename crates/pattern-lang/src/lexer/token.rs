// Token definitions for the pattern language

use crate::name::Name;
use crate::span::Span;

/// A single token produced by the lexer
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// All possible token types in the pattern language
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    IntLiteral(u128),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),

    // Identifier
    Ident(Name),

    // Keywords - types
    KwStruct,
    KwUnion,
    KwEnum,
    KwBitfield,
    KwFn,

    // Keywords - control flow
    KwIf,
    KwElse,
    KwMatch,
    KwWhile,
    KwFor,
    KwBreak,
    KwContinue,
    KwReturn,
    KwTry,
    KwCatch,

    // Keywords - parameter modifiers
    KwIn,
    KwOut,
    KwRef,

    // Keywords - namespace / using
    KwNamespace,
    KwUsing,

    // Keywords - endianness
    KwLe,
    KwBe,

    // Keywords - misc
    KwConst,
    KwTrue,
    KwFalse,
    KwNull,
    KwSizeof,
    KwAddressof,
    KwAuto,
    KwImport,
    KwAs,

    // Arithmetic operators
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Bitwise operators
    Ampersand, // &
    Pipe,      // |
    Caret,     // ^
    Tilde,     // ~
    LShift,    // <<
    RShift,    // >>

    // Logical operators
    AmpAmp,   // &&
    PipePipe, // ||
    Bang,     // !

    // Comparison operators
    EqEq,      // ==
    BangEq,    // !=
    Less,      // <
    Greater,   // >
    LessEq,    // <=
    GreaterEq, // >=

    // Assignment operators
    Eq,        // =
    PlusEq,    // +=
    MinusEq,   // -=
    StarEq,    // *=
    SlashEq,   // /=
    PercentEq, // %=
    AmpEq,     // &=
    PipeEq,    // |=
    CaretEq,   // ^=
    LShiftEq,  // <<=
    RShiftEq,  // >>=

    // Special symbols
    Dollar,     // $
    At,         // @
    ColonColon, // ::
    DotDotDot,  // ...
    Dot,        // .
    Arrow,      // ->
    Comma,      // ,
    Semicolon,  // ;
    Colon,      // :
    Question,   // ?

    // Brackets
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]
    LBrace,   // {
    RBrace,   // }

    // Preprocessor
    Hash, // #

    // End of file
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::IntLiteral(v) => write!(f, "integer '{}'", v),
            TokenKind::FloatLiteral(v) => write!(f, "float '{}'", v),
            TokenKind::StringLiteral(v) => write!(f, "string \"{}\"", v),
            TokenKind::CharLiteral(v) => write!(f, "char '{}'", v),
            TokenKind::BoolLiteral(v) => write!(f, "'{}'", v),
            TokenKind::Ident(name) => write!(f, "'{}'", name),
            TokenKind::KwStruct => write!(f, "'struct'"),
            TokenKind::KwUnion => write!(f, "'union'"),
            TokenKind::KwEnum => write!(f, "'enum'"),
            TokenKind::KwBitfield => write!(f, "'bitfield'"),
            TokenKind::KwFn => write!(f, "'fn'"),
            TokenKind::KwIf => write!(f, "'if'"),
            TokenKind::KwElse => write!(f, "'else'"),
            TokenKind::KwMatch => write!(f, "'match'"),
            TokenKind::KwWhile => write!(f, "'while'"),
            TokenKind::KwFor => write!(f, "'for'"),
            TokenKind::KwBreak => write!(f, "'break'"),
            TokenKind::KwContinue => write!(f, "'continue'"),
            TokenKind::KwReturn => write!(f, "'return'"),
            TokenKind::KwTry => write!(f, "'try'"),
            TokenKind::KwCatch => write!(f, "'catch'"),
            TokenKind::KwIn => write!(f, "'in'"),
            TokenKind::KwOut => write!(f, "'out'"),
            TokenKind::KwRef => write!(f, "'ref'"),
            TokenKind::KwNamespace => write!(f, "'namespace'"),
            TokenKind::KwUsing => write!(f, "'using'"),
            TokenKind::KwLe => write!(f, "'le'"),
            TokenKind::KwBe => write!(f, "'be'"),
            TokenKind::KwConst => write!(f, "'const'"),
            TokenKind::KwTrue => write!(f, "'true'"),
            TokenKind::KwFalse => write!(f, "'false'"),
            TokenKind::KwNull => write!(f, "'null'"),
            TokenKind::KwSizeof => write!(f, "'sizeof'"),
            TokenKind::KwAddressof => write!(f, "'addressof'"),
            TokenKind::KwAuto => write!(f, "'auto'"),
            TokenKind::KwImport => write!(f, "'import'"),
            TokenKind::KwAs => write!(f, "'as'"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::Ampersand => write!(f, "'&'"),
            TokenKind::Pipe => write!(f, "'|'"),
            TokenKind::Caret => write!(f, "'^'"),
            TokenKind::Tilde => write!(f, "'~'"),
            TokenKind::LShift => write!(f, "'<<'"),
            TokenKind::RShift => write!(f, "'>>'"),
            TokenKind::AmpAmp => write!(f, "'&&'"),
            TokenKind::PipePipe => write!(f, "'||'"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::EqEq => write!(f, "'=='"),
            TokenKind::BangEq => write!(f, "'!='"),
            TokenKind::Less => write!(f, "'<'"),
            TokenKind::Greater => write!(f, "'>'"),
            TokenKind::LessEq => write!(f, "'<='"),
            TokenKind::GreaterEq => write!(f, "'>='"),
            TokenKind::Eq => write!(f, "'='"),
            TokenKind::PlusEq => write!(f, "'+='"),
            TokenKind::MinusEq => write!(f, "'-='"),
            TokenKind::StarEq => write!(f, "'*='"),
            TokenKind::SlashEq => write!(f, "'/='"),
            TokenKind::PercentEq => write!(f, "'%='"),
            TokenKind::AmpEq => write!(f, "'&='"),
            TokenKind::PipeEq => write!(f, "'|='"),
            TokenKind::CaretEq => write!(f, "'^='"),
            TokenKind::LShiftEq => write!(f, "'<<='"),
            TokenKind::RShiftEq => write!(f, "'>>='"),
            TokenKind::Dollar => write!(f, "'$'"),
            TokenKind::At => write!(f, "'@'"),
            TokenKind::ColonColon => write!(f, "'::'"),
            TokenKind::DotDotDot => write!(f, "'...'"),
            TokenKind::Dot => write!(f, "'.'"),
            TokenKind::Arrow => write!(f, "'->'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Semicolon => write!(f, "';'"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Question => write!(f, "'?'"),
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBracket => write!(f, "'['"),
            TokenKind::RBracket => write!(f, "']'"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::Hash => write!(f, "'#'"),
            TokenKind::Eof => write!(f, "end of file"),
        }
    }
}

impl TokenKind {
    /// Check if this token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::KwStruct
                | TokenKind::KwUnion
                | TokenKind::KwEnum
                | TokenKind::KwBitfield
                | TokenKind::KwFn
                | TokenKind::KwIf
                | TokenKind::KwElse
                | TokenKind::KwMatch
                | TokenKind::KwWhile
                | TokenKind::KwFor
                | TokenKind::KwBreak
                | TokenKind::KwContinue
                | TokenKind::KwReturn
                | TokenKind::KwTry
                | TokenKind::KwCatch
                | TokenKind::KwIn
                | TokenKind::KwOut
                | TokenKind::KwRef
                | TokenKind::KwNamespace
                | TokenKind::KwUsing
                | TokenKind::KwLe
                | TokenKind::KwBe
                | TokenKind::KwConst
                | TokenKind::KwTrue
                | TokenKind::KwFalse
                | TokenKind::KwNull
                | TokenKind::KwSizeof
                | TokenKind::KwAddressof
                | TokenKind::KwAuto
                | TokenKind::KwImport
                | TokenKind::KwAs
        )
    }

    /// Get the keyword for an identifier string, if any
    pub fn keyword_from_str(s: &str) -> Option<TokenKind> {
        match s {
            "struct" => Some(TokenKind::KwStruct),
            "union" => Some(TokenKind::KwUnion),
            "enum" => Some(TokenKind::KwEnum),
            "bitfield" => Some(TokenKind::KwBitfield),
            "fn" => Some(TokenKind::KwFn),
            "if" => Some(TokenKind::KwIf),
            "else" => Some(TokenKind::KwElse),
            "match" => Some(TokenKind::KwMatch),
            "while" => Some(TokenKind::KwWhile),
            "for" => Some(TokenKind::KwFor),
            "break" => Some(TokenKind::KwBreak),
            "continue" => Some(TokenKind::KwContinue),
            "return" => Some(TokenKind::KwReturn),
            "try" => Some(TokenKind::KwTry),
            "catch" => Some(TokenKind::KwCatch),
            "in" => Some(TokenKind::KwIn),
            "out" => Some(TokenKind::KwOut),
            "ref" => Some(TokenKind::KwRef),
            "namespace" => Some(TokenKind::KwNamespace),
            "using" => Some(TokenKind::KwUsing),
            "le" => Some(TokenKind::KwLe),
            "be" => Some(TokenKind::KwBe),
            "const" => Some(TokenKind::KwConst),
            "true" => Some(TokenKind::KwTrue),
            "false" => Some(TokenKind::KwFalse),
            "null" => Some(TokenKind::KwNull),
            "sizeof" => Some(TokenKind::KwSizeof),
            "addressof" => Some(TokenKind::KwAddressof),
            "auto" => Some(TokenKind::KwAuto),
            "import" => Some(TokenKind::KwImport),
            "as" => Some(TokenKind::KwAs),
            _ => None,
        }
    }
}
