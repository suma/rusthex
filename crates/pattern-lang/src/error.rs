// Error types for the pattern language

use crate::span::Span;
use std::fmt;

/// Severity level for diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

/// A diagnostic message with source location
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub notes: Vec<(String, Span)>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
            notes: Vec::new(),
        }
    }

    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
            notes: Vec::new(),
        }
    }

    pub fn with_note(mut self, message: impl Into<String>, span: Span) -> Self {
        self.notes.push((message.into(), span));
        self
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
        };
        write!(f, "{}: {}", prefix, self.message)
    }
}

/// Lexer error
#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl LexError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lex error: {}", self.message)
    }
}

impl std::error::Error for LexError {}

/// Parse error
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub expected: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            expected: None,
        }
    }

    pub fn expected(message: impl Into<String>, expected: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            expected: Some(expected.into()),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error: {}", self.message)?;
        if let Some(ref expected) = self.expected {
            write!(f, " (expected {})", expected)?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

/// Evaluation error
#[derive(Debug, Clone)]
pub struct EvalError {
    pub message: String,
    pub span: Option<Span>,
    pub line: Option<u32>,
    pub col: Option<u32>,
    /// True when the error is caused by reading past end of data.
    /// Used to gracefully terminate loops instead of hard-failing.
    pub is_read_oob: bool,
}

impl EvalError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
            line: None,
            col: None,
            is_read_oob: false,
        }
    }

    pub fn with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
            line: None,
            col: None,
            is_read_oob: false,
        }
    }

    pub fn with_location(message: impl Into<String>, span: Span, line: u32, col: u32) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
            line: Some(line),
            col: Some(col),
            is_read_oob: false,
        }
    }

    /// Create a read-out-of-bounds error
    pub fn read_oob(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
            line: None,
            col: None,
            is_read_oob: true,
        }
    }

    /// Set span if not already present
    pub fn with_span_if_none(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }
        self
    }

    /// Compute line/col from source text (if span is set but line/col are not)
    pub fn resolve_location(mut self, source: &str) -> Self {
        if let (Some(span), None) = (&self.span, &self.line) {
            let (line, col) = crate::span::offset_to_line_col(source, span.start);
            self.line = Some(line);
            self.col = Some(col);
        }
        self
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.line, self.col) {
            (Some(line), Some(col)) => {
                write!(f, "eval error at {}:{}: {}", line, col, self.message)
            }
            _ => write!(f, "eval error: {}", self.message),
        }
    }
}

impl std::error::Error for EvalError {}

/// Top-level error type
#[derive(Debug)]
pub enum Error {
    Lex(Vec<LexError>),
    Parse(Vec<ParseError>),
    Eval(EvalError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lex(errors) => {
                for e in errors {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
            Error::Parse(errors) => {
                for e in errors {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
            Error::Eval(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for Error {}
