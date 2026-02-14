// ImHex pattern language parser and evaluator library

pub mod error;
pub mod eval;
pub mod lexer;
pub mod name;
pub mod parser;
pub mod preprocessor;
pub mod span;
pub mod stdlib;

pub use name::{Name, StringInterner};

// Re-export key types for public API
pub use error::Error;
pub use eval::data_source::{DataSource, SliceDataSource};
pub use eval::pattern::{PatternAttributes, PatternNode, PatternValue};
pub use eval::value::Value;
pub use eval::{EvalStats, Evaluator};
pub use parser::ast::Ast;

use error::{EvalError, ParseError};
use lexer::Lexer;
use parser::ast::Endianness;
use parser::Parser;
use preprocessor::{Endian, IncludeResolver, NoopResolver, Pragmas, Preprocessor};
use span::SourceId;
use std::sync::atomic::{AtomicBool, AtomicU64};
use std::sync::Arc;

/// Options for controlling evaluation resource usage.
#[derive(Clone, Default)]
pub struct EvalOptions {
    /// Maximum total PatternNode count (0 = use default).
    /// At ~512 bytes per node, 500K nodes ≈ 256 MB.
    pub max_total_nodes: u64,
    /// Cancellation token — evaluation stops when set to true.
    pub cancellation_token: Option<Arc<AtomicBool>>,
    /// Shared stmt counter — readable from outside during evaluation.
    pub shared_stmt_count: Option<Arc<AtomicU64>>,
    /// Shared read_type counter — readable from outside during evaluation.
    pub shared_read_type_count: Option<Arc<AtomicU64>>,
}

/// Main entry point for the pattern language engine.
/// Provides parse, evaluate, and run methods.
pub struct PatternEngine {
    resolver: Box<dyn IncludeResolver>,
}

impl PatternEngine {
    /// Create a new pattern engine with no include resolver
    pub fn new() -> Self {
        Self {
            resolver: Box::new(NoopResolver),
        }
    }

    /// Create a new pattern engine with a custom include resolver
    pub fn with_resolver(resolver: impl IncludeResolver + 'static) -> Self {
        Self {
            resolver: Box::new(resolver),
        }
    }

    /// Parse source code into an AST
    pub fn parse(&self, source: &str) -> Result<Ast, Vec<ParseError>> {
        // Preprocess
        let mut pp = Preprocessor::new(self.resolver.as_ref());
        let processed = pp.process(source).map_err(|errors| {
            errors
                .into_iter()
                .map(|e| ParseError::new(e.message, span::Span::dummy()))
                .collect::<Vec<_>>()
        })?;

        // Lex
        let mut interner = StringInterner::new();
        let lexer = Lexer::new(&processed.source, SourceId(0), &mut interner);
        let (tokens, lex_errors) = lexer.tokenize();
        if !lex_errors.is_empty() {
            return Err(lex_errors
                .into_iter()
                .map(|e| ParseError::new(e.message, e.span))
                .collect());
        }

        // Parse
        Parser::new(tokens, &mut interner).parse()
    }

    /// Evaluate an AST against binary data
    pub fn evaluate(
        &self,
        ast: &Ast,
        data: &dyn DataSource,
    ) -> Result<Vec<PatternNode>, EvalError> {
        let mut evaluator = Evaluator::new(data);
        stdlib::register_all(&mut evaluator.scope, &mut evaluator.interner, data.size());
        evaluator.evaluate(ast)
    }

    /// Parse and evaluate in one step
    pub fn run(&self, source: &str, data: &dyn DataSource) -> Result<Vec<PatternNode>, Error> {
        self.run_with_options(source, data, EvalOptions::default())
            .map(|(nodes, _stats)| nodes)
    }

    /// Parse and evaluate with resource limit options.
    /// Returns pattern nodes and evaluation statistics.
    pub fn run_with_options(
        &self,
        source: &str,
        data: &dyn DataSource,
        options: EvalOptions,
    ) -> Result<(Vec<PatternNode>, EvalStats), Error> {
        // Preprocess to get both source and pragmas
        let mut pp = Preprocessor::new(self.resolver.as_ref());
        let processed = pp.process(source).map_err(|errors| {
            Error::Parse(
                errors
                    .into_iter()
                    .map(|e| ParseError::new(e.message, span::Span::dummy()))
                    .collect(),
            )
        })?;

        // Lex
        let mut interner = StringInterner::new();
        let lexer = Lexer::new(&processed.source, SourceId(0), &mut interner);
        let (tokens, lex_errors) = lexer.tokenize();
        if !lex_errors.is_empty() {
            return Err(Error::Parse(
                lex_errors
                    .into_iter()
                    .map(|e| ParseError::new(e.message, e.span))
                    .collect(),
            ));
        }

        // Parse
        let ast = Parser::new(tokens, &mut interner)
            .parse()
            .map_err(Error::Parse)?;

        // Evaluate with pragmas applied
        self.evaluate_with_pragmas(
            &ast,
            data,
            &processed.pragmas,
            &processed.source,
            interner,
            options,
        )
        .map_err(Error::Eval)
    }

    /// Evaluate an AST with pragma settings applied
    fn evaluate_with_pragmas(
        &self,
        ast: &Ast,
        data: &dyn DataSource,
        pragmas: &Pragmas,
        source: &str,
        interner: StringInterner,
        options: EvalOptions,
    ) -> Result<(Vec<PatternNode>, EvalStats), EvalError> {
        let mut evaluator = Evaluator::with_interner(data, interner);
        evaluator.set_source(source.to_string());
        stdlib::register_all(&mut evaluator.scope, &mut evaluator.interner, data.size());

        // Apply pragma limits
        evaluator.set_limits(
            pragmas.array_limit,
            pragmas.pattern_limit,
            pragmas.recursion_depth,
        );

        // Apply pragma endianness
        if let Some(endian) = pragmas.endian {
            evaluator.set_default_endian(match endian {
                Endian::Big => Endianness::Big,
                Endian::Little => Endianness::Little,
            });
        }

        // Apply eval options
        if options.max_total_nodes > 0 {
            evaluator.set_max_total_nodes(options.max_total_nodes);
        }

        // Ensure total node limit respects #pragma pattern_limit
        evaluator.ensure_min_total_nodes(pragmas.pattern_limit);
        if let Some(token) = options.cancellation_token {
            evaluator.set_cancellation_token(token);
        }
        if let Some(counter) = options.shared_stmt_count {
            evaluator.set_shared_stmt_count(counter);
        }
        if let Some(counter) = options.shared_read_type_count {
            evaluator.set_shared_read_type_count(counter);
        }

        let results = evaluator.evaluate(ast)?;
        let stats = evaluator.stats();
        Ok((results, stats))
    }
}

impl Default for PatternEngine {
    fn default() -> Self {
        Self::new()
    }
}

// Need to make scope public on Evaluator for stdlib registration
// This is done via the existing pub field
