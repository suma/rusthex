# pattern-lang

A parser and evaluator library for the [ImHex](https://imhex.werwolv.net/) pattern language.

## Usage

```rust
use pattern_lang::{PatternEngine, SliceDataSource, PatternValue};

let source = r#"
    struct Header {
        u32 magic;
        u16 version;
        u16 flags;
    };
    be Header header @ 0x00;
"#;

let data: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x00, 0x01, 0x00, 0x03];
let engine = PatternEngine::new();
let ds = SliceDataSource::new(data);
let results = engine.run(source, &ds).unwrap();

assert_eq!(results[0].children[0].value, PatternValue::Unsigned(0x89504E47));
assert_eq!(results[0].children[1].value, PatternValue::Unsigned(1));
```

You can also call `parse()` and `evaluate()` separately:

```rust
let ast = engine.parse(source).unwrap();
let results = engine.evaluate(&ast, &ds).unwrap();
```

## Custom DataSource

Implement the `DataSource` trait to apply patterns against any data source:

```rust
use pattern_lang::{DataSource, PatternEngine};
use pattern_lang::error::EvalError;

struct MyFileSource { /* ... */ }

impl DataSource for MyFileSource {
    fn read_bytes(&self, offset: u64, size: u64) -> Result<Vec<u8>, EvalError> {
        // Read from file, memory map, etc.
        todo!()
    }

    fn size(&self) -> u64 {
        todo!()
    }
}
```

## Module Structure

```
src/
├── lib.rs              # Public API (PatternEngine)
├── span.rs             # Source location info
├── error.rs            # Error types
├── lexer/              # Hand-written lexer
├── preprocessor/       # #include, #define, #pragma
├── parser/             # Recursive descent parser with Pratt expression parsing
├── eval/               # Tree-walking evaluator
│   ├── data_source.rs  # DataSource trait
│   ├── value.rs        # Runtime values
│   ├── scope.rs        # Scope chain
│   └── pattern.rs      # PatternNode output tree
└── stdlib/             # Standard library (7 modules)
```

## Testing

```bash
# Run all tests (unit + integration)
cargo test -p pattern-lang

# Run a specific test by name
cargo test -p pattern-lang test_endian_cast

# Run tests with stdout visible (useful for debugging)
cargo test -p pattern-lang -- --nocapture

# Run only unit tests in the eval module
cargo test -p pattern-lang eval::tests

# Run only parser tests
cargo test -p pattern-lang parser::tests

# Run only integration tests
cargo test -p pattern-lang --test integration
```

### Test Structure

| Location | Description |
|----------|-------------|
| `src/eval/mod.rs` | Evaluator unit tests — type reading, struct layout, sizeof, templates, etc. |
| `src/parser/mod.rs` | Parser unit tests — AST construction for each syntax element |
| `src/lexer/mod.rs` | Lexer unit tests — tokenization of literals, operators, keywords |
| `src/preprocessor/mod.rs` | Preprocessor unit tests — `#define`, `#include`, `#pragma` |
| `src/eval/value.rs` | Value conversion tests |
| `src/eval/scope.rs` | Scope chain tests |
| `src/stdlib/` | Standard library function tests (math, string, bit, mem, etc.) |
| `tests/integration.rs` | End-to-end tests via `PatternEngine` public API |
| `tests/hexpat_files.rs` | Parsing validation against `.hexpat` pattern files |

