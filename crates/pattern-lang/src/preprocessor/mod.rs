// Preprocessor for #include, #define, #ifdef, #pragma directives

use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::HashMap;

/// Result of preprocessing source code
#[derive(Debug, Clone)]
pub struct PreprocessResult {
    /// Processed source with directives expanded
    pub source: String,
    /// Pragma settings collected during preprocessing
    pub pragmas: Pragmas,
}

/// Pragma settings that affect evaluation behavior
#[derive(Debug, Clone)]
pub struct Pragmas {
    pub endian: Option<Endian>,
    pub bitfield_bit_order: Option<BitOrder>,
    pub array_limit: u64,
    pub pattern_limit: u64,
    pub recursion_depth: u32,
    pub once: bool,
    /// Metadata from unknown pragmas (e.g. author, description, MIME, magic)
    pub metadata: HashMap<String, String>,
}

impl Default for Pragmas {
    fn default() -> Self {
        Self {
            endian: None,
            bitfield_bit_order: None,
            array_limit: 0x10000,
            pattern_limit: 0x40000,
            recursion_depth: 256,
            once: false,
            metadata: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endian {
    Little,
    Big,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitOrder {
    LeftToRight,
    RightToLeft,
}

/// Trait for resolving #include paths
pub trait IncludeResolver {
    fn resolve(&self, path: &str, is_system: bool) -> Option<String>;
}

/// Default resolver that always fails (no includes available)
pub struct NoopResolver;

impl IncludeResolver for NoopResolver {
    fn resolve(&self, _path: &str, _is_system: bool) -> Option<String> {
        None
    }
}

/// Map-based resolver for testing
pub struct MapResolver {
    files: FxHashMap<String, String>,
}

impl Default for MapResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl MapResolver {
    pub fn new() -> Self {
        Self {
            files: FxHashMap::default(),
        }
    }

    pub fn add(&mut self, path: impl Into<String>, content: impl Into<String>) {
        self.files.insert(path.into(), content.into());
    }
}

impl IncludeResolver for MapResolver {
    fn resolve(&self, path: &str, _is_system: bool) -> Option<String> {
        self.files.get(path).cloned()
    }
}

/// Preprocessor error
#[derive(Debug, Clone)]
pub struct PreprocessError {
    pub message: String,
    pub line: usize,
}

impl std::fmt::Display for PreprocessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for PreprocessError {}

/// Preprocessor that handles directives before lexing/parsing
pub struct Preprocessor<'a> {
    defines: FxHashMap<String, String>,
    included: FxHashSet<String>,
    resolver: &'a dyn IncludeResolver,
    pragmas: Pragmas,
    errors: Vec<PreprocessError>,
}

impl<'a> Preprocessor<'a> {
    pub fn new(resolver: &'a dyn IncludeResolver) -> Self {
        Self {
            defines: FxHashMap::default(),
            included: FxHashSet::default(),
            resolver,
            pragmas: Pragmas::default(),
            errors: Vec::new(),
        }
    }

    /// Add a predefined macro
    pub fn define(&mut self, name: impl Into<String>, value: impl Into<String>) {
        self.defines.insert(name.into(), value.into());
    }

    /// Process the source, returning the expanded text
    pub fn process(&mut self, source: &str) -> Result<PreprocessResult, Vec<PreprocessError>> {
        let result = self.process_source(source, 0);
        if self.errors.is_empty() {
            Ok(PreprocessResult {
                source: result,
                pragmas: self.pragmas.clone(),
            })
        } else {
            Err(self.errors.clone())
        }
    }

    fn process_source(&mut self, source: &str, depth: usize) -> String {
        if depth > 64 {
            self.errors.push(PreprocessError {
                message: "include depth exceeded (max 64)".into(),
                line: 0,
            });
            return String::new();
        }

        let mut output = String::new();
        let mut if_stack: Vec<IfState> = Vec::new();
        let mut line_num = 0;
        let mut in_block_comment = false;

        for line in source.lines() {
            line_num += 1;
            let trimmed = line.trim();

            // Track block comments: skip directive processing inside /* ... */
            if in_block_comment {
                if trimmed.contains("*/") {
                    in_block_comment = false;
                }
                // Only output if in an active conditional block
                let active = if_stack.iter().all(|s| s.active);
                if active {
                    let expanded = self.expand_macros(line);
                    output.push_str(&expanded);
                    output.push('\n');
                }
                continue;
            }

            // Check for block comment start (simple heuristic: not inside a string)
            if trimmed.contains("/*") {
                if !trimmed.contains("*/") || trimmed.find("/*") > trimmed.rfind("*/") {
                    in_block_comment = true;
                }
                // Only output if in an active conditional block
                let active = if_stack.iter().all(|s| s.active);
                if active {
                    let expanded = self.expand_macros(line);
                    output.push_str(&expanded);
                    output.push('\n');
                }
                continue;
            }

            // Check if we're inside a false conditional
            let active = if_stack.iter().all(|s| s.active);

            if let Some(directive) = trimmed.strip_prefix('#') {
                let directive = directive.trim();

                // Handle conditional directives even when inactive
                if let Some(rest) = directive.strip_prefix("ifdef") {
                    let name = rest.trim();
                    if active {
                        let defined = self.defines.contains_key(name);
                        if_stack.push(IfState {
                            active: defined,
                            seen_true: defined,
                            else_seen: false,
                        });
                    } else {
                        // Nested conditional in inactive block
                        if_stack.push(IfState {
                            active: false,
                            seen_true: true, // prevent else from activating
                            else_seen: false,
                        });
                    }
                    continue;
                }

                if let Some(rest) = directive.strip_prefix("ifndef") {
                    let name = rest.trim();
                    if active {
                        let not_defined = !self.defines.contains_key(name);
                        if_stack.push(IfState {
                            active: not_defined,
                            seen_true: not_defined,
                            else_seen: false,
                        });
                    } else {
                        if_stack.push(IfState {
                            active: false,
                            seen_true: true,
                            else_seen: false,
                        });
                    }
                    continue;
                }

                if directive == "else" {
                    if let Some(last_idx) = if_stack.len().checked_sub(1) {
                        if if_stack[last_idx].else_seen {
                            self.errors.push(PreprocessError {
                                message: "duplicate #else".into(),
                                line: line_num,
                            });
                        }
                        let seen_true = if_stack[last_idx].seen_true;
                        // Only activate else if parent is active and we haven't seen a true branch
                        let parent_active =
                            last_idx == 0 || if_stack[..last_idx].iter().all(|s| s.active);
                        if_stack[last_idx].else_seen = true;
                        if_stack[last_idx].active = parent_active && !seen_true;
                    } else {
                        self.errors.push(PreprocessError {
                            message: "#else without matching #ifdef/#ifndef".into(),
                            line: line_num,
                        });
                    }
                    continue;
                }

                if directive == "endif" {
                    if if_stack.pop().is_none() {
                        self.errors.push(PreprocessError {
                            message: "#endif without matching #ifdef/#ifndef".into(),
                            line: line_num,
                        });
                    }
                    continue;
                }

                if !active {
                    continue;
                }

                // Active-only directives
                if let Some(rest) = directive.strip_prefix("define") {
                    let rest = rest.trim();
                    // Split name and value
                    if let Some(space_pos) = rest.find(|c: char| c.is_whitespace()) {
                        let name = &rest[..space_pos];
                        let value = rest[space_pos..].trim();
                        self.defines.insert(name.to_string(), value.to_string());
                    } else if !rest.is_empty() {
                        self.defines.insert(rest.to_string(), String::new());
                    } else {
                        self.errors.push(PreprocessError {
                            message: "#define requires a name".into(),
                            line: line_num,
                        });
                    }
                    continue;
                }

                if let Some(rest) = directive.strip_prefix("undef") {
                    let name = rest.trim();
                    if !name.is_empty() {
                        self.defines.remove(name);
                    } else {
                        self.errors.push(PreprocessError {
                            message: "#undef requires a name".into(),
                            line: line_num,
                        });
                    }
                    continue;
                }

                if let Some(rest) = directive.strip_prefix("include") {
                    let rest = rest.trim();
                    let (path, is_system) = if rest.starts_with('"') && rest.ends_with('"') {
                        (&rest[1..rest.len() - 1], false)
                    } else if rest.starts_with('<') && rest.ends_with('>') {
                        (&rest[1..rest.len() - 1], true)
                    } else {
                        self.errors.push(PreprocessError {
                            message: format!("invalid #include syntax: {}", rest),
                            line: line_num,
                        });
                        continue;
                    };

                    // Check for #pragma once
                    if self.included.contains(path) {
                        continue;
                    }

                    if let Some(content) = self.resolver.resolve(path, is_system) {
                        // Check if the included file has #pragma once
                        let has_pragma_once = content.lines().any(|l| l.trim() == "#pragma once");
                        if has_pragma_once {
                            self.included.insert(path.to_string());
                        }
                        let expanded = self.process_source(&content, depth + 1);
                        output.push_str(&expanded);
                        output.push('\n');
                    } else {
                        self.errors.push(PreprocessError {
                            message: format!("cannot resolve include: {}", path),
                            line: line_num,
                        });
                    }
                    continue;
                }

                if let Some(rest) = directive.strip_prefix("pragma") {
                    let rest = rest.trim();
                    self.process_pragma(rest, line_num);
                    continue;
                }

                // #error — skip (we are not ImHex, so conditional #error is irrelevant)
                if directive.starts_with("error") {
                    continue;
                }

                // Unknown directive
                self.errors.push(PreprocessError {
                    message: format!("unknown preprocessor directive: #{}", directive),
                    line: line_num,
                });
                continue;
            }

            if !active {
                continue;
            }

            // Handle `import X.Y.Z;` statements
            if let Some(file_path) = parse_import_line(trimmed) {
                // Dedup via #pragma once mechanism
                if self.included.contains(&file_path) {
                    continue;
                }
                if let Some(content) = self.resolver.resolve(&file_path, true) {
                    // Always mark as included to avoid duplicate imports
                    self.included.insert(file_path);
                    let expanded = self.process_source(&content, depth + 1);
                    output.push_str(&expanded);
                    output.push('\n');
                } else {
                    // Unresolvable import: pass through as-is for the parser
                    let expanded = self.expand_macros(line);
                    output.push_str(&expanded);
                    output.push('\n');
                }
                continue;
            }

            // Macro expansion in regular lines
            let expanded = self.expand_macros(line);
            output.push_str(&expanded);
            output.push('\n');
        }

        if !if_stack.is_empty() {
            self.errors.push(PreprocessError {
                message: format!("{} unterminated #ifdef/#ifndef", if_stack.len()),
                line: line_num,
            });
        }

        output
    }

    fn process_pragma(&mut self, pragma: &str, line_num: usize) {
        if pragma == "once" {
            self.pragmas.once = true;
            return;
        }

        if let Some(rest) = pragma.strip_prefix("endian") {
            let val = rest.trim();
            match val {
                "little" => self.pragmas.endian = Some(Endian::Little),
                "big" => self.pragmas.endian = Some(Endian::Big),
                _ => {
                    self.errors.push(PreprocessError {
                        message: format!("invalid endian value: {}", val),
                        line: line_num,
                    });
                }
            }
            return;
        }

        if let Some(rest) = pragma.strip_prefix("bitfield_bit_order") {
            let val = rest.trim();
            match val {
                "left_to_right" => self.pragmas.bitfield_bit_order = Some(BitOrder::LeftToRight),
                "right_to_left" => self.pragmas.bitfield_bit_order = Some(BitOrder::RightToLeft),
                _ => {
                    self.errors.push(PreprocessError {
                        message: format!("invalid bitfield_bit_order value: {}", val),
                        line: line_num,
                    });
                }
            }
            return;
        }

        if let Some(rest) = pragma.strip_prefix("array_limit") {
            let val = rest.trim();
            match parse_int_value(val) {
                Some(n) => self.pragmas.array_limit = n,
                None => {
                    self.errors.push(PreprocessError {
                        message: format!("invalid array_limit value: {}", val),
                        line: line_num,
                    });
                }
            }
            return;
        }

        if let Some(rest) = pragma.strip_prefix("pattern_limit") {
            let val = rest.trim();
            match parse_int_value(val) {
                Some(n) => self.pragmas.pattern_limit = n,
                None => {
                    self.errors.push(PreprocessError {
                        message: format!("invalid pattern_limit value: {}", val),
                        line: line_num,
                    });
                }
            }
            return;
        }

        // eval_depth is an alias for recursion_depth (used by ImHex patterns)
        if let Some(rest) = pragma.strip_prefix("eval_depth") {
            let val = rest.trim();
            match parse_int_value(val) {
                Some(n) => self.pragmas.recursion_depth = n as u32,
                None => {
                    self.errors.push(PreprocessError {
                        message: format!("invalid eval_depth value: {}", val),
                        line: line_num,
                    });
                }
            }
            return;
        }

        if let Some(rest) = pragma.strip_prefix("recursion_depth") {
            let val = rest.trim();
            match parse_int_value(val) {
                Some(n) => self.pragmas.recursion_depth = n as u32,
                None => {
                    self.errors.push(PreprocessError {
                        message: format!("invalid recursion_depth value: {}", val),
                        line: line_num,
                    });
                }
            }
            return;
        }

        // Store unknown pragmas as metadata instead of raising an error
        if let Some(space_pos) = pragma.find(|c: char| c.is_whitespace()) {
            let key = pragma[..space_pos].to_string();
            let value = pragma[space_pos..].trim().to_string();
            self.pragmas.metadata.insert(key, value);
        } else {
            self.pragmas
                .metadata
                .insert(pragma.to_string(), String::new());
        }
    }

    fn expand_macros(&self, line: &str) -> String {
        if self.defines.is_empty() {
            return line.to_string();
        }

        let mut result = line.to_string();
        // Iterate up to a reasonable limit to handle chained expansions
        for _ in 0..16 {
            let mut changed = false;
            for (name, value) in &self.defines {
                if result.contains(name.as_str()) {
                    // Only replace whole word matches
                    let new = replace_whole_word(&result, name, value);
                    if new != result {
                        result = new;
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }
        result
    }
}

/// Replace whole-word occurrences of `word` with `replacement`
fn replace_whole_word(text: &str, word: &str, replacement: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let bytes = text.as_bytes();
    let word_bytes = word.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if i + word_bytes.len() <= bytes.len() && &bytes[i..i + word_bytes.len()] == word_bytes {
            // Check word boundary before
            let before_ok = i == 0 || !is_ident_char(bytes[i - 1]);
            // Check word boundary after
            let after_ok =
                i + word_bytes.len() >= bytes.len() || !is_ident_char(bytes[i + word_bytes.len()]);

            if before_ok && after_ok {
                result.push_str(replacement);
                i += word_bytes.len();
                continue;
            }
        }
        result.push(bytes[i] as char);
        i += 1;
    }

    result
}

/// Parse an integer value, supporting decimal and 0x hex prefix
fn parse_int_value(s: &str) -> Option<u64> {
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        u64::from_str_radix(hex, 16).ok()
    } else {
        s.parse::<u64>().ok()
    }
}

fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Parse an `import X.Y.Z;` line, returning the module path as `X/Y/Z.pat`.
/// Returns None if the line is not an import statement.
fn parse_import_line(trimmed: &str) -> Option<String> {
    // Must start with "import " keyword
    let rest = trimmed.strip_prefix("import")?;
    // Ensure "import" is followed by whitespace (not "important" etc.)
    if !rest.starts_with(|c: char| c.is_whitespace()) {
        return None;
    }
    let rest = rest.trim();
    // Strip trailing semicolon
    let module_path = rest.strip_suffix(';').unwrap_or(rest).trim();
    if module_path.is_empty() {
        return None;
    }
    // Validate: only alphanumeric, underscore, and dots
    if !module_path
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.')
    {
        return None;
    }
    // Convert dots to path separators and append .pat
    let file_path = module_path.replace('.', "/") + ".pat";
    Some(file_path)
}

struct IfState {
    active: bool,
    seen_true: bool,
    else_seen: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn preprocess(source: &str) -> PreprocessResult {
        let resolver = NoopResolver;
        let mut pp = Preprocessor::new(&resolver);
        pp.process(source).expect("preprocessing failed")
    }

    fn preprocess_with_resolver(source: &str, resolver: &dyn IncludeResolver) -> PreprocessResult {
        let mut pp = Preprocessor::new(resolver);
        pp.process(source).expect("preprocessing failed")
    }

    fn preprocess_err(source: &str) -> Vec<PreprocessError> {
        let resolver = NoopResolver;
        let mut pp = Preprocessor::new(&resolver);
        pp.process(source).unwrap_err()
    }

    // --- #define ---

    #[test]
    fn test_define_simple() {
        let result = preprocess("#define FOO 42\nu32 x = FOO;");
        assert!(result.source.contains("u32 x = 42;"));
    }

    #[test]
    fn test_define_no_value() {
        let result = preprocess("#define FOO\n#ifdef FOO\nyes\n#endif");
        assert!(result.source.contains("yes"));
    }

    #[test]
    fn test_define_whole_word() {
        let result = preprocess("#define X 10\nXY = X;");
        // XY should not be changed, only standalone X
        assert!(result.source.contains("XY = 10;"));
    }

    #[test]
    fn test_define_chained() {
        let result = preprocess("#define A 1\n#define B A\nval = B;");
        assert!(result.source.contains("val = 1;"));
    }

    // --- #ifdef / #ifndef / #else / #endif ---

    #[test]
    fn test_ifdef_defined() {
        let result = preprocess("#define FOO\n#ifdef FOO\nyes\n#endif");
        assert!(result.source.contains("yes"));
    }

    #[test]
    fn test_ifdef_not_defined() {
        let result = preprocess("#ifdef FOO\nyes\n#endif\nafter");
        assert!(!result.source.contains("yes"));
        assert!(result.source.contains("after"));
    }

    #[test]
    fn test_ifndef_not_defined() {
        let result = preprocess("#ifndef FOO\nyes\n#endif");
        assert!(result.source.contains("yes"));
    }

    #[test]
    fn test_ifndef_defined() {
        let result = preprocess("#define FOO\n#ifndef FOO\nyes\n#endif");
        assert!(!result.source.contains("yes"));
    }

    #[test]
    fn test_ifdef_else() {
        let result = preprocess("#ifdef FOO\nyes\n#else\nno\n#endif");
        assert!(!result.source.contains("yes"));
        assert!(result.source.contains("no"));
    }

    #[test]
    fn test_ifdef_else_defined() {
        let result = preprocess("#define FOO\n#ifdef FOO\nyes\n#else\nno\n#endif");
        assert!(result.source.contains("yes"));
        assert!(!result.source.contains("no"));
    }

    #[test]
    fn test_nested_ifdef() {
        let result = preprocess("#define A\n#define B\n#ifdef A\n#ifdef B\nboth\n#endif\n#endif");
        assert!(result.source.contains("both"));
    }

    #[test]
    fn test_nested_ifdef_inner_false() {
        let result =
            preprocess("#define A\n#ifdef A\n#ifdef B\ninner\n#else\nouter\n#endif\n#endif");
        assert!(!result.source.contains("inner"));
        assert!(result.source.contains("outer"));
    }

    // --- #pragma ---

    #[test]
    fn test_pragma_endian_little() {
        let result = preprocess("#pragma endian little");
        assert_eq!(result.pragmas.endian, Some(Endian::Little));
    }

    #[test]
    fn test_pragma_endian_big() {
        let result = preprocess("#pragma endian big");
        assert_eq!(result.pragmas.endian, Some(Endian::Big));
    }

    #[test]
    fn test_pragma_array_limit() {
        let result = preprocess("#pragma array_limit 100");
        assert_eq!(result.pragmas.array_limit, 100);
    }

    #[test]
    fn test_pragma_pattern_limit() {
        let result = preprocess("#pragma pattern_limit 50000");
        assert_eq!(result.pragmas.pattern_limit, 50000);
    }

    #[test]
    fn test_pragma_recursion_depth() {
        let result = preprocess("#pragma recursion_depth 64");
        assert_eq!(result.pragmas.recursion_depth, 64);
    }

    #[test]
    fn test_pragma_eval_depth_alias() {
        let result = preprocess("#pragma eval_depth 100");
        assert_eq!(result.pragmas.recursion_depth, 100);
    }

    #[test]
    fn test_pragma_eval_depth_large() {
        let result = preprocess("#pragma eval_depth 65536");
        assert_eq!(result.pragmas.recursion_depth, 65536);
    }

    #[test]
    fn test_pragma_bitfield_order() {
        let result = preprocess("#pragma bitfield_bit_order left_to_right");
        assert_eq!(
            result.pragmas.bitfield_bit_order,
            Some(BitOrder::LeftToRight)
        );
    }

    #[test]
    fn test_pragma_once() {
        let result = preprocess("#pragma once");
        assert!(result.pragmas.once);
    }

    // --- #include ---

    #[test]
    fn test_include_basic() {
        let mut resolver = MapResolver::new();
        resolver.add("header.hexpat", "u32 magic;");
        let result = preprocess_with_resolver("#include \"header.hexpat\"\nu8 x;", &resolver);
        assert!(result.source.contains("u32 magic;"));
        assert!(result.source.contains("u8 x;"));
    }

    #[test]
    fn test_include_system() {
        let mut resolver = MapResolver::new();
        resolver.add("std/mem", "fn eof() -> bool;");
        let result = preprocess_with_resolver("#include <std/mem>\ncode;", &resolver);
        assert!(result.source.contains("fn eof() -> bool;"));
    }

    #[test]
    fn test_include_pragma_once() {
        let mut resolver = MapResolver::new();
        resolver.add("header.hexpat", "#pragma once\nu32 magic;");
        let result = preprocess_with_resolver(
            "#include \"header.hexpat\"\n#include \"header.hexpat\"\nend;",
            &resolver,
        );
        // Should only include once
        let count = result.source.matches("u32 magic;").count();
        assert_eq!(count, 1);
    }

    #[test]
    fn test_include_nested() {
        let mut resolver = MapResolver::new();
        resolver.add("inner.hexpat", "u8 inner;");
        resolver.add("outer.hexpat", "#include \"inner.hexpat\"\nu16 outer;");
        let result = preprocess_with_resolver("#include \"outer.hexpat\"", &resolver);
        assert!(result.source.contains("u8 inner;"));
        assert!(result.source.contains("u16 outer;"));
    }

    // --- Error cases ---

    #[test]
    fn test_error_unresolved_include() {
        let errors = preprocess_err("#include \"nonexistent.hexpat\"");
        assert!(errors.iter().any(|e| e.message.contains("cannot resolve")));
    }

    #[test]
    fn test_error_endif_without_ifdef() {
        let errors = preprocess_err("#endif");
        assert!(errors
            .iter()
            .any(|e| e.message.contains("#endif without matching")));
    }

    #[test]
    fn test_error_unterminated_ifdef() {
        let errors = preprocess_err("#ifdef FOO\nyes");
        assert!(errors.iter().any(|e| e.message.contains("unterminated")));
    }

    #[test]
    fn test_error_duplicate_else() {
        let errors = preprocess_err("#ifdef FOO\n#else\n#else\n#endif");
        assert!(errors.iter().any(|e| e.message.contains("duplicate #else")));
    }

    // --- Passthrough ---

    #[test]
    fn test_regular_lines_preserved() {
        let result = preprocess("u32 magic;\nu16 version;");
        assert!(result.source.contains("u32 magic;"));
        assert!(result.source.contains("u16 version;"));
    }

    #[test]
    fn test_empty_input() {
        let result = preprocess("");
        assert!(result.source.is_empty() || result.source.trim().is_empty());
    }

    // --- Unknown pragmas stored as metadata ---

    #[test]
    fn test_pragma_unknown_stored_as_metadata() {
        let result = preprocess(
            "#pragma author me\n#pragma description A test\n#pragma MIME application/octet-stream",
        );
        assert_eq!(result.pragmas.metadata.get("author").unwrap(), "me");
        assert_eq!(
            result.pragmas.metadata.get("description").unwrap(),
            "A test"
        );
        assert_eq!(
            result.pragmas.metadata.get("MIME").unwrap(),
            "application/octet-stream"
        );
    }

    // --- import ---

    #[test]
    fn test_import_basic() {
        let mut resolver = MapResolver::new();
        resolver.add("std/mem.pat", "fn std_mem();");
        let result = preprocess_with_resolver("import std.mem;\ncode;", &resolver);
        assert!(result.source.contains("fn std_mem();"));
        assert!(result.source.contains("code;"));
    }

    #[test]
    fn test_import_dedup() {
        let mut resolver = MapResolver::new();
        resolver.add("std/mem.pat", "fn std_mem();");
        let result = preprocess_with_resolver("import std.mem;\nimport std.mem;\ncode;", &resolver);
        let count = result.source.matches("fn std_mem();").count();
        assert_eq!(count, 1);
    }

    #[test]
    fn test_import_not_confused_with_ident() {
        // "important" should not be treated as an import statement
        let result = preprocess("u32 important = 1;");
        assert!(result.source.contains("u32 important = 1;"));
    }

    #[test]
    fn test_import_unresolvable_passthrough() {
        // Unresolvable import should pass through without error
        let result = preprocess("import unknown.module;\ncode;");
        assert!(result.source.contains("import unknown.module;"));
        assert!(result.source.contains("code;"));
    }

    // --- #undef ---

    #[test]
    fn test_undef() {
        let result = preprocess("#define FOO 42\n#undef FOO\n#ifdef FOO\nyes\n#else\nno\n#endif");
        assert!(!result.source.contains("yes"));
        assert!(result.source.contains("no"));
    }

    // --- Error case tests ---

    fn preprocess_errors(source: &str) -> Vec<PreprocessError> {
        let resolver = NoopResolver;
        let mut pp = Preprocessor::new(&resolver);
        pp.process(source).unwrap_err()
    }

    #[test]
    fn test_pragma_array_limit_large_value() {
        // Large values within u64 range should be accepted
        let result = preprocess("#pragma array_limit 0xFFFFFFFF");
        assert_eq!(result.pragmas.array_limit, 0xFFFF_FFFF);
    }

    #[test]
    fn test_pragma_array_limit_overflow() {
        // Values exceeding u64 range should produce an error
        let errors = preprocess_errors("#pragma array_limit 99999999999999999999");
        assert!(
            errors.iter().any(|e| e.message.contains("invalid")),
            "expected 'invalid' error, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_pragma_recursion_depth_large_accepted() {
        // Large recursion depth values are accepted (no upper limit)
        let result = preprocess("#pragma recursion_depth 99999");
        assert_eq!(result.pragmas.recursion_depth, 99999);
    }

    #[test]
    fn test_block_comment_hides_directive() {
        let result = preprocess("/* #define FOO bar */\n#ifdef FOO\nyes\n#else\nno\n#endif");
        assert!(!result.source.contains("yes"));
        assert!(result.source.contains("no"));
    }

    #[test]
    fn test_nested_ifdef_outer_false() {
        // When outer is false, inner directives should be skipped
        let result = preprocess(
            "#ifdef UNDEF\n#ifdef ALSO_UNDEF\ninner\n#endif\nouter\n#else\nfallback\n#endif",
        );
        assert!(!result.source.contains("inner"));
        assert!(!result.source.contains("outer"));
        assert!(result.source.contains("fallback"));
    }
}
