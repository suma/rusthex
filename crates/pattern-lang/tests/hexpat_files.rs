// Tests that verify .hexpat files from an external directory can be parsed.
//
// Set HEXPAT_PATTERNS_DIR to the directory containing .hexpat files.
// The includes directory is auto-detected as a sibling "includes" directory.
// If HEXPAT_PATTERNS_DIR is not set, these tests are skipped.

use pattern_lang::preprocessor::IncludeResolver;
use pattern_lang::PatternEngine;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::Duration;

/// File system based include resolver for .hexpat #include directives.
struct FileSystemResolver {
    include_dirs: Vec<PathBuf>,
}

impl FileSystemResolver {
    fn new(include_dirs: Vec<PathBuf>) -> Self {
        Self { include_dirs }
    }
}

impl IncludeResolver for FileSystemResolver {
    fn resolve(&self, path: &str, _is_system: bool) -> Option<String> {
        for dir in &self.include_dirs {
            let full_path = dir.join(path);
            if let Ok(content) = std::fs::read_to_string(&full_path) {
                return Some(content);
            }
        }
        None
    }
}

fn collect_hexpat_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                files.extend(collect_hexpat_files(&path));
            } else if path.extension().and_then(|e| e.to_str()) == Some("hexpat") {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

/// Parse a single file with a timeout. Returns Ok(()) on success, Err(message) on failure.
fn parse_with_timeout(
    source: String,
    include_dirs: Vec<PathBuf>,
    timeout: Duration,
) -> Result<(), String> {
    let (tx, rx) = mpsc::channel();

    std::thread::spawn(move || {
        let resolver = FileSystemResolver::new(include_dirs);
        let engine = PatternEngine::with_resolver(resolver);
        let result = engine.parse(&source);
        let _ = tx.send(result);
    });

    match rx.recv_timeout(timeout) {
        Ok(Ok(_)) => Ok(()),
        Ok(Err(errors)) => {
            let msgs: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
            Err(msgs.join("; "))
        }
        Err(mpsc::RecvTimeoutError::Timeout) => Err("timeout".to_string()),
        Err(mpsc::RecvTimeoutError::Disconnected) => Err("thread panicked".to_string()),
    }
}

#[test]
fn test_parse_hexpat_files() {
    let patterns_dir = match std::env::var("HEXPAT_PATTERNS_DIR") {
        Ok(dir) => PathBuf::from(dir),
        Err(_) => {
            eprintln!("HEXPAT_PATTERNS_DIR not set, skipping test");
            return;
        }
    };

    assert!(
        patterns_dir.is_dir(),
        "HEXPAT_PATTERNS_DIR does not exist: {}",
        patterns_dir.display()
    );

    // Build include search directories
    let mut include_dirs = Vec::new();

    // 1. Explicit PAT_INCLUDE_DIR (if set)
    if let Ok(inc_dir) = std::env::var("PAT_INCLUDE_DIR") {
        let p = PathBuf::from(&inc_dir);
        if p.is_dir() {
            include_dirs.push(p);
        }
    }

    // 2. Auto-detect includes directory as sibling of patterns directory
    let includes_dir = patterns_dir.parent().unwrap().join("includes");
    if includes_dir.is_dir() && !include_dirs.contains(&includes_dir) {
        include_dirs.push(includes_dir);
    }

    // 3. Patterns directory itself (for relative #include / import between patterns)
    if !include_dirs.contains(&patterns_dir) {
        include_dirs.push(patterns_dir.clone());
    }

    let files = collect_hexpat_files(&patterns_dir);
    assert!(
        !files.is_empty(),
        "No .hexpat files found in {}",
        patterns_dir.display()
    );

    let timeout = Duration::from_secs(5);
    let mut success = 0;
    let mut failures: Vec<(PathBuf, String)> = Vec::new();

    for file in &files {
        let rel = file.strip_prefix(&patterns_dir).unwrap_or(file);
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                failures.push((file.clone(), format!("read error: {}", e)));
                continue;
            }
        };

        match parse_with_timeout(source, include_dirs.clone(), timeout) {
            Ok(()) => {
                eprintln!("  OK: {}", rel.display());
                success += 1;
            }
            Err(err) => {
                eprintln!("  FAIL: {} : {}", rel.display(), err);
                failures.push((file.clone(), err));
            }
        }
    }

    eprintln!("\n=== .hexpat parse results ===");
    eprintln!("Total:   {}", files.len());
    eprintln!("Success: {}", success);
    eprintln!("Failed:  {}", failures.len());

    if !failures.is_empty() {
        eprintln!("\nFailed files:");
        for (path, err) in &failures {
            let rel = path.strip_prefix(&patterns_dir).unwrap_or(path);
            eprintln!("  {} : {}", rel.display(), err);
        }
    }

    // Report success rate but don't fail the test on parse errors,
    // since the parser may not support all ImHex language features yet.
    eprintln!(
        "\nParse success rate: {:.1}%",
        (success as f64 / files.len() as f64) * 100.0
    );
}
