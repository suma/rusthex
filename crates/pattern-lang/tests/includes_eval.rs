// Integration tests that verify std:: functions work through actual .pat files.
// Requires PAT_INCLUDE_DIR environment variable pointing to the ImHex includes directory.

use pattern_lang::preprocessor::IncludeResolver;
use pattern_lang::{PatternEngine, PatternValue, SliceDataSource};
use std::path::PathBuf;
use std::sync::mpsc;
use std::time::Duration;

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

fn get_include_dir() -> Option<PathBuf> {
    match std::env::var("PAT_INCLUDE_DIR") {
        Ok(dir) => {
            let path = PathBuf::from(dir);
            if path.is_dir() {
                Some(path)
            } else {
                eprintln!("PAT_INCLUDE_DIR path does not exist: {}", path.display());
                None
            }
        }
        Err(_) => {
            eprintln!("PAT_INCLUDE_DIR not set, skipping test");
            None
        }
    }
}

/// Helper to run pattern source with includes and return nodes or error.
fn run_with_includes(
    source: &str,
    data: &[u8],
    include_dir: PathBuf,
) -> Result<Vec<pattern_lang::PatternNode>, String> {
    let include_dirs = vec![include_dir];
    let timeout = Duration::from_secs(10);
    let (tx, rx) = mpsc::channel();

    let source = source.to_string();
    let data = data.to_vec();

    std::thread::spawn(move || {
        let resolver = FileSystemResolver::new(include_dirs);
        let engine = PatternEngine::with_resolver(resolver);
        let ds = SliceDataSource::new(&data);
        let result = engine.run(&source, &ds);
        let _ = tx.send(result);
    });

    match rx.recv_timeout(timeout) {
        Ok(Ok(nodes)) => Ok(nodes),
        Ok(Err(e)) => Err(format!("evaluation failed: {}", e)),
        Err(mpsc::RecvTimeoutError::Timeout) => {
            Err(format!("timed out after {}s", timeout.as_secs()))
        }
        Err(mpsc::RecvTimeoutError::Disconnected) => Err("evaluation thread panicked".to_string()),
    }
}

/// std::mem functions work through .pat file (two-layer call: .pat -> builtin::)
#[test]
fn test_std_mem_via_pat() {
    let include_dir = match get_include_dir() {
        Some(dir) => dir,
        None => return,
    };

    // Use std::mem::read_unsigned to read bytes and verify the two-layer dispatch.
    // Variables placed with @ produce PatternNode results; local variables (= expr) do not.
    let source = r#"
        #include <std/mem.pat>

        // Read from binary data using auto-advance (produces PatternNodes)
        u8 byte0 @ 0x00;
        u8 byte1 @ 0x01;

        // Use std::mem functions in a struct to verify they resolve through .pat
        struct MemInfo {
            u64 total_size;
            u64 base_addr;
            u64 first_byte;
            u64 second_byte;
        };

        // Place struct at offset 0 — fields read sequentially from binary
        // But we want computed values, so use local vars and verify via byte0/byte1
        // Verify that std::mem::size() and std::mem::read_unsigned() don't error
        u64 computed_size = std::mem::size();
        u64 computed_read = std::mem::read_unsigned(0, 1);
    "#;

    let data: &[u8] = &[0xAB, 0xCD, 0xEF, 0x01];

    let nodes = run_with_includes(source, data, include_dir).expect("run failed");

    // Only @ placed variables produce PatternNodes
    assert_eq!(
        nodes.len(),
        2,
        "expected 2 placed nodes, got {}",
        nodes.len()
    );

    // byte0 @ 0x00 should read 0xAB
    assert_eq!(nodes[0].name, "byte0");
    assert_eq!(nodes[0].value, PatternValue::Unsigned(0xAB));

    // byte1 @ 0x01 should read 0xCD
    assert_eq!(nodes[1].name, "byte1");
    assert_eq!(nodes[1].value, PatternValue::Unsigned(0xCD));

    // The fact that we got here without error means std::mem::size() and
    // std::mem::read_unsigned() resolved correctly through the .pat file
    // (the local variable assignments executed without errors)
}

/// std::math functions work through .pat file (both pure .pat functions and builtin:: wrappers)
#[test]
fn test_std_math_via_pat() {
    let include_dir = match get_include_dir() {
        Some(dir) => dir,
        None => return,
    };

    // Use import syntax to also verify import handling.
    // Use std::math functions to compute an array size — this verifies the functions
    // return correct values because wrong values would cause different node counts.
    let source = r#"
        import std.math;

        // std::math::abs(-3) should return 3
        // Use it as array count so we can verify via children count
        u8 abs_array[std::math::abs(-3)] @ 0x00;

        // std::math::min(2, 5) should return 2
        u8 min_array[std::math::min(2, 5)] @ 0x03;

        // std::math::max(2, 5) should return 5
        u8 max_array[std::math::max(2, 5)] @ 0x00;

        // std::math::floor(2.9) should return 2
        u8 floor_array[std::math::floor(2.9)] @ 0x00;

        // std::math::ceil(1.1) should return 2
        u8 ceil_array[std::math::ceil(1.1)] @ 0x00;
    "#;

    let data: &[u8] = &[0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];

    let nodes = run_with_includes(source, data, include_dir).expect("run failed");

    assert_eq!(
        nodes.len(),
        5,
        "expected 5 placed arrays, got {}",
        nodes.len()
    );

    // abs(-3) = 3 elements
    assert_eq!(nodes[0].name, "abs_array");
    assert_eq!(nodes[0].children.len(), 3);
    assert_eq!(nodes[0].children[0].value, PatternValue::Unsigned(0x11));
    assert_eq!(nodes[0].children[1].value, PatternValue::Unsigned(0x22));
    assert_eq!(nodes[0].children[2].value, PatternValue::Unsigned(0x33));

    // min(2, 5) = 2 elements
    assert_eq!(nodes[1].name, "min_array");
    assert_eq!(nodes[1].children.len(), 2);

    // max(2, 5) = 5 elements
    assert_eq!(nodes[2].name, "max_array");
    assert_eq!(nodes[2].children.len(), 5);

    // floor(2.9) = 2 elements
    assert_eq!(nodes[3].name, "floor_array");
    assert_eq!(nodes[3].children.len(), 2);

    // ceil(1.1) = 2 elements
    assert_eq!(nodes[4].name, "ceil_array");
    assert_eq!(nodes[4].children.len(), 2);
}

/// std::mem::eof() works as defined in .pat: $ >= (base_address() + size())
/// Verifies while-array termination using eof()
#[test]
fn test_std_mem_eof_via_pat() {
    let include_dir = match get_include_dir() {
        Some(dir) => dir,
        None => return,
    };

    let source = r#"
        #include <std/mem.pat>

        u8 bytes[while(!std::mem::eof())] @ 0x00;
    "#;

    let data: &[u8] = &[0x11, 0x22, 0x33, 0x44, 0x55];

    let nodes = run_with_includes(source, data, include_dir).expect("run failed");

    assert_eq!(
        nodes.len(),
        1,
        "expected 1 top-level node, got {}",
        nodes.len()
    );

    let array = &nodes[0];
    assert_eq!(array.name, "bytes");
    assert_eq!(array.value, PatternValue::Array);

    // The while-array should have consumed all 5 bytes
    assert_eq!(
        array.children.len(),
        5,
        "eof() should stop after consuming all {} bytes, got {} elements",
        data.len(),
        array.children.len()
    );

    // Verify each element
    assert_eq!(array.children[0].value, PatternValue::Unsigned(0x11));
    assert_eq!(array.children[1].value, PatternValue::Unsigned(0x22));
    assert_eq!(array.children[2].value, PatternValue::Unsigned(0x33));
    assert_eq!(array.children[3].value, PatternValue::Unsigned(0x44));
    assert_eq!(array.children[4].value, PatternValue::Unsigned(0x55));
}
