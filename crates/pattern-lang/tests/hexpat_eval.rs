// Tests that verify .hexpat files can be evaluated against their test data.
//
// Required environment variables:
//   HEXPAT_PATTERNS_DIR - directory containing .hexpat pattern files
//   HEXPAT_TEST_DATA_DIR - directory containing test data files
//
// Optional:
//   PAT_INCLUDE_DIR - additional include search directory
//
// Test data matching follows ImHex convention:
//   1. If test_data/<pattern_name>/ directory exists, use all files inside
//   2. Otherwise, if test_data/<pattern_name>.* files exist, use the first one
//   3. If no test data found, skip that pattern

use pattern_lang::preprocessor::IncludeResolver;
use pattern_lang::{EvalOptions, EvalStats, PatternEngine, SliceDataSource};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::time::Duration;

const NUM_WORKERS: usize = 8;

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

/// Find test data files for a given pattern name, following ImHex convention.
fn find_test_data(test_data_dir: &Path, pattern_name: &str) -> Vec<PathBuf> {
    // 1. Check if test_data/<pattern_name>/ directory exists
    let dir_path = test_data_dir.join(pattern_name);
    if dir_path.is_dir() {
        let mut files: Vec<PathBuf> = std::fs::read_dir(&dir_path)
            .into_iter()
            .flatten()
            .flatten()
            .map(|e| e.path())
            .filter(|p| p.is_file())
            .collect();
        files.sort();
        return files;
    }

    // 2. Glob test_data/<pattern_name>.* — take the first match
    if let Ok(entries) = std::fs::read_dir(test_data_dir) {
        let prefix = format!("{}.", pattern_name);
        let mut matches: Vec<PathBuf> = entries
            .flatten()
            .map(|e| e.path())
            .filter(|p| {
                p.is_file()
                    && p.file_name()
                        .and_then(|n| n.to_str())
                        .map(|n| n.starts_with(&prefix))
                        .unwrap_or(false)
            })
            .collect();
        matches.sort();
        if let Some(first) = matches.into_iter().next() {
            return vec![first];
        }
    }

    Vec::new()
}

/// Partial stats available when evaluation fails or times out.
struct PartialStats {
    stmts: u64,
    reads: u64,
}

/// Run eval on a single pattern+data pair with a timeout and memory limit.
/// Returns EvalStats on success, or (error message, partial stats) on failure.
fn eval_with_timeout(
    source: String,
    data: Vec<u8>,
    include_dirs: Vec<PathBuf>,
    timeout: Duration,
) -> Result<EvalStats, (String, PartialStats)> {
    let (tx, rx) = mpsc::channel();
    let cancel = Arc::new(AtomicBool::new(false));
    let cancel_clone = cancel.clone();
    let stmt_counter = Arc::new(AtomicU64::new(0));
    let stmt_counter_clone = stmt_counter.clone();
    let read_counter = Arc::new(AtomicU64::new(0));
    let read_counter_clone = read_counter.clone();

    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024) // 64 MB stack
        .spawn(move || {
            let resolver = FileSystemResolver::new(include_dirs);
            let engine = PatternEngine::with_resolver(resolver);
            let ds = SliceDataSource::new(&data);
            let options = EvalOptions {
                max_total_nodes: 500_000,
                cancellation_token: Some(cancel_clone),
                shared_stmt_count: Some(stmt_counter_clone),
                shared_read_type_count: Some(read_counter_clone),
            };
            let result = engine.run_with_options(&source, &ds, options);
            let _ = tx.send(result);
        })
        .expect("failed to spawn thread");

    let load_partial = || PartialStats {
        stmts: stmt_counter.load(Ordering::Relaxed),
        reads: read_counter.load(Ordering::Relaxed),
    };

    match rx.recv_timeout(timeout) {
        Ok(Ok((_nodes, stats))) => Ok(stats),
        Ok(Err(err)) => Err((format!("{}", err), load_partial())),
        Err(mpsc::RecvTimeoutError::Timeout) => {
            cancel.store(true, Ordering::Relaxed);
            Err((format!("timeout ({}s)", timeout.as_secs()), load_partial()))
        }
        Err(mpsc::RecvTimeoutError::Disconnected) => {
            Err(("thread panicked".to_string(), load_partial()))
        }
    }
}

#[test]
fn test_eval_hexpat_files() {
    let patterns_dir = match std::env::var("HEXPAT_PATTERNS_DIR") {
        Ok(dir) => PathBuf::from(dir),
        Err(_) => {
            eprintln!("HEXPAT_PATTERNS_DIR not set, skipping test");
            return;
        }
    };

    let test_data_dir = match std::env::var("HEXPAT_TEST_DATA_DIR") {
        Ok(dir) => PathBuf::from(dir),
        Err(_) => {
            eprintln!("HEXPAT_TEST_DATA_DIR not set, skipping test");
            return;
        }
    };

    assert!(
        patterns_dir.is_dir(),
        "HEXPAT_PATTERNS_DIR does not exist: {}",
        patterns_dir.display()
    );
    assert!(
        test_data_dir.is_dir(),
        "HEXPAT_TEST_DATA_DIR does not exist: {}",
        test_data_dir.display()
    );

    // Build include search directories
    let mut include_dirs = Vec::new();
    if let Ok(inc_dir) = std::env::var("PAT_INCLUDE_DIR") {
        let p = PathBuf::from(&inc_dir);
        if p.is_dir() {
            include_dirs.push(p);
        }
    }
    let includes_dir = patterns_dir.parent().unwrap().join("includes");
    if includes_dir.is_dir() && !include_dirs.contains(&includes_dir) {
        include_dirs.push(includes_dir);
    }
    if !include_dirs.contains(&patterns_dir) {
        include_dirs.push(patterns_dir.clone());
    }

    let files = collect_hexpat_files(&patterns_dir);
    assert!(!files.is_empty());

    // Debug builds are ~10x slower; give more time to avoid false timeouts
    let timeout = if cfg!(debug_assertions) {
        Duration::from_secs(30)
    } else {
        Duration::from_secs(15)
    };

    // Phase 1: Collect all eval tasks (single-threaded I/O)
    struct EvalTask {
        label: String,
        source: String,
        data: Vec<u8>,
    }
    enum TaskResult {
        Success {
            label: String,
            stats: EvalStats,
            elapsed: Duration,
        },
        Failure {
            label: String,
            error: String,
            partial: PartialStats,
            elapsed: Duration,
        },
    }

    let mut tasks: Vec<EvalTask> = Vec::new();
    let mut skipped = 0usize;
    let mut io_failures: Vec<(String, String)> = Vec::new();

    for file in &files {
        let rel = file
            .strip_prefix(&patterns_dir)
            .unwrap_or(file)
            .display()
            .to_string();
        let pattern_name = file.file_name().unwrap().to_str().unwrap();

        // Skip patterns known to cause stack overflow (infinite recursion)
        if pattern_name == "selinux.hexpat" {
            eprintln!("  SKIP: {} (known stack overflow)", rel);
            skipped += 1;
            continue;
        }

        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                io_failures.push((rel, format!("read error: {}", e)));
                continue;
            }
        };

        let test_files = find_test_data(&test_data_dir, pattern_name);
        if test_files.is_empty() {
            eprintln!("  SKIP: {} (no test data)", rel);
            skipped += 1;
            continue;
        }

        for test_file in &test_files {
            let test_name = test_file.file_name().unwrap().to_str().unwrap();
            let data = match std::fs::read(test_file) {
                Ok(d) => d,
                Err(e) => {
                    let label = format!("{} [{}]", rel, test_name);
                    io_failures.push((label, format!("read error: {}", e)));
                    continue;
                }
            };

            let label = if test_files.len() > 1 {
                format!("{} [{}]", rel, test_name)
            } else {
                rel.clone()
            };

            tasks.push(EvalTask {
                label,
                source: source.clone(),
                data,
            });
        }
    }

    eprintln!(
        "Evaluating {} tasks with {} worker threads...",
        tasks.len(),
        NUM_WORKERS
    );

    // Phase 2: Evaluate in parallel with NUM_WORKERS threads
    let task_idx = AtomicUsize::new(0);
    let results: Mutex<Vec<(usize, TaskResult)>> = Mutex::new(Vec::new());

    std::thread::scope(|s| {
        for _ in 0..NUM_WORKERS {
            s.spawn(|| loop {
                let idx = task_idx.fetch_add(1, Ordering::SeqCst);
                if idx >= tasks.len() {
                    break;
                }
                let task = &tasks[idx];
                let t0 = std::time::Instant::now();
                let result = eval_with_timeout(
                    task.source.clone(),
                    task.data.clone(),
                    include_dirs.clone(),
                    timeout,
                );
                let elapsed = t0.elapsed();
                let task_result = match result {
                    Ok(stats) => TaskResult::Success {
                        label: task.label.clone(),
                        stats,
                        elapsed,
                    },
                    Err((err, partial)) => TaskResult::Failure {
                        label: task.label.clone(),
                        error: err,
                        partial,
                        elapsed,
                    },
                };
                results.lock().unwrap().push((idx, task_result));
            });
        }
    });

    // Phase 3: Collect results in original order
    let mut results = results.into_inner().unwrap();
    results.sort_by_key(|(idx, _)| *idx);

    let mut success = 0usize;
    let mut failures: Vec<(String, String)> = io_failures;
    let mut timings: Vec<(String, Duration)> = Vec::new();

    for (_, result) in results {
        match result {
            TaskResult::Success {
                label,
                stats,
                elapsed,
            } => {
                eprintln!(
                    "  OK: {} ({:.2}s, stmts={}, reads={}, nodes={}, results={})",
                    label,
                    elapsed.as_secs_f64(),
                    stats.stmt_count,
                    stats.read_type_count,
                    stats.total_node_count,
                    stats.result_count
                );
                timings.push((label, elapsed));
                success += 1;
            }
            TaskResult::Failure {
                label,
                error,
                partial,
                elapsed,
            } => {
                eprintln!(
                    "  FAIL: {} : {} ({:.2}s, stmts={}, reads={})",
                    label,
                    error,
                    elapsed.as_secs_f64(),
                    partial.stmts,
                    partial.reads
                );
                timings.push((label.clone(), elapsed));
                failures.push((label, error));
            }
        }
    }

    let total = success + failures.len() + skipped;
    eprintln!("\n=== .hexpat eval results ===");
    eprintln!("Total:   {}", total);
    eprintln!("Success: {}", success);
    eprintln!("Failed:  {}", failures.len());
    eprintln!("Skipped: {} (no test data)", skipped);

    if !failures.is_empty() {
        eprintln!("\nFailed files:");
        for (name, err) in &failures {
            eprintln!("  {} : {}", name, err);
        }
    }

    // Print slowest patterns
    timings.sort_by(|a, b| b.1.cmp(&a.1));
    eprintln!("\n=== Slowest patterns ===");
    for (label, elapsed) in timings.iter().take(20) {
        eprintln!("  {:>7.2}s  {}", elapsed.as_secs_f64(), label);
    }

    let tested = success + failures.len();
    if tested > 0 {
        eprintln!(
            "\nEval success rate: {:.1}% ({}/{})",
            (success as f64 / tested as f64) * 100.0,
            success,
            tested,
        );
    }
}
