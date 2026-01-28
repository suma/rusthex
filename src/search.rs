//! Search functionality for the hex editor
//!
//! This module provides search capabilities including:
//! - ASCII text search
//! - Hex value search (space-separated bytes)
//! - Wildcard support (`??` matches any byte)
//! - Background search execution with cancellation
//! - Efficient O(1) result lookup using HashSet
//! - SIMD-accelerated search using memchr for exact patterns

use std::collections::HashMap;
use std::fs::File;
use std::sync::Arc;
use std::sync::atomic::Ordering;

use memchr::memmem;
use memmap2::Mmap;

use crate::document::SearchDataSource;
use crate::HexEditor;

/// Search mode selection
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SearchMode {
    /// Search for ASCII text
    Ascii,
    /// Search for hex values (space-separated bytes)
    Hex,
}

/// A single byte in a search pattern
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PatternByte {
    /// Match an exact byte value
    Exact(u8),
    /// Match any byte (wildcard `??`)
    Any,
}

impl HexEditor {
    /// Perform search in background thread
    ///
    /// Cancels any ongoing search and starts a new one.
    /// Results are asynchronously stored and can be polled via update_search_results()
    pub fn perform_search(&mut self) {
        // Cancel any ongoing search
        self.tab().search_cancel_flag.store(true, Ordering::Relaxed);

        // Note: We don't wait for cancellation to complete here to avoid blocking UI thread.
        // The background thread will check the cancel flag periodically and stop on its own.

        self.tab_mut().search_results.clear();
        self.tab_mut().current_search_index = None;
        self.tab_mut().search_match_set.clear();
        self.tab_mut().search_truncated = false;

        if self.tab().search_query.is_empty() {
            self.tab_mut().is_searching = false;
            return;
        }

        let pattern: Vec<PatternByte> = match self.tab().search_mode {
            SearchMode::Ascii => {
                // ASCII search: convert query string to exact bytes
                self.tab().search_query.as_bytes().iter()
                    .map(|&b| PatternByte::Exact(b))
                    .collect()
            }
            SearchMode::Hex => {
                // Hex search: parse space-separated hex values with wildcard support
                let hex_parts: Vec<&str> = self.tab().search_query.split_whitespace().collect();
                let mut pattern = Vec::new();
                for part in hex_parts {
                    if part == "??" || part == "?" {
                        // Wildcard: matches any byte
                        pattern.push(PatternByte::Any);
                    } else if let Ok(byte) = u8::from_str_radix(part, 16) {
                        pattern.push(PatternByte::Exact(byte));
                    } else {
                        // Invalid hex, abort search
                        self.tab_mut().is_searching = false;
                        return;
                    }
                }
                pattern
            }
        };

        if pattern.is_empty() {
            self.tab_mut().is_searching = false;
            return;
        }

        // Reset cancel flag and set searching flag
        self.tab().search_cancel_flag.store(false, Ordering::Relaxed);
        self.tab_mut().is_searching = true;

        // Clear shared results
        if let Ok(mut results) = self.tab().shared_search_results.lock() {
            *results = None;
        }

        // Set up progress tracking
        self.tab().search_progress.store(0, Ordering::Relaxed);
        self.tab_mut().search_total = self.tab().document.len();

        // Clone Arc references for the thread
        let shared_results = Arc::clone(&self.tab().shared_search_results);
        let cancel_flag = Arc::clone(&self.tab().search_cancel_flag);
        let progress = Arc::clone(&self.tab().search_progress);

        // Prepare search data source (fast - no large copy on UI thread)
        let data_source = self.tab().document.prepare_search_data();
        let pattern_len = pattern.len();

        // Spawn background search thread
        std::thread::spawn(move || {
            // Execute search based on data source type
            let local_results = match data_source {
                SearchDataSource::InMemory(data) => {
                    search_in_memory(&data, &pattern, &cancel_flag, &progress)
                }
                SearchDataSource::FilePath { path, overlay, len } => {
                    search_in_file(&path, &overlay, len, &pattern, &cancel_flag, &progress)
                }
            };

            // Build match set and store results if search wasn't cancelled
            if let Some(mut results) = local_results {
                // Limit results to prevent UI freeze during HashSet construction
                const MAX_RESULTS: usize = 10_000;
                let truncated = results.len() > MAX_RESULTS;
                if truncated {
                    results.truncate(MAX_RESULTS);
                }

                // Build HashSet in background thread
                // Only build if we have a reasonable number of results
                let match_set = if results.len() <= MAX_RESULTS {
                    let mut set = std::collections::HashSet::new();
                    for &match_start in &results {
                        for offset in 0..pattern_len {
                            set.insert(match_start + offset);
                        }
                    }
                    set
                } else {
                    std::collections::HashSet::new() // Empty set for too many results
                };

                if let Ok(mut shared) = shared_results.lock() {
                    *shared = Some((results, match_set, truncated));
                }
            }
        });

        // Poll for results (simple approach - check on next render)
        // Results will be picked up in update_search_results()
    }

    /// Poll for search results from background thread
    ///
    /// Returns true if search completed this frame
    pub fn update_search_results(&mut self) -> bool {
        if !self.tab().is_searching {
            return false;
        }

        // Check if search completed
        let results_opt = if let Ok(mut shared_results) = self.tab().shared_search_results.lock() {
            shared_results.take()
        } else {
            None
        };

        if let Some((results, match_set, truncated)) = results_opt {
            // Search completed - receive results, match set, and truncated flag
            self.tab_mut().search_results = results;
            self.tab_mut().search_match_set = match_set;
            self.tab_mut().search_truncated = truncated;
            self.tab_mut().is_searching = false;

            // Set current index to first result and scroll to it
            if !self.tab().search_results.is_empty() {
                self.tab_mut().current_search_index = Some(0);
                let first_result = self.tab().search_results[0];
                self.scroll_to_search_result(first_result);
            }
            return true;
        }

        false
    }

    /// Navigate to next search result
    pub fn next_search_result(&mut self) {
        if self.tab().search_results.is_empty() {
            return;
        }

        if let Some(current_idx) = self.tab().current_search_index {
            let next_idx = (current_idx + 1) % self.tab().search_results.len();
            self.tab_mut().current_search_index = Some(next_idx);
            let result_pos = self.tab().search_results[next_idx];
            self.scroll_to_search_result(result_pos);
        }
    }

    /// Navigate to previous search result
    pub fn prev_search_result(&mut self) {
        if self.tab().search_results.is_empty() {
            return;
        }

        if let Some(current_idx) = self.tab().current_search_index {
            let prev_idx = if current_idx == 0 {
                self.tab().search_results.len() - 1
            } else {
                current_idx - 1
            };
            self.tab_mut().current_search_index = Some(prev_idx);
            let result_pos = self.tab().search_results[prev_idx];
            self.scroll_to_search_result(result_pos);
        }
    }

    /// Scroll to make search result visible and update cursor
    pub fn scroll_to_search_result(&mut self, result_position: usize) {
        // Update cursor position to the search result
        self.tab_mut().cursor_position = result_position;

        // Use ensure_cursor_visible_by_row() for proper virtual scroll handling
        self.ensure_cursor_visible_by_row();
    }
}

/// Extract leading exact bytes from pattern for SIMD pre-filtering
fn extract_exact_prefix(pattern: &[PatternByte]) -> Vec<u8> {
    pattern
        .iter()
        .take_while(|p| matches!(p, PatternByte::Exact(_)))
        .map(|p| match p {
            PatternByte::Exact(b) => *b,
            _ => unreachable!(),
        })
        .collect()
}

/// Check if pattern is all exact bytes (no wildcards)
fn is_all_exact(pattern: &[PatternByte]) -> bool {
    pattern.iter().all(|p| matches!(p, PatternByte::Exact(_)))
}

/// Convert all-exact pattern to byte slice
fn pattern_to_bytes(pattern: &[PatternByte]) -> Vec<u8> {
    pattern
        .iter()
        .map(|p| match p {
            PatternByte::Exact(b) => *b,
            PatternByte::Any => unreachable!(),
        })
        .collect()
}

/// Verify full pattern match at given position (for wildcard patterns)
fn verify_pattern_at(data: &[u8], pos: usize, pattern: &[PatternByte]) -> bool {
    if pos + pattern.len() > data.len() {
        return false;
    }
    for (j, pat_byte) in pattern.iter().enumerate() {
        if let PatternByte::Exact(expected) = pat_byte {
            if data[pos + j] != *expected {
                return false;
            }
        }
        // PatternByte::Any always matches
    }
    true
}

/// Search in memory data (for small in-memory files)
/// Uses SIMD-accelerated search via memchr when possible
fn search_in_memory(
    data: &[u8],
    pattern: &[PatternByte],
    cancel_flag: &std::sync::atomic::AtomicBool,
    progress: &std::sync::atomic::AtomicUsize,
) -> Option<Vec<usize>> {
    let data_len = data.len();
    let pattern_len = pattern.len();

    if pattern_len == 0 || data_len < pattern_len {
        progress.store(data_len, Ordering::Relaxed);
        return Some(Vec::new());
    }

    // Fast path: all exact bytes - use SIMD search directly
    if is_all_exact(pattern) {
        let needle = pattern_to_bytes(pattern);
        let finder = memmem::Finder::new(&needle);
        let mut results = Vec::new();

        for pos in finder.find_iter(data) {
            // Check cancellation periodically
            if results.len() % 1000 == 0 {
                if cancel_flag.load(Ordering::Relaxed) {
                    return None;
                }
                progress.store(pos, Ordering::Relaxed);
            }
            results.push(pos);
        }

        progress.store(data_len, Ordering::Relaxed);
        return Some(results);
    }

    // Pattern contains wildcards - use prefix for pre-filtering
    let prefix = extract_exact_prefix(pattern);

    if prefix.len() >= 2 {
        // Use SIMD to find prefix candidates, then verify full pattern
        let finder = memmem::Finder::new(&prefix);
        let mut results = Vec::new();

        for pos in finder.find_iter(data) {
            if results.len() % 1000 == 0 {
                if cancel_flag.load(Ordering::Relaxed) {
                    return None;
                }
                progress.store(pos, Ordering::Relaxed);
            }

            if verify_pattern_at(data, pos, pattern) {
                results.push(pos);
            }
        }

        progress.store(data_len, Ordering::Relaxed);
        return Some(results);
    }

    // Fallback: naive search for patterns with short/no prefix
    let mut results = Vec::new();
    for i in 0..=data_len.saturating_sub(pattern_len) {
        if i % 10000 == 0 {
            if cancel_flag.load(Ordering::Relaxed) {
                return None;
            }
            progress.store(i, Ordering::Relaxed);
        }

        if verify_pattern_at(data, i, pattern) {
            results.push(i);
        }
    }

    progress.store(data_len, Ordering::Relaxed);
    Some(results)
}

/// Verify pattern match at position considering overlay modifications
fn verify_pattern_at_with_overlay(
    mmap: &[u8],
    overlay: &HashMap<usize, u8>,
    pos: usize,
    pattern: &[PatternByte],
    data_len: usize,
) -> bool {
    if pos + pattern.len() > data_len {
        return false;
    }
    for (j, pat_byte) in pattern.iter().enumerate() {
        let byte_pos = pos + j;
        let byte = overlay.get(&byte_pos).copied().unwrap_or_else(|| mmap[byte_pos]);
        if let PatternByte::Exact(expected) = pat_byte {
            if byte != *expected {
                return false;
            }
        }
    }
    true
}

/// Check if any position in range overlaps with overlay
fn has_overlay_in_range(overlay: &HashMap<usize, u8>, start: usize, len: usize) -> bool {
    for offset in 0..len {
        if overlay.contains_key(&(start + offset)) {
            return true;
        }
    }
    false
}

/// Search in file using memory mapping (for large files)
/// Uses SIMD-accelerated search via memchr when possible
fn search_in_file(
    path: &std::path::PathBuf,
    overlay: &HashMap<usize, u8>,
    data_len: usize,
    pattern: &[PatternByte],
    cancel_flag: &std::sync::atomic::AtomicBool,
    progress: &std::sync::atomic::AtomicUsize,
) -> Option<Vec<usize>> {
    let file = match File::open(path) {
        Ok(f) => f,
        Err(_) => return Some(Vec::new()),
    };

    let mmap = match unsafe { Mmap::map(&file) } {
        Ok(m) => m,
        Err(_) => return Some(Vec::new()),
    };

    let pattern_len = pattern.len();

    if pattern_len == 0 || data_len < pattern_len {
        progress.store(data_len, Ordering::Relaxed);
        return Some(Vec::new());
    }

    // Fast path: all exact bytes and no overlay - use SIMD search directly
    if is_all_exact(pattern) && overlay.is_empty() {
        let needle = pattern_to_bytes(pattern);
        let finder = memmem::Finder::new(&needle);
        let mut results = Vec::new();

        for pos in finder.find_iter(&mmap[..data_len]) {
            if results.len() % 1000 == 0 {
                if cancel_flag.load(Ordering::Relaxed) {
                    return None;
                }
                progress.store(pos, Ordering::Relaxed);
            }
            results.push(pos);
        }

        progress.store(data_len, Ordering::Relaxed);
        return Some(results);
    }

    // With overlay or wildcards: use SIMD for candidates, then verify with overlay
    if is_all_exact(pattern) || extract_exact_prefix(pattern).len() >= 2 {
        let search_bytes = if is_all_exact(pattern) {
            pattern_to_bytes(pattern)
        } else {
            extract_exact_prefix(pattern)
        };

        let finder = memmem::Finder::new(&search_bytes);
        let mut results = Vec::new();

        for pos in finder.find_iter(&mmap[..data_len]) {
            if results.len() % 1000 == 0 {
                if cancel_flag.load(Ordering::Relaxed) {
                    return None;
                }
                progress.store(pos, Ordering::Relaxed);
            }

            // Re-verify considering overlay
            if has_overlay_in_range(overlay, pos, pattern_len) {
                // Has overlay modifications - verify with overlay
                if verify_pattern_at_with_overlay(&mmap, overlay, pos, pattern, data_len) {
                    results.push(pos);
                }
            } else if is_all_exact(pattern) {
                // No overlay in range and all exact - already matched
                results.push(pos);
            } else {
                // No overlay but has wildcards - verify pattern
                if verify_pattern_at(&mmap[..data_len], pos, pattern) {
                    results.push(pos);
                }
            }
        }

        // Also search in overlay regions that might match but weren't found in mmap
        if !overlay.is_empty() {
            let overlay_results =
                search_overlay_regions(&mmap, overlay, data_len, pattern, cancel_flag, progress)?;
            for pos in overlay_results {
                if !results.contains(&pos) {
                    results.push(pos);
                }
            }
            results.sort_unstable();
        }

        progress.store(data_len, Ordering::Relaxed);
        return Some(results);
    }

    // Fallback: naive search
    let mut results = Vec::new();
    for i in 0..=data_len.saturating_sub(pattern_len) {
        if i % 10000 == 0 {
            if cancel_flag.load(Ordering::Relaxed) {
                return None;
            }
            progress.store(i, Ordering::Relaxed);
        }

        if verify_pattern_at_with_overlay(&mmap, overlay, i, pattern, data_len) {
            results.push(i);
        }
    }

    progress.store(data_len, Ordering::Relaxed);
    Some(results)
}

/// Search regions affected by overlay modifications
fn search_overlay_regions(
    mmap: &[u8],
    overlay: &HashMap<usize, u8>,
    data_len: usize,
    pattern: &[PatternByte],
    cancel_flag: &std::sync::atomic::AtomicBool,
    _progress: &std::sync::atomic::AtomicUsize,
) -> Option<Vec<usize>> {
    let pattern_len = pattern.len();
    let mut results = Vec::new();

    // For each overlay position, check if pattern could match starting near it
    for &overlay_pos in overlay.keys() {
        if cancel_flag.load(Ordering::Relaxed) {
            return None;
        }

        // Check positions where this overlay byte could be part of a match
        let start = overlay_pos.saturating_sub(pattern_len - 1);
        let end = (overlay_pos + 1).min(data_len.saturating_sub(pattern_len - 1));

        for pos in start..=end {
            if pos + pattern_len <= data_len
                && verify_pattern_at_with_overlay(mmap, overlay, pos, pattern, data_len)
            {
                results.push(pos);
            }
        }
    }

    results.sort_unstable();
    results.dedup();
    Some(results)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::AtomicUsize;

    // Helper to create Exact pattern from bytes
    fn exact(bytes: &[u8]) -> Vec<PatternByte> {
        bytes.iter().map(|&b| PatternByte::Exact(b)).collect()
    }

    // Helper to create pattern with wildcards
    fn pattern(spec: &[Option<u8>]) -> Vec<PatternByte> {
        spec.iter()
            .map(|opt| match opt {
                Some(b) => PatternByte::Exact(*b),
                None => PatternByte::Any,
            })
            .collect()
    }

    // =========================================
    // Tests for extract_exact_prefix
    // =========================================

    #[test]
    fn test_extract_exact_prefix_all_exact() {
        let pat = exact(b"hello");
        assert_eq!(extract_exact_prefix(&pat), b"hello");
    }

    #[test]
    fn test_extract_exact_prefix_with_wildcard() {
        let pat = pattern(&[Some(0x41), Some(0x42), None, Some(0x44)]);
        assert_eq!(extract_exact_prefix(&pat), vec![0x41, 0x42]);
    }

    #[test]
    fn test_extract_exact_prefix_starts_with_wildcard() {
        let pat = pattern(&[None, Some(0x41), Some(0x42)]);
        assert_eq!(extract_exact_prefix(&pat), Vec::<u8>::new());
    }

    #[test]
    fn test_extract_exact_prefix_empty() {
        let pat: Vec<PatternByte> = vec![];
        assert_eq!(extract_exact_prefix(&pat), Vec::<u8>::new());
    }

    // =========================================
    // Tests for is_all_exact
    // =========================================

    #[test]
    fn test_is_all_exact_true() {
        let pat = exact(b"test");
        assert!(is_all_exact(&pat));
    }

    #[test]
    fn test_is_all_exact_false() {
        let pat = pattern(&[Some(0x41), None, Some(0x42)]);
        assert!(!is_all_exact(&pat));
    }

    #[test]
    fn test_is_all_exact_empty() {
        let pat: Vec<PatternByte> = vec![];
        assert!(is_all_exact(&pat));
    }

    // =========================================
    // Tests for pattern_to_bytes
    // =========================================

    #[test]
    fn test_pattern_to_bytes() {
        let pat = exact(b"\x00\x01\x02\xff");
        assert_eq!(pattern_to_bytes(&pat), vec![0x00, 0x01, 0x02, 0xff]);
    }

    // =========================================
    // Tests for verify_pattern_at
    // =========================================

    #[test]
    fn test_verify_pattern_at_exact_match() {
        let data = b"hello world";
        let pat = exact(b"world");
        assert!(verify_pattern_at(data, 6, &pat));
    }

    #[test]
    fn test_verify_pattern_at_no_match() {
        let data = b"hello world";
        let pat = exact(b"World"); // Case sensitive
        assert!(!verify_pattern_at(data, 6, &pat));
    }

    #[test]
    fn test_verify_pattern_at_with_wildcard() {
        let data = b"hello world";
        // "w??ld" should match "world"
        let pat = pattern(&[Some(b'w'), None, None, Some(b'l'), Some(b'd')]);
        assert!(verify_pattern_at(data, 6, &pat));
    }

    #[test]
    fn test_verify_pattern_at_out_of_bounds() {
        let data = b"hello";
        let pat = exact(b"hello world");
        assert!(!verify_pattern_at(data, 0, &pat));
    }

    #[test]
    fn test_verify_pattern_at_boundary() {
        let data = b"test";
        let pat = exact(b"st");
        assert!(verify_pattern_at(data, 2, &pat));
        assert!(!verify_pattern_at(data, 3, &pat)); // Would overflow
    }

    // =========================================
    // Tests for has_overlay_in_range
    // =========================================

    #[test]
    fn test_has_overlay_in_range_yes() {
        let mut overlay = HashMap::new();
        overlay.insert(5, 0x00);
        assert!(has_overlay_in_range(&overlay, 3, 4)); // Range 3..7 includes 5
    }

    #[test]
    fn test_has_overlay_in_range_no() {
        let mut overlay = HashMap::new();
        overlay.insert(10, 0x00);
        assert!(!has_overlay_in_range(&overlay, 0, 5)); // Range 0..5 doesn't include 10
    }

    #[test]
    fn test_has_overlay_in_range_empty() {
        let overlay = HashMap::new();
        assert!(!has_overlay_in_range(&overlay, 0, 100));
    }

    // =========================================
    // Tests for verify_pattern_at_with_overlay
    // =========================================

    #[test]
    fn test_verify_pattern_with_overlay_match() {
        let data = b"hello world";
        let mut overlay = HashMap::new();
        overlay.insert(6, b'W'); // Change 'w' to 'W'
        let pat = exact(b"World");
        assert!(verify_pattern_at_with_overlay(data, &overlay, 6, &pat, data.len()));
    }

    #[test]
    fn test_verify_pattern_with_overlay_no_match() {
        let data = b"hello world";
        let mut overlay = HashMap::new();
        overlay.insert(6, b'X');
        let pat = exact(b"world");
        assert!(!verify_pattern_at_with_overlay(data, &overlay, 6, &pat, data.len()));
    }

    // =========================================
    // Tests for search_in_memory
    // =========================================

    fn make_flags() -> (AtomicBool, AtomicUsize) {
        (AtomicBool::new(false), AtomicUsize::new(0))
    }

    #[test]
    fn test_search_in_memory_exact_single() {
        let data = b"hello world hello";
        let pat = exact(b"hello");
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 12]);
    }

    #[test]
    fn test_search_in_memory_exact_no_match() {
        let data = b"hello world";
        let pat = exact(b"xyz");
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_in_memory_with_wildcard() {
        let data = b"cat bat rat mat";
        // "?at" should match all four
        let pat = pattern(&[None, Some(b'a'), Some(b't')]);
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 4, 8, 12]);
    }

    #[test]
    fn test_search_in_memory_wildcard_in_middle() {
        let data = b"abcXdef abcYdef abcZdef";
        // "abc?def"
        let pat = pattern(&[
            Some(b'a'),
            Some(b'b'),
            Some(b'c'),
            None,
            Some(b'd'),
            Some(b'e'),
            Some(b'f'),
        ]);
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 8, 16]);
    }

    #[test]
    fn test_search_in_memory_empty_pattern() {
        let data = b"hello";
        let pat: Vec<PatternByte> = vec![];
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_in_memory_pattern_longer_than_data() {
        let data = b"hi";
        let pat = exact(b"hello world");
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_in_memory_binary_data() {
        let data: &[u8] = &[0x00, 0x01, 0x02, 0x03, 0x00, 0x01, 0x02, 0x04];
        let pat = exact(&[0x00, 0x01, 0x02]);
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 4]);
    }

    #[test]
    fn test_search_in_memory_overlapping_matches() {
        // Note: memchr::memmem returns non-overlapping matches
        // "aaaa" with pattern "aa" finds matches at [0, 2] (not [0, 1, 2])
        let data = b"aaaa";
        let pat = exact(b"aa");
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 2]);
    }

    #[test]
    fn test_search_in_memory_cancellation() {
        let data = b"hello world hello world hello";
        let pat = exact(b"hello");
        let cancel = AtomicBool::new(true); // Pre-cancelled
        let progress = AtomicUsize::new(0);

        let results = search_in_memory(data, &pat, &cancel, &progress);
        assert!(results.is_none());
    }

    #[test]
    fn test_search_in_memory_all_wildcards() {
        let data = b"abcd";
        // "????" - matches every position where 4 bytes are available
        let pat = pattern(&[None, None, None, None]);
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0]);
    }

    #[test]
    fn test_search_in_memory_wildcard_at_end() {
        let data = b"test1 test2 test3";
        // "test?"
        let pat = pattern(&[
            Some(b't'),
            Some(b'e'),
            Some(b's'),
            Some(b't'),
            None,
        ]);
        let (cancel, progress) = make_flags();

        let results = search_in_memory(data, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 6, 12]);
    }

    // =========================================
    // Tests for search_in_file (using temp files)
    // =========================================

    #[test]
    fn test_search_in_file_exact() {
        use std::io::Write;
        let mut temp = tempfile::NamedTempFile::new().unwrap();
        temp.write_all(b"hello world hello").unwrap();
        temp.flush().unwrap();

        let path = temp.path().to_path_buf();
        let overlay = HashMap::new();
        let pat = exact(b"hello");
        let (cancel, progress) = make_flags();

        let results = search_in_file(&path, &overlay, 17, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 12]);
    }

    #[test]
    fn test_search_in_file_with_overlay_creates_match() {
        use std::io::Write;
        let mut temp = tempfile::NamedTempFile::new().unwrap();
        temp.write_all(b"hallo world").unwrap();
        temp.flush().unwrap();

        let path = temp.path().to_path_buf();
        let mut overlay = HashMap::new();
        overlay.insert(1, b'e'); // Change 'a' to 'e' -> "hello"
        let pat = exact(b"hello");
        let (cancel, progress) = make_flags();

        let results = search_in_file(&path, &overlay, 11, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0]);
    }

    #[test]
    fn test_search_in_file_overlay_removes_match() {
        use std::io::Write;
        let mut temp = tempfile::NamedTempFile::new().unwrap();
        temp.write_all(b"hello world").unwrap();
        temp.flush().unwrap();

        let path = temp.path().to_path_buf();
        let mut overlay = HashMap::new();
        overlay.insert(0, b'X'); // Change 'h' to 'X' -> "Xello"
        let pat = exact(b"hello");
        let (cancel, progress) = make_flags();

        let results = search_in_file(&path, &overlay, 11, &pat, &cancel, &progress).unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_in_file_with_wildcard() {
        use std::io::Write;
        let mut temp = tempfile::NamedTempFile::new().unwrap();
        temp.write_all(b"cat bat rat").unwrap();
        temp.flush().unwrap();

        let path = temp.path().to_path_buf();
        let overlay = HashMap::new();
        let pat = pattern(&[None, Some(b'a'), Some(b't')]);
        let (cancel, progress) = make_flags();

        let results = search_in_file(&path, &overlay, 11, &pat, &cancel, &progress).unwrap();
        assert_eq!(results, vec![0, 4, 8]);
    }

    #[test]
    fn test_search_in_file_nonexistent() {
        let path = std::path::PathBuf::from("/nonexistent/file/path");
        let overlay = HashMap::new();
        let pat = exact(b"test");
        let (cancel, progress) = make_flags();

        let results = search_in_file(&path, &overlay, 100, &pat, &cancel, &progress).unwrap();
        assert!(results.is_empty());
    }
}
