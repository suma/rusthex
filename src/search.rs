//! Search functionality for the hex editor
//!
//! This module provides search capabilities including:
//! - ASCII text search
//! - Hex value search (space-separated bytes)
//! - Background search execution with cancellation
//! - Efficient O(1) result lookup using HashSet

use std::collections::HashMap;
use std::fs::File;
use std::sync::Arc;
use std::sync::atomic::Ordering;

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

impl HexEditor {
    /// Perform search in background thread
    ///
    /// Cancels any ongoing search and starts a new one.
    /// Results are asynchronously stored and can be polled via update_search_results()
    pub fn perform_search(&mut self) {
        // Cancel any ongoing search
        self.search_cancel_flag.store(true, Ordering::Relaxed);

        // Note: We don't wait for cancellation to complete here to avoid blocking UI thread.
        // The background thread will check the cancel flag periodically and stop on its own.

        self.search_results.clear();
        self.current_search_index = None;
        self.search_match_set.clear();

        if self.search_query.is_empty() {
            self.is_searching = false;
            return;
        }

        let pattern = match self.search_mode {
            SearchMode::Ascii => {
                // ASCII search: convert query string to bytes
                self.search_query.as_bytes().to_vec()
            }
            SearchMode::Hex => {
                // Hex search: parse space-separated hex values
                let hex_parts: Vec<&str> = self.search_query.split_whitespace().collect();
                let mut pattern = Vec::new();
                for part in hex_parts {
                    if let Ok(byte) = u8::from_str_radix(part, 16) {
                        pattern.push(byte);
                    } else {
                        // Invalid hex, abort search
                        self.is_searching = false;
                        return;
                    }
                }
                pattern
            }
        };

        if pattern.is_empty() {
            self.is_searching = false;
            return;
        }

        // Reset cancel flag and set searching flag
        self.search_cancel_flag.store(false, Ordering::Relaxed);
        self.is_searching = true;

        // Clear shared results
        if let Ok(mut results) = self.shared_search_results.lock() {
            *results = None;
        }

        // Set up progress tracking
        self.search_progress.store(0, Ordering::Relaxed);
        self.search_total = self.document.len();

        // Clone Arc references for the thread
        let shared_results = Arc::clone(&self.shared_search_results);
        let cancel_flag = Arc::clone(&self.search_cancel_flag);
        let progress = Arc::clone(&self.search_progress);

        // Prepare search data source (fast - no large copy on UI thread)
        let data_source = self.document.prepare_search_data();
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
            if let Some(results) = local_results {
                // Build HashSet in background thread to avoid UI freeze
                let mut match_set = std::collections::HashSet::new();
                for &match_start in &results {
                    for offset in 0..pattern_len {
                        match_set.insert(match_start + offset);
                    }
                }

                if let Ok(mut shared) = shared_results.lock() {
                    *shared = Some((results, match_set));
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
        if !self.is_searching {
            return false;
        }

        // Check if search completed
        let results_opt = if let Ok(mut shared_results) = self.shared_search_results.lock() {
            shared_results.take()
        } else {
            None
        };

        if let Some((results, match_set)) = results_opt {
            // Search completed - receive both results and pre-built match set
            self.search_results = results;
            self.search_match_set = match_set;
            self.is_searching = false;

            // Set current index to first result and scroll to it
            if !self.search_results.is_empty() {
                self.current_search_index = Some(0);
                self.scroll_to_search_result(self.search_results[0]);
            }
            return true;
        }

        false
    }

    /// Navigate to next search result
    pub fn next_search_result(&mut self) {
        if self.search_results.is_empty() {
            return;
        }

        if let Some(current_idx) = self.current_search_index {
            let next_idx = (current_idx + 1) % self.search_results.len();
            self.current_search_index = Some(next_idx);
            self.scroll_to_search_result(self.search_results[next_idx]);
        }
    }

    /// Navigate to previous search result
    pub fn prev_search_result(&mut self) {
        if self.search_results.is_empty() {
            return;
        }

        if let Some(current_idx) = self.current_search_index {
            let prev_idx = if current_idx == 0 {
                self.search_results.len() - 1
            } else {
                current_idx - 1
            };
            self.current_search_index = Some(prev_idx);
            self.scroll_to_search_result(self.search_results[prev_idx]);
        }
    }

    /// Scroll to make search result visible and update cursor
    pub fn scroll_to_search_result(&mut self, result_position: usize) {
        let result_row = result_position / self.bytes_per_row;

        // Try to center the result in the viewport
        // This provides better visibility than just making it visible
        self.scroll_handle.scroll_to_item(result_row);

        // Update cursor position to the search result
        self.cursor_position = result_position;
    }
}

/// Search in memory data (for small in-memory files)
fn search_in_memory(
    data: &[u8],
    pattern: &[u8],
    cancel_flag: &std::sync::atomic::AtomicBool,
    progress: &std::sync::atomic::AtomicUsize,
) -> Option<Vec<usize>> {
    let data_len = data.len();
    let pattern_len = pattern.len();
    let mut results = Vec::new();

    for i in 0..=data_len.saturating_sub(pattern_len) {
        // Check for cancellation and update progress every 10000 iterations
        if i % 10000 == 0 {
            if cancel_flag.load(Ordering::Relaxed) {
                return None; // Search cancelled
            }
            progress.store(i, Ordering::Relaxed);
        }

        // Check if pattern matches at this position
        let mut matches = true;
        for j in 0..pattern_len {
            if data[i + j] != pattern[j] {
                matches = false;
                break;
            }
        }
        if matches {
            results.push(i);
        }
    }

    // Mark as complete
    progress.store(data_len, Ordering::Relaxed);
    Some(results)
}

/// Search in file using memory mapping (for large files)
/// This avoids copying the entire file into memory
fn search_in_file(
    path: &std::path::PathBuf,
    overlay: &HashMap<usize, u8>,
    data_len: usize,
    pattern: &[u8],
    cancel_flag: &std::sync::atomic::AtomicBool,
    progress: &std::sync::atomic::AtomicUsize,
) -> Option<Vec<usize>> {
    // Open and memory-map the file
    let file = match File::open(path) {
        Ok(f) => f,
        Err(_) => return Some(Vec::new()), // Return empty results on error
    };

    let mmap = match unsafe { Mmap::map(&file) } {
        Ok(m) => m,
        Err(_) => return Some(Vec::new()), // Return empty results on error
    };

    let pattern_len = pattern.len();
    let mut results = Vec::new();

    for i in 0..=data_len.saturating_sub(pattern_len) {
        // Check for cancellation and update progress every 10000 iterations
        if i % 10000 == 0 {
            if cancel_flag.load(Ordering::Relaxed) {
                return None; // Search cancelled
            }
            progress.store(i, Ordering::Relaxed);
        }

        // Check if pattern matches at this position
        // Consider overlay modifications
        let mut matches = true;
        for j in 0..pattern_len {
            let pos = i + j;
            let byte = overlay.get(&pos).copied().unwrap_or_else(|| mmap[pos]);
            if byte != pattern[j] {
                matches = false;
                break;
            }
        }
        if matches {
            results.push(i);
        }
    }

    // Mark as complete
    progress.store(data_len, Ordering::Relaxed);
    Some(results)
}
