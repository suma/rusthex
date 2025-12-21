//! Search functionality for the hex editor
//!
//! This module provides search capabilities including:
//! - ASCII text search
//! - Hex value search (space-separated bytes)
//! - Background search execution with cancellation
//! - Efficient O(1) result lookup using HashSet

use std::sync::Arc;
use std::sync::atomic::Ordering;

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
    /// Build HashSet of all byte positions that are part of search matches
    /// This enables O(1) lookup during rendering instead of O(n) iteration
    pub fn build_search_match_set(&mut self) {
        self.search_match_set.clear();

        let pattern_len = match self.search_mode {
            SearchMode::Ascii => self.search_query.len(),
            SearchMode::Hex => self.search_query.split_whitespace().count(),
        };

        for &match_start in &self.search_results {
            for offset in 0..pattern_len {
                self.search_match_set.insert(match_start + offset);
            }
        }
    }

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

        // Clone Arc references for the thread
        let shared_results = Arc::clone(&self.shared_search_results);
        let cancel_flag = Arc::clone(&self.search_cancel_flag);

        // Efficiently copy document data using optimized to_vec() method
        // This is fast even for large files (bulk memory copy for mmap)
        let data = self.document.to_vec();

        // Spawn background search thread
        std::thread::spawn(move || {
            let data_len = data.len();
            let pattern_len = pattern.len();
            let mut local_results = Vec::new();

            for i in 0..=data_len.saturating_sub(pattern_len) {
                // Check for cancellation every 1000 iterations
                if i % 1000 == 0 && cancel_flag.load(Ordering::Relaxed) {
                    return; // Search cancelled
                }

                let mut matches = true;
                for j in 0..pattern_len {
                    if data[i + j] != pattern[j] {
                        matches = false;
                        break;
                    }
                }
                if matches {
                    local_results.push(i);
                }
            }

            // Store results
            if let Ok(mut results) = shared_results.lock() {
                *results = Some(local_results);
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

        if let Some(results) = results_opt {
            // Search completed
            self.search_results = results;
            self.is_searching = false;

            // Build HashSet for efficient O(1) lookup during rendering
            self.build_search_match_set();

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
