//! Render cache module for differential rendering optimization
//!
//! This module provides caching for row render data to avoid
//! recalculating byte states on every render cycle.

use std::collections::{HashMap, HashSet};
use crate::ui::TextEncoding;

/// Cached render data for a single byte
#[derive(Clone, PartialEq)]
pub struct ByteRenderData {
    pub value: u8,
    pub is_cursor: bool,
    pub is_selected: bool,
    pub is_search_match: bool,
    pub is_current_search: bool,
}

/// Cached render data for a single row
#[derive(Clone)]
pub struct RowRenderData {
    pub address: String,
    pub bytes: Vec<ByteRenderData>,
    pub is_cursor_row: bool,
}

/// Cache state tracking for invalidation
#[derive(Clone, PartialEq)]
pub struct CacheState {
    pub cursor_position: usize,
    pub selection_start: Option<usize>,
    pub selection_end: Option<usize>,
    pub search_results_count: usize,
    pub current_search_index: Option<usize>,
    pub document_len: usize,
    pub bytes_per_row: usize,
    pub text_encoding: TextEncoding,
}

/// Row render cache
pub struct RenderCache {
    /// Cached row data, keyed by row index
    rows: HashMap<usize, RowRenderData>,
    /// State at the time cache was built
    state: Option<CacheState>,
    /// Set of modified byte offsets (for partial invalidation)
    modified_offsets: HashSet<usize>,
}

impl RenderCache {
    pub fn new() -> Self {
        Self {
            rows: HashMap::new(),
            state: None,
            modified_offsets: HashSet::new(),
        }
    }

    /// Check if cache is valid for the given state
    pub fn is_valid(&self, new_state: &CacheState) -> bool {
        match &self.state {
            Some(old_state) => old_state == new_state && self.modified_offsets.is_empty(),
            None => false,
        }
    }

    /// Mark a byte offset as modified (needs re-render)
    pub fn mark_modified(&mut self, offset: usize) {
        self.modified_offsets.insert(offset);
    }

    /// Clear the cache
    pub fn invalidate(&mut self) {
        self.rows.clear();
        self.state = None;
        self.modified_offsets.clear();
    }

    /// Get cached row data, or None if not cached
    pub fn get_row(&self, row: usize) -> Option<&RowRenderData> {
        self.rows.get(&row)
    }

    /// Store row data in cache
    pub fn cache_row(&mut self, row: usize, data: RowRenderData) {
        self.rows.insert(row, data);
    }

    /// Update cache state after building
    pub fn set_state(&mut self, state: CacheState) {
        self.state = Some(state);
        self.modified_offsets.clear();
    }

    /// Check if a specific row needs rebuild due to modifications
    pub fn row_needs_rebuild(&self, row: usize, bytes_per_row: usize) -> bool {
        let row_start = row * bytes_per_row;
        let row_end = row_start + bytes_per_row;

        self.modified_offsets.iter().any(|&offset| {
            offset >= row_start && offset < row_end
        })
    }

    /// Build row render data
    pub fn build_row_data(
        row: usize,
        bytes_per_row: usize,
        document_len: usize,
        cursor_position: usize,
        selection_range: Option<(usize, usize)>,
        search_match_set: &HashSet<usize>,
        current_search_range: Option<(usize, usize)>,
        get_byte: impl Fn(usize) -> Option<u8>,
    ) -> RowRenderData {
        let row_start = row * bytes_per_row;
        let row_end = (row_start + bytes_per_row).min(document_len);
        let cursor_row = cursor_position / bytes_per_row;

        let address = format!("{:08X}", row_start);

        let bytes: Vec<ByteRenderData> = (row_start..row_end)
            .map(|byte_idx| {
                let value = get_byte(byte_idx).unwrap_or(0);
                let is_cursor = byte_idx == cursor_position;
                let is_selected = selection_range
                    .map(|(start, end)| byte_idx >= start && byte_idx <= end)
                    .unwrap_or(false);
                let is_search_match = search_match_set.contains(&byte_idx);
                let is_current_search = current_search_range
                    .map(|(start, end)| byte_idx >= start && byte_idx < end)
                    .unwrap_or(false);

                ByteRenderData {
                    value,
                    is_cursor,
                    is_selected,
                    is_search_match,
                    is_current_search,
                }
            })
            .collect();

        RowRenderData {
            address,
            bytes,
            is_cursor_row: row == cursor_row,
        }
    }
}

impl Default for RenderCache {
    fn default() -> Self {
        Self::new()
    }
}
