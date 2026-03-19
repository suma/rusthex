//! Render cache module for differential rendering optimization
//!
//! This module provides caching for row render data to avoid
//! recalculating byte states on every render cycle.

use crate::ui::TextEncoding;
use std::collections::{HashMap, HashSet};

/// Cached render data for a single byte
#[derive(Clone, PartialEq)]
pub struct ByteRenderData {
    pub value: u8,
    pub is_cursor: bool,
    pub is_selected: bool,
    pub is_search_match: bool,
    pub is_current_search: bool,
    pub is_modified: bool,
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
    #[cfg(test)]
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

        self.modified_offsets
            .iter()
            .any(|&offset| offset >= row_start && offset < row_end)
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
        address_chars: usize,
        get_byte: impl Fn(usize) -> Option<u8>,
        is_modified: impl Fn(usize) -> bool,
    ) -> RowRenderData {
        let row_start = row * bytes_per_row;
        let row_end = (row_start + bytes_per_row).min(document_len);
        let cursor_row = cursor_position / bytes_per_row;

        let address = crate::ui::format_address(row_start, address_chars);

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
                    is_modified: is_modified(byte_idx),
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn sample_state() -> CacheState {
        CacheState {
            cursor_position: 0,
            selection_start: None,
            selection_end: None,
            search_results_count: 0,
            current_search_index: None,
            document_len: 256,
            bytes_per_row: 16,
            text_encoding: TextEncoding::Ascii,
        }
    }

    // -- is_valid --

    #[test]
    fn new_cache_is_invalid() {
        let cache = RenderCache::new();
        assert!(!cache.is_valid(&sample_state()));
    }

    #[test]
    fn cache_valid_after_set_state() {
        let mut cache = RenderCache::new();
        let state = sample_state();
        cache.set_state(state.clone());
        assert!(cache.is_valid(&state));
    }

    #[test]
    fn cache_invalid_when_state_changes() {
        let mut cache = RenderCache::new();
        let state = sample_state();
        cache.set_state(state);

        let mut new_state = sample_state();
        new_state.cursor_position = 10;
        assert!(!cache.is_valid(&new_state));
    }

    #[test]
    fn cache_invalid_after_mark_modified() {
        let mut cache = RenderCache::new();
        let state = sample_state();
        cache.set_state(state.clone());

        cache.mark_modified(5);
        assert!(!cache.is_valid(&state));
    }

    #[test]
    fn cache_invalid_after_invalidate() {
        let mut cache = RenderCache::new();
        cache.set_state(sample_state());
        cache.invalidate();
        assert!(!cache.is_valid(&sample_state()));
    }

    // -- row_needs_rebuild --

    #[test]
    fn row_needs_rebuild_when_modified() {
        let mut cache = RenderCache::new();
        cache.mark_modified(20); // byte 20 is in row 1 (bytes_per_row=16)
        assert!(cache.row_needs_rebuild(1, 16));
    }

    #[test]
    fn row_no_rebuild_when_different_row_modified() {
        let mut cache = RenderCache::new();
        cache.mark_modified(20); // row 1
        assert!(!cache.row_needs_rebuild(0, 16)); // row 0
        assert!(!cache.row_needs_rebuild(2, 16)); // row 2
    }

    #[test]
    fn row_needs_rebuild_boundary_bytes() {
        let mut cache = RenderCache::new();
        // Row 1 = bytes 16..32
        cache.mark_modified(16); // first byte of row 1
        assert!(cache.row_needs_rebuild(1, 16));

        let mut cache2 = RenderCache::new();
        cache2.mark_modified(31); // last byte of row 1
        assert!(cache2.row_needs_rebuild(1, 16));
    }

    #[test]
    fn row_no_rebuild_when_no_modifications() {
        let cache = RenderCache::new();
        assert!(!cache.row_needs_rebuild(0, 16));
    }

    // -- get_row / cache_row --

    #[test]
    fn cache_and_retrieve_row() {
        let mut cache = RenderCache::new();
        let row = RowRenderData {
            address: "00000000".to_string(),
            bytes: vec![],
            is_cursor_row: true,
        };
        cache.cache_row(0, row);
        let retrieved = cache.get_row(0);
        assert!(retrieved.is_some());
        assert!(retrieved.unwrap().is_cursor_row);
    }

    #[test]
    fn get_row_returns_none_for_uncached() {
        let cache = RenderCache::new();
        assert!(cache.get_row(0).is_none());
    }

    #[test]
    fn invalidate_clears_rows() {
        let mut cache = RenderCache::new();
        cache.cache_row(0, RowRenderData {
            address: "00000000".to_string(),
            bytes: vec![],
            is_cursor_row: false,
        });
        cache.invalidate();
        assert!(cache.get_row(0).is_none());
    }

    // -- set_state clears modified_offsets --

    #[test]
    fn set_state_clears_modifications() {
        let mut cache = RenderCache::new();
        cache.mark_modified(5);
        let state = sample_state();
        cache.set_state(state.clone());
        // After set_state, modified_offsets should be cleared
        assert!(cache.is_valid(&state));
    }

    // -- build_row_data --

    #[test]
    fn build_row_data_basic() {
        let data: Vec<u8> = (0..32).collect();
        let search_set = HashSet::new();

        let row = RenderCache::build_row_data(
            0,      // row
            16,     // bytes_per_row
            32,     // document_len
            5,      // cursor_position
            None,   // selection_range
            &search_set,
            None,   // current_search_range
            8,      // address_chars
            |i| data.get(i).copied(),
            |_| false,
        );

        assert_eq!(row.address, "00000000");
        assert_eq!(row.bytes.len(), 16);
        assert!(row.is_cursor_row); // cursor at byte 5 -> row 0
        assert!(row.bytes[5].is_cursor);
        assert!(!row.bytes[0].is_cursor);
    }

    #[test]
    fn build_row_data_with_selection() {
        let data: Vec<u8> = (0..32).collect();
        let search_set = HashSet::new();

        let row = RenderCache::build_row_data(
            0, 16, 32, 0,
            Some((2, 5)),
            &search_set,
            None, 8,
            |i| data.get(i).copied(),
            |_| false,
        );

        assert!(!row.bytes[1].is_selected);
        assert!(row.bytes[2].is_selected);
        assert!(row.bytes[3].is_selected);
        assert!(row.bytes[5].is_selected);
        assert!(!row.bytes[6].is_selected);
    }

    #[test]
    fn build_row_data_with_search_match() {
        let data: Vec<u8> = (0..32).collect();
        let mut search_set = HashSet::new();
        search_set.insert(3);
        search_set.insert(4);

        let row = RenderCache::build_row_data(
            0, 16, 32, 0,
            None,
            &search_set,
            Some((3, 5)), // current search range
            8,
            |i| data.get(i).copied(),
            |_| false,
        );

        assert!(row.bytes[3].is_search_match);
        assert!(row.bytes[3].is_current_search);
        assert!(row.bytes[4].is_search_match);
        assert!(row.bytes[4].is_current_search);
        assert!(!row.bytes[5].is_current_search); // end is exclusive
    }

    #[test]
    fn build_row_data_second_row() {
        let data: Vec<u8> = (0..32).collect();
        let search_set = HashSet::new();

        let row = RenderCache::build_row_data(
            1, 16, 32, 20, // cursor at byte 20 -> row 1
            None, &search_set, None, 8,
            |i| data.get(i).copied(),
            |_| false,
        );

        assert_eq!(row.address, "00000010");
        assert_eq!(row.bytes.len(), 16);
        assert!(row.is_cursor_row);
        assert_eq!(row.bytes[0].value, 16); // first byte of row 1
        assert!(row.bytes[4].is_cursor); // byte 20 = row 1, col 4
    }

    #[test]
    fn build_row_data_modified_byte() {
        let data: Vec<u8> = (0..32).collect();
        let search_set = HashSet::new();

        let row = RenderCache::build_row_data(
            0, 16, 32, 0,
            None, &search_set, None, 8,
            |i| data.get(i).copied(),
            |i| i == 7, // byte 7 is modified
        );

        assert!(!row.bytes[6].is_modified);
        assert!(row.bytes[7].is_modified);
        assert!(!row.bytes[8].is_modified);
    }
}
