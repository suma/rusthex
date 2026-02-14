//! Tab management functionality for the hex editor
//!
//! This module contains methods for managing editor tabs:
//! - Creating and closing tabs
//! - Switching between tabs
//! - Reordering tabs via drag and drop

use crate::Document;
use crate::HexEditor;
use crate::pattern;
use crate::tab::EditorTab;
use std::path::PathBuf;

impl HexEditor {
    /// Create a new empty tab
    pub fn new_tab(&mut self) {
        let mut tab = EditorTab::new();
        tab.pattern.available_patterns =
            pattern::scan_hexpat_dir(&self.settings.pattern.hexpat_dir);
        self.tabs.push(tab);
        self.active_tab = self.tabs.len() - 1;
    }

    /// Close current tab
    pub fn close_tab(&mut self) -> bool {
        if self.tabs.len() <= 1 {
            return false; // Don't close the last tab
        }
        self.tabs.remove(self.active_tab);
        if self.active_tab >= self.tabs.len() {
            self.active_tab = self.tabs.len() - 1;
        }
        true
    }

    /// Switch to next tab
    pub fn next_tab(&mut self) {
        if self.tabs.len() > 1 {
            self.active_tab = (self.active_tab + 1) % self.tabs.len();
        }
    }

    /// Switch to previous tab
    pub fn prev_tab(&mut self) {
        if self.tabs.len() > 1 {
            self.active_tab = if self.active_tab == 0 {
                self.tabs.len() - 1
            } else {
                self.active_tab - 1
            };
        }
    }

    /// Switch to specific tab by index
    pub fn switch_to_tab(&mut self, index: usize) {
        if index < self.tabs.len() {
            self.active_tab = index;
        }
    }

    /// Reorder tabs by moving a tab from one index to another
    pub fn reorder_tab(&mut self, from: usize, to: usize) {
        if from >= self.tabs.len() || to >= self.tabs.len() || from == to {
            return;
        }

        let tab = self.tabs.remove(from);
        self.tabs.insert(to, tab);

        // Update active_tab to follow the moved tab if it was active
        if self.active_tab == from {
            self.active_tab = to;
        } else if from < self.active_tab && to >= self.active_tab {
            self.active_tab -= 1;
        } else if from > self.active_tab && to <= self.active_tab {
            self.active_tab += 1;
        }
    }

    /// Open a file in a new tab
    pub fn open_file_in_new_tab(&mut self, path: PathBuf) -> std::io::Result<()> {
        let mut doc = Document::new();
        doc.load(path)?;
        let mut tab = EditorTab::with_document(doc);
        tab.pattern.available_patterns =
            pattern::scan_hexpat_dir(&self.settings.pattern.hexpat_dir);
        self.tabs.push(tab);
        self.active_tab = self.tabs.len() - 1;
        Ok(())
    }
}
