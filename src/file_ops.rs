//! File operation functionality for the hex editor
//!
//! This module contains methods for file I/O operations:
//! - Loading and saving files
//! - File dialogs (open, save as)
//! - File drop handling
//! - Unsaved changes management

use gpui::{Context, ExternalPaths, PathPromptOptions, PromptLevel, SharedString, Window};
use std::path::PathBuf;

use crate::HexEditor;

impl HexEditor {
    /// Load file from path
    pub fn load_file(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.tab_mut().document.load(path)?;
        self.tab_mut().cursor_position = 0;
        self.tab_mut().selection_start = None;
        Ok(())
    }

    /// Save file to current path (internal, no confirmation)
    pub fn save_file(&mut self) -> std::io::Result<()> {
        self.tab_mut().document.save()?;
        if let Some(path) = self.tab().document.file_path() {
            self.save_message = Some(format!("Saved to {}", path.display()));
        }
        Ok(())
    }

    /// Save file to a new path
    pub fn save_file_as(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.tab_mut().document.save_as(path.clone())?;
        self.save_message = Some(format!("Saved to {}", path.display()));
        Ok(())
    }

    /// Save file with confirmation dialog
    pub fn save_with_confirmation(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        // Check if there are unsaved changes
        if !self.tab().document.has_unsaved_changes() {
            self.save_message = Some("No changes to save".to_string());
            cx.notify();
            return;
        }

        // Get file path for the dialog message
        let file_name = self
            .tab()
            .document
            .file_name()
            .unwrap_or("file")
            .to_string();

        let receiver = window.prompt(
            PromptLevel::Warning,
            "Save File",
            Some(&format!("Do you want to save changes to '{}'?", file_name)),
            &["Save", "Cancel"],
            cx,
        );

        cx.spawn_in(window, async move |entity, cx| {
            if let Ok(answer) = receiver.await {
                if answer == 0 {
                    // User clicked "Save"
                    let _ = entity.update(cx, |editor, cx| {
                        match editor.save_file() {
                            Ok(_) => {
                                eprintln!("File saved successfully");
                            }
                            Err(e) => {
                                eprintln!("Failed to save file: {}", e);
                                editor.save_message = Some(format!("Error: {}", e));
                            }
                        }
                        cx.notify();
                    });
                }
            }
        })
        .detach();
    }

    /// Check if any tab has unsaved changes
    pub fn has_any_unsaved_changes(&self) -> bool {
        self.tabs
            .iter()
            .any(|tab| tab.document.has_unsaved_changes())
    }

    /// Get list of files with unsaved changes
    pub fn get_unsaved_file_names(&self) -> Vec<String> {
        self.tabs
            .iter()
            .filter(|tab| tab.document.has_unsaved_changes())
            .map(|tab| tab.document.file_name().unwrap_or("Untitled").to_string())
            .collect()
    }

    /// Show confirmation dialog before closing with unsaved changes
    pub fn confirm_close_with_unsaved(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        let unsaved_files = self.get_unsaved_file_names();
        if unsaved_files.is_empty() {
            // No unsaved changes, just quit
            self.force_close = true;
            cx.quit();
            return;
        }

        let message = if unsaved_files.len() == 1 {
            format!("'{}' has unsaved changes.", unsaved_files[0])
        } else {
            format!(
                "{} files have unsaved changes:\n{}",
                unsaved_files.len(),
                unsaved_files.join(", ")
            )
        };

        let receiver = window.prompt(
            PromptLevel::Warning,
            "Unsaved Changes",
            Some(&format!(
                "{}\n\nDo you want to discard changes and quit?",
                message
            )),
            &["Discard and Quit", "Cancel"],
            cx,
        );

        cx.spawn_in(window, async move |entity, cx| {
            if let Ok(answer) = receiver.await {
                if answer == 0 {
                    // User clicked "Discard and Quit"
                    let _ = entity.update(cx, |editor, cx| {
                        editor.force_close = true;
                        cx.quit();
                    });
                }
                // If Cancel (answer == 1), do nothing - window stays open
            }
        })
        .detach();
    }

    /// Open file dialog and load selected file
    pub fn open_file_dialog(&mut self, cx: &mut Context<Self>) {
        let options = PathPromptOptions {
            files: true,
            directories: false,
            multiple: false,
            prompt: Some(SharedString::from("Open File")),
        };

        let receiver = cx.prompt_for_paths(options);

        cx.spawn(async move |entity, cx| {
            if let Ok(Ok(Some(paths))) = receiver.await {
                if let Some(path) = paths.into_iter().next() {
                    let _ = entity.update(cx, |editor, cx| {
                        match editor.open_file_in_new_tab(path.clone()) {
                            Ok(_) => {
                                editor.save_message =
                                    Some(format!("Opened in new tab: {}", path.display()));
                            }
                            Err(e) => {
                                editor.save_message = Some(format!("Error: {}", e));
                            }
                        }
                        cx.notify();
                    });
                }
            }
        })
        .detach();
    }

    /// Save As dialog - save file to a new location
    pub fn save_as_dialog(&mut self, cx: &mut Context<Self>) {
        // Get directory and suggested filename from current document
        let (directory, suggested_name) = if let Some(path) = self.tab().document.file_path() {
            let dir = path
                .parent()
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| PathBuf::from("."));
            let name = path.file_name().map(|n| n.to_string_lossy().to_string());
            (dir, name)
        } else {
            (PathBuf::from("."), Some("untitled.bin".to_string()))
        };

        let receiver = cx.prompt_for_new_path(&directory, suggested_name.as_deref());

        cx.spawn(async move |entity, cx| {
            if let Ok(Ok(Some(path))) = receiver.await {
                let _ = entity.update(cx, |editor, cx| {
                    match editor.save_file_as(path.clone()) {
                        Ok(_) => {
                            editor.save_message = Some(format!("Saved as: {}", path.display()));
                        }
                        Err(e) => {
                            editor.save_message = Some(format!("Error: {}", e));
                        }
                    }
                    cx.notify();
                });
            }
        })
        .detach();
    }

    /// Save selection to a new file
    pub fn save_selection_as_dialog(&mut self, cx: &mut Context<Self>) {
        let (start, end) = match self.selection_range() {
            Some(range) => range,
            None => {
                self.save_message = Some("No selection to save".to_string());
                cx.notify();
                return;
            }
        };

        let bytes = match self.get_selected_bytes() {
            Some((_offset, bytes)) => bytes,
            None => {
                self.save_message = Some("No bytes in selection".to_string());
                cx.notify();
                return;
            }
        };

        let byte_count = end - start + 1;
        let directory = if let Some(path) = self.tab().document.file_path() {
            path.parent()
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| PathBuf::from("."))
        } else {
            PathBuf::from(".")
        };

        let receiver = cx.prompt_for_new_path(&directory, Some("selection.bin"));

        cx.spawn(async move |entity, cx| {
            if let Ok(Ok(Some(path))) = receiver.await {
                let _ = entity.update(cx, |editor, cx| {
                    match std::fs::write(&path, &bytes) {
                        Ok(_) => {
                            editor.save_message = Some(format!(
                                "Saved {} bytes to {}",
                                byte_count,
                                path.display()
                            ));
                        }
                        Err(e) => {
                            editor.save_message = Some(format!("Error: {}", e));
                        }
                    }
                    cx.notify();
                });
            }
        })
        .detach();
    }

    /// Handle file drop event - open in new tab
    pub fn handle_file_drop(&mut self, paths: &ExternalPaths, cx: &mut Context<Self>) {
        if let Some(path) = paths.paths().first() {
            match self.open_file_in_new_tab(path.clone()) {
                Ok(_) => {
                    self.save_message = Some(format!("Opened: {}", path.display()));
                }
                Err(e) => {
                    self.save_message = Some(format!("Error: {}", e));
                }
            }
            cx.notify();
        }
    }
}
