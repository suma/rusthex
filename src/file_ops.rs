//! File operation functionality for the hex editor
//!
//! This module contains methods for file I/O operations:
//! - Loading and saving files
//! - File dialogs (open, save as)
//! - File drop handling
//! - Unsaved changes management

use gpui::{Context, ExternalPaths, PathPromptOptions, PromptLevel, SharedString, Window};
use std::io::Write;
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
            self.log(crate::log_panel::LogLevel::Info, format!("Saved to {}", path.display()));
        }
        Ok(())
    }

    /// Save file to a new path
    pub fn save_file_as(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.tab_mut().document.save_as(path.clone())?;
        self.log(crate::log_panel::LogLevel::Info, format!("Saved to {}", path.display()));
        Ok(())
    }

    /// Save file with confirmation dialog
    pub fn save_with_confirmation(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        // Check if there are unsaved changes
        if !self.tab().document.has_unsaved_changes() {
            self.log(crate::log_panel::LogLevel::Info, "No changes to save");
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
                                editor.log(crate::log_panel::LogLevel::Error, format!("Error: {}", e));
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
                                editor.log(crate::log_panel::LogLevel::Info, format!("Opened in new tab: {}", path.display()));
                                editor.detect_file_type(cx);
                            }
                            Err(e) => {
                                editor.log(crate::log_panel::LogLevel::Error, format!("Error: {}", e));
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
                            editor.log(crate::log_panel::LogLevel::Info, format!("Saved as: {}", path.display()));
                        }
                        Err(e) => {
                            editor.log(crate::log_panel::LogLevel::Error, format!("Error: {}", e));
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
                self.log(crate::log_panel::LogLevel::Info, "No selection to save");
                cx.notify();
                return;
            }
        };

        let byte_count = end - start + 1;
        if byte_count == 0 || end >= self.tab().document.len() {
            self.log(crate::log_panel::LogLevel::Info, "No bytes in selection");
            cx.notify();
            return;
        }

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
                    let result = (|| {
                        let file = std::fs::File::create(&path)?;
                        let mut writer = std::io::BufWriter::new(file);
                        editor
                            .tab()
                            .document
                            .write_range_to(start..start + byte_count, &mut writer)?;
                        writer.flush()
                    })();
                    match result {
                        Ok(_) => {
                            editor.log(crate::log_panel::LogLevel::Info, format!(
                                "Saved {} bytes to {}",
                                byte_count,
                                path.display()
                            ));
                        }
                        Err(e) => {
                            editor.log(crate::log_panel::LogLevel::Error, format!("Error: {}", e));
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
                    self.log(crate::log_panel::LogLevel::Info, format!("Opened: {}", path.display()));
                    self.detect_file_type(cx);
                }
                Err(e) => {
                    self.log(crate::log_panel::LogLevel::Error, format!("Error: {}", e));
                }
            }
            cx.notify();
        }
    }

    /// Analyze the current selection by piping context to an external command
    pub fn analyze_selection(&mut self, cx: &mut Context<Self>) {
        let command_str = self.settings.analyze.command.clone();
        if command_str.is_empty() {
            self.log(
                crate::log_panel::LogLevel::Warning,
                "Set [analyze] command in config.toml to enable selection analysis",
            );
            cx.notify();
            return;
        }

        let (sel_start, sel_end) = match self.selection_range() {
            Some(range) => range,
            None => {
                self.log(
                    crate::log_panel::LogLevel::Info,
                    "No selection to analyze",
                );
                cx.notify();
                return;
            }
        };

        let doc = &self.tab().document;
        let doc_len = doc.len();
        let sel_len = sel_end - sel_start + 1;

        // Gather selection bytes as hex string
        let selection_hex = (sel_start..=sel_end)
            .filter_map(|i| doc.get_byte(i))
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<_>>()
            .join(" ");

        // Context: up to 256 bytes before selection
        let ctx_before_start = sel_start.saturating_sub(256);
        let context_before_hex = (ctx_before_start..sel_start)
            .filter_map(|i| doc.get_byte(i))
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<_>>()
            .join(" ");

        // Context: up to 256 bytes after selection
        let ctx_after_end = (sel_end + 1 + 256).min(doc_len);
        let context_after_hex = (sel_end + 1..ctx_after_end)
            .filter_map(|i| doc.get_byte(i))
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<_>>()
            .join(" ");

        let file_name = doc.file_name().unwrap_or("(unknown)").to_string();
        let file_type = self.tab().file_type.clone().unwrap_or_default();

        let json = serde_json::json!({
            "file_name": file_name,
            "file_size": doc_len,
            "file_type": file_type,
            "selection_offset": sel_start,
            "selection_length": sel_len,
            "selection_hex": selection_hex,
            "context_before_hex": context_before_hex,
            "context_after_hex": context_after_hex,
        });
        let prompt = &self.settings.analyze.prompt;
        let json_str = if prompt.is_empty() {
            json.to_string()
        } else {
            format!("{}\n{}", prompt, json.to_string())
        };

        self.log(
            crate::log_panel::LogLevel::Info,
            "Analyzing selection...",
        );
        self.log_panel.visible = true;
        self.log_panel.active_tab = crate::log_panel::BottomPanelTab::Log;
        cx.notify();

        let args = shell_words(&command_str);
        if args.is_empty() {
            self.log(
                crate::log_panel::LogLevel::Error,
                "Analyze command is empty after parsing",
            );
            cx.notify();
            return;
        }

        let timeout_secs = self.settings.analyze.timeout;

        // Run the blocking command on a background thread to keep the UI responsive
        let (tx, rx) = std::sync::mpsc::channel::<Result<std::process::Output, String>>();
        std::thread::spawn(move || {
            let result = std::process::Command::new(&args[0])
                .args(&args[1..])
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::null())
                .spawn()
                .map_err(|e| format!("Failed to run analyze command: {}", e))
                .and_then(|mut child| {
                    if let Some(ref mut stdin) = child.stdin {
                        let _ = stdin.write_all(json_str.as_bytes());
                    }
                    drop(child.stdin.take());

                    // Poll with timeout
                    let deadline = std::time::Instant::now()
                        + std::time::Duration::from_secs(timeout_secs);
                    loop {
                        match child.try_wait() {
                            Ok(Some(_status)) => {
                                // Process exited; collect output
                                return child
                                    .wait_with_output()
                                    .map_err(|e| format!("Failed to read output: {}", e));
                            }
                            Ok(None) => {
                                // Still running
                                if std::time::Instant::now() >= deadline {
                                    let _ = child.kill();
                                    let _ = child.wait();
                                    return Err(format!(
                                        "Analyze command timed out after {}s",
                                        timeout_secs
                                    ));
                                }
                                std::thread::sleep(std::time::Duration::from_millis(100));
                            }
                            Err(e) => {
                                return Err(format!("Failed to wait for process: {}", e));
                            }
                        }
                    }
                });
            let _ = tx.send(result);
        });

        cx.spawn(async move |entity, cx| {
            // Poll the channel without blocking the UI thread
            let result = loop {
                match rx.try_recv() {
                    Ok(r) => break r,
                    Err(std::sync::mpsc::TryRecvError::Empty) => {
                        gpui::Timer::after(std::time::Duration::from_millis(100)).await;
                    }
                    Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                        break Err("Analyze worker thread terminated unexpectedly".to_string());
                    }
                }
            };

            let _ = entity.update(cx, |editor, cx| {
                match result {
                    Ok(output) if output.status.success() => {
                        let stdout = String::from_utf8_lossy(&output.stdout)
                            .trim()
                            .to_string();
                        if stdout.is_empty() {
                            editor.log(
                                crate::log_panel::LogLevel::Info,
                                "Analysis complete (no output)",
                            );
                        } else {
                            for line in stdout.lines() {
                                editor.log(
                                    crate::log_panel::LogLevel::Info,
                                    line.to_string(),
                                );
                            }
                        }
                    }
                    Ok(output) => {
                        let code = output.status.code().unwrap_or(-1);
                        eprintln!("Analyze command exited with code {}", code);
                    }
                    Err(e) => {
                        eprintln!("Analyze error: {}", e);
                    }
                }
                cx.notify();
            });
        })
        .detach();
    }

    /// Detect file type using the `file` command and log the result
    pub fn detect_file_type(&mut self, cx: &mut Context<Self>) {
        // Check if `file` command exists on PATH
        if find_file_command().is_none() {
            return;
        }

        let len = self.tab().document.len();
        if len == 0 {
            return;
        }
        let size = len.min(1024);
        let bytes = match self.tab().document.get_slice(0..size) {
            Some(b) => b,
            None => return,
        };
        let display_name = self
            .tab()
            .document
            .file_name()
            .unwrap_or("(stdin)")
            .to_string();

        cx.spawn(async move |entity, cx| {
            let result = std::process::Command::new("file")
                .args(["-b", "-"])
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .spawn()
                .and_then(|mut child| {
                    if let Some(ref mut stdin) = child.stdin {
                        let _ = stdin.write_all(&bytes);
                    }
                    drop(child.stdin.take()); // close stdin
                    child.wait_with_output()
                });

            let _ = entity.update(cx, |editor, cx| {
                match result {
                    Ok(output) if output.status.success() => {
                        let file_type = String::from_utf8_lossy(&output.stdout)
                            .trim()
                            .to_string();
                        editor.tab_mut().file_type = Some(file_type.clone());
                        editor.log(
                            crate::log_panel::LogLevel::Info,
                            format!("{}: {}", display_name, file_type),
                        );
                    }
                    Ok(output) => {
                        let stderr = String::from_utf8_lossy(&output.stderr)
                            .trim()
                            .to_string();
                        editor.log(
                            crate::log_panel::LogLevel::Warning,
                            format!("file command failed: {}", stderr),
                        );
                    }
                    Err(e) => {
                        editor.log(
                            crate::log_panel::LogLevel::Warning,
                            format!("file command not available: {}", e),
                        );
                    }
                }
                cx.notify();
            });
        })
        .detach();
    }
}

/// Split a command string into arguments, respecting single and double quotes.
fn shell_words(input: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '"' => {
                // Consume until closing double quote
                for c2 in chars.by_ref() {
                    if c2 == '"' {
                        break;
                    }
                    current.push(c2);
                }
            }
            '\'' => {
                // Consume until closing single quote
                for c2 in chars.by_ref() {
                    if c2 == '\'' {
                        break;
                    }
                    current.push(c2);
                }
            }
            ' ' | '\t' => {
                if !current.is_empty() {
                    args.push(std::mem::take(&mut current));
                }
            }
            _ => {
                current.push(c);
            }
        }
    }
    if !current.is_empty() {
        args.push(current);
    }
    args
}

/// Search PATH for the `file` command, returning its full path if found.
/// On Unix, looks for "file". On Windows, looks for "file.exe".
fn find_file_command() -> Option<PathBuf> {
    #[cfg(target_os = "windows")]
    const FILE_NAME: &str = "file.exe";
    #[cfg(not(target_os = "windows"))]
    const FILE_NAME: &str = "file";

    let paths = std::env::var_os("PATH")?;
    std::env::split_paths(&paths).find_map(|dir| {
        let candidate = dir.join(FILE_NAME);
        candidate.is_file().then_some(candidate)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shell_words_basic() {
        assert_eq!(shell_words("llm -m gpt-4o"), vec!["llm", "-m", "gpt-4o"]);
    }

    #[test]
    fn test_shell_words_double_quotes() {
        assert_eq!(
            shell_words(r#"echo "hello world" foo"#),
            vec!["echo", "hello world", "foo"]
        );
    }

    #[test]
    fn test_shell_words_single_quotes() {
        assert_eq!(
            shell_words("echo 'hello world' foo"),
            vec!["echo", "hello world", "foo"]
        );
    }

    #[test]
    fn test_shell_words_empty() {
        assert!(shell_words("").is_empty());
        assert!(shell_words("   ").is_empty());
    }

    #[test]
    fn test_shell_words_extra_whitespace() {
        assert_eq!(
            shell_words("  cmd   arg1   arg2  "),
            vec!["cmd", "arg1", "arg2"]
        );
    }

    #[test]
    fn test_shell_words_mixed_quotes() {
        assert_eq!(
            shell_words(r#"cmd "arg 1" 'arg 2' arg3"#),
            vec!["cmd", "arg 1", "arg 2", "arg3"]
        );
    }
}
