//! Configuration management module
//!
//! This module handles loading and saving application settings from a TOML file.
//! Settings are stored at:
//! - macOS/Linux: ~/.config/rusthex/config.toml
//! - Windows: %APPDATA%\rusthex\config.toml

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Default endianness for data inspector
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum DefaultEndian {
    Little,
    Big,
}

impl Default for DefaultEndian {
    fn default() -> Self {
        DefaultEndian::Little
    }
}

/// Display settings
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct DisplaySettings {
    /// Bytes per row (8, 16, or 32)
    pub bytes_per_row: usize,
    /// Font size in pixels
    pub font_size: f32,
    /// Font family name (e.g., "Monaco", "Menlo", "Consolas")
    pub font_name: String,
    /// Color theme name ("dark", "light", "monokai")
    pub theme: String,
}

impl Default for DisplaySettings {
    fn default() -> Self {
        let font_name = if cfg!(target_os = "macos") {
            "Monaco"
        } else {
            "Consolas"
        };
        Self {
            bytes_per_row: 16,
            font_size: 12.0,
            font_name: font_name.to_string(),
            theme: "dark".to_string(),
        }
    }
}

/// Editor settings
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct EditorSettings {
    /// Default endianness for data inspector
    pub default_endian: DefaultEndian,
    /// Maximum number of undo levels
    pub max_undo_levels: usize,
}

impl Default for EditorSettings {
    fn default() -> Self {
        Self {
            default_endian: DefaultEndian::default(),
            max_undo_levels: 1000,
        }
    }
}

/// Window settings
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct WindowSettings {
    /// Window width in pixels
    pub width: u32,
    /// Window height in pixels
    pub height: u32,
}

impl Default for WindowSettings {
    fn default() -> Self {
        Self {
            width: 800,
            height: 600,
        }
    }
}

/// Pattern language settings
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct PatternSettings {
    /// Directory path containing .hexpat files
    pub hexpat_dir: String,
    /// Additional directories to search for #include / import files
    pub include_dirs: Vec<String>,
}

impl Default for PatternSettings {
    fn default() -> Self {
        Self {
            hexpat_dir: String::new(),
            include_dirs: Vec::new(),
        }
    }
}

/// Analyze selection settings
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct AnalyzeSettings {
    /// External command to pipe selection context to (e.g., "llm -m gpt-4o")
    pub command: String,
    /// Prompt text prepended before the JSON input
    pub prompt: String,
    /// Timeout in seconds (default: 30)
    pub timeout: u64,
}

impl Default for AnalyzeSettings {
    fn default() -> Self {
        Self {
            command: String::new(),
            prompt: "Parse next data:".to_string(),
            timeout: 30,
        }
    }
}

/// Application settings
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct Settings {
    /// Display settings
    pub display: DisplaySettings,
    /// Editor settings
    pub editor: EditorSettings,
    /// Window settings
    pub window: WindowSettings,
    /// Pattern language settings
    pub pattern: PatternSettings,
    /// Analyze selection settings
    pub analyze: AnalyzeSettings,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            display: DisplaySettings::default(),
            editor: EditorSettings::default(),
            window: WindowSettings::default(),
            pattern: PatternSettings::default(),
            analyze: AnalyzeSettings::default(),
        }
    }
}

impl Settings {
    /// Get the configuration file path
    pub fn config_path() -> Option<PathBuf> {
        dirs::config_dir().map(|p| p.join("rusthex").join("config.toml"))
    }

    /// Load settings from the configuration file
    /// Returns default settings if the file doesn't exist or is invalid
    pub fn load() -> Self {
        if let Some(path) = Self::config_path() {
            if path.exists() {
                match std::fs::read_to_string(&path) {
                    Ok(content) => {
                        match toml::from_str(&content) {
                            Ok(settings) => {
                                let mut settings: Settings = settings;
                                // Validate and fix invalid values
                                settings.validate();
                                return settings;
                            }
                            Err(e) => {
                                eprintln!("Failed to parse config file: {}", e);
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("Failed to read config file: {}", e);
                    }
                }
            }
        }
        Self::default()
    }

    /// Validate and fix invalid setting values
    fn validate(&mut self) {
        // Validate bytes_per_row (must be 8, 16, or 32)
        if ![8, 16, 32].contains(&self.display.bytes_per_row) {
            self.display.bytes_per_row = 16;
        }

        // Validate font_size (reasonable range: 8-32)
        if self.display.font_size < 8.0 || self.display.font_size > 32.0 {
            self.display.font_size = 12.0;
        }

        // Validate theme name
        if !["dark", "light", "monokai"].contains(&self.display.theme.to_lowercase().as_str()) {
            self.display.theme = "dark".to_string();
        }

        // Validate max_undo_levels (reasonable range: 10-10000)
        if self.editor.max_undo_levels < 10 || self.editor.max_undo_levels > 10000 {
            self.editor.max_undo_levels = 1000;
        }

        // Validate window size (minimum 400x300)
        if self.window.width < 400 {
            self.window.width = 800;
        }
        if self.window.height < 300 {
            self.window.height = 600;
        }

        // Trim analyze command whitespace
        self.analyze.command = self.analyze.command.trim().to_string();

        // Validate analyze timeout (1-600 seconds)
        if self.analyze.timeout == 0 || self.analyze.timeout > 600 {
            self.analyze.timeout = 30;
        }
    }
}
