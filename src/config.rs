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
    /// Window X position (None = centered)
    pub x: Option<i32>,
    /// Window Y position (None = centered)
    pub y: Option<i32>,
}

impl Default for WindowSettings {
    fn default() -> Self {
        Self {
            width: 800,
            height: 600,
            x: None,
            y: None,
        }
    }
}

/// Layout settings for panel visibility and sizes
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct LayoutSettings {
    pub inspector_visible: bool,
    pub inspector_endian: String,
    pub pattern_panel_visible: bool,
    pub pattern_panel_width: f32,
    pub bitmap_visible: bool,
    pub bitmap_panel_width: f32,
    pub bitmap_color_mode: String,
    pub bitmap_width: usize,
    pub text_encoding: String,
    pub log_panel_visible: bool,
    pub log_panel_height: f32,
    pub log_panel_tab: String,
}

impl Default for LayoutSettings {
    fn default() -> Self {
        Self {
            inspector_visible: false,
            inspector_endian: "little".to_string(),
            pattern_panel_visible: false,
            pattern_panel_width: 280.0,
            bitmap_visible: false,
            bitmap_panel_width: 520.0,
            bitmap_color_mode: "grayscale".to_string(),
            bitmap_width: 256,
            text_encoding: "ascii".to_string(),
            log_panel_visible: false,
            log_panel_height: 49.0,
            log_panel_tab: "log".to_string(),
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
    /// Layout settings (panel visibility and sizes)
    pub layout: LayoutSettings,
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
            layout: LayoutSettings::default(),
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

    /// Save settings to the configuration file
    pub fn save(&self) -> std::io::Result<()> {
        if let Some(path) = Self::config_path() {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let content = toml::to_string_pretty(self)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
            std::fs::write(&path, content)?;
        }
        Ok(())
    }

    /// Load settings from a TOML string (for testing)
    pub fn load_from_str(content: &str) -> Self {
        match toml::from_str(content) {
            Ok(settings) => {
                let mut settings: Settings = settings;
                settings.validate();
                settings
            }
            Err(_) => Self::default(),
        }
    }

    /// Validate and fix invalid setting values
    pub fn validate(&mut self) {
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

        // Validate layout settings
        if self.layout.pattern_panel_width < 100.0 || self.layout.pattern_panel_width > 2000.0 {
            self.layout.pattern_panel_width = 280.0;
        }
        if self.layout.bitmap_panel_width < 100.0 || self.layout.bitmap_panel_width > 2000.0 {
            self.layout.bitmap_panel_width = 520.0;
        }
        if !["little", "big"].contains(&self.layout.inspector_endian.as_str()) {
            self.layout.inspector_endian = "little".to_string();
        }
        if !["grayscale", "heatmap", "category"]
            .contains(&self.layout.bitmap_color_mode.as_str())
        {
            self.layout.bitmap_color_mode = "grayscale".to_string();
        }
        if !crate::bitmap::BITMAP_WIDTHS.contains(&self.layout.bitmap_width) {
            self.layout.bitmap_width = 256;
        }
        if crate::ui::TextEncoding::from_label(&self.layout.text_encoding).is_none() {
            self.layout.text_encoding = "ascii".to_string();
        }
        if self.layout.log_panel_height < crate::log_panel::MIN_HEIGHT
            || self.layout.log_panel_height > crate::log_panel::MAX_HEIGHT
        {
            self.layout.log_panel_height = crate::log_panel::DEFAULT_HEIGHT;
        }
        if !["log", "info"].contains(&self.layout.log_panel_tab.as_str()) {
            self.layout.log_panel_tab = "log".to_string();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_settings_default_values() {
        let layout = LayoutSettings::default();
        assert!(!layout.inspector_visible);
        assert_eq!(layout.inspector_endian, "little");
        assert!(!layout.pattern_panel_visible);
        assert_eq!(layout.pattern_panel_width, 280.0);
        assert!(!layout.bitmap_visible);
        assert_eq!(layout.bitmap_panel_width, 520.0);
        assert_eq!(layout.bitmap_color_mode, "grayscale");
        assert_eq!(layout.bitmap_width, 256);
        assert_eq!(layout.text_encoding, "ascii");
        assert!(!layout.log_panel_visible);
        assert_eq!(layout.log_panel_height, 49.0);
        assert_eq!(layout.log_panel_tab, "log");
    }

    #[test]
    fn settings_toml_roundtrip() {
        let mut settings = Settings::default();
        settings.layout.inspector_visible = true;
        settings.layout.inspector_endian = "big".to_string();
        settings.layout.bitmap_visible = true;
        settings.layout.bitmap_color_mode = "heatmap".to_string();
        settings.layout.text_encoding = "utf-8".to_string();
        settings.layout.log_panel_visible = true;
        settings.layout.log_panel_height = 100.0;
        settings.layout.log_panel_tab = "info".to_string();
        settings.window.x = Some(100);
        settings.window.y = Some(200);

        let toml_str = toml::to_string_pretty(&settings).unwrap();
        let loaded: Settings = toml::from_str(&toml_str).unwrap();

        assert!(loaded.layout.inspector_visible);
        assert_eq!(loaded.layout.inspector_endian, "big");
        assert!(loaded.layout.bitmap_visible);
        assert_eq!(loaded.layout.bitmap_color_mode, "heatmap");
        assert_eq!(loaded.layout.text_encoding, "utf-8");
        assert!(loaded.layout.log_panel_visible);
        assert_eq!(loaded.layout.log_panel_height, 100.0);
        assert_eq!(loaded.layout.log_panel_tab, "info");
        assert_eq!(loaded.window.x, Some(100));
        assert_eq!(loaded.window.y, Some(200));
    }

    #[test]
    fn settings_load_without_layout_section() {
        let toml_str = r#"
[display]
bytes_per_row = 16

[window]
width = 1024
height = 768
"#;
        let settings = Settings::load_from_str(toml_str);
        assert_eq!(settings.window.width, 1024);
        assert_eq!(settings.window.height, 768);
        // layout should have defaults
        assert!(!settings.layout.inspector_visible);
        assert_eq!(settings.layout.pattern_panel_width, 280.0);
        assert_eq!(settings.layout.text_encoding, "ascii");
        assert_eq!(settings.window.x, None);
        assert_eq!(settings.window.y, None);
    }

    #[test]
    fn validate_fixes_invalid_layout_values() {
        let mut settings = Settings::default();
        settings.layout.pattern_panel_width = 50.0; // too small
        settings.layout.bitmap_panel_width = 5000.0; // too large
        settings.layout.inspector_endian = "invalid".to_string();
        settings.layout.bitmap_color_mode = "invalid".to_string();
        settings.layout.bitmap_width = 999; // not in BITMAP_WIDTHS
        settings.layout.text_encoding = "invalid".to_string();
        settings.layout.log_panel_height = 5.0; // below MIN_HEIGHT
        settings.layout.log_panel_tab = "invalid".to_string();

        settings.validate();

        assert_eq!(settings.layout.pattern_panel_width, 280.0);
        assert_eq!(settings.layout.bitmap_panel_width, 520.0);
        assert_eq!(settings.layout.inspector_endian, "little");
        assert_eq!(settings.layout.bitmap_color_mode, "grayscale");
        assert_eq!(settings.layout.bitmap_width, 256);
        assert_eq!(settings.layout.text_encoding, "ascii");
        assert_eq!(settings.layout.log_panel_height, 49.0);
        assert_eq!(settings.layout.log_panel_tab, "log");
    }

    #[test]
    fn window_position_serialization() {
        let mut settings = Settings::default();
        settings.window.x = Some(-50);
        settings.window.y = Some(300);

        let toml_str = toml::to_string_pretty(&settings).unwrap();
        let loaded: Settings = toml::from_str(&toml_str).unwrap();
        assert_eq!(loaded.window.x, Some(-50));
        assert_eq!(loaded.window.y, Some(300));
    }

    #[test]
    fn window_position_none_when_absent() {
        let toml_str = r#"
[window]
width = 800
height = 600
"#;
        let settings = Settings::load_from_str(toml_str);
        assert_eq!(settings.window.x, None);
        assert_eq!(settings.window.y, None);
    }
}
