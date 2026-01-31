use gpui::{Hsla, rgb, rgba};

/// Theme name enumeration
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ThemeName {
    Dark,
    Light,
    Monokai,
}

impl ThemeName {
    pub fn label(&self) -> &'static str {
        match self {
            ThemeName::Dark => "Dark",
            ThemeName::Light => "Light",
            ThemeName::Monokai => "Monokai",
        }
    }

    pub fn all() -> &'static [ThemeName] {
        &[ThemeName::Dark, ThemeName::Light, ThemeName::Monokai]
    }

    pub fn from_str(s: &str) -> ThemeName {
        match s.to_lowercase().as_str() {
            "light" => ThemeName::Light,
            "monokai" => ThemeName::Monokai,
            _ => ThemeName::Dark,
        }
    }
}

/// hex -> Hsla conversion helper
fn color(hex: u32) -> Hsla {
    rgb(hex).into()
}

/// hex (RGBA) -> Hsla conversion helper
fn color_a(hex: u32) -> Hsla {
    rgba(hex).into()
}

/// Color theme struct
#[derive(Clone)]
pub struct Theme {
    // Backgrounds
    pub bg_primary: Hsla,
    pub bg_secondary: Hsla,
    pub bg_surface: Hsla,
    pub bg_elevated: Hsla,
    pub bg_hover: Hsla,
    pub bg_hover_secondary: Hsla,
    pub bg_hover_tertiary: Hsla,
    pub bg_selection: Hsla,

    // Borders
    pub border_primary: Hsla,
    pub border_secondary: Hsla,
    pub border_dropdown: Hsla,

    // Text
    pub text_primary: Hsla,
    pub text_secondary: Hsla,
    pub text_muted: Hsla,
    pub text_dim: Hsla,
    pub text_on_accent: Hsla,

    // Accent
    pub accent_primary: Hsla,
    pub accent_secondary: Hsla,
    pub accent_success: Hsla,

    // Search
    pub search_current_bg: Hsla,
    pub search_match_bg: Hsla,

    // Warning / Error
    pub text_warning: Hsla,
    pub text_warning_secondary: Hsla,
    pub text_error: Hsla,
    pub text_diff: Hsla,

    // Special
    pub text_modified: Hsla,
    pub text_insert_mode: Hsla,
    pub bookmark_plain: Hsla,
    pub bookmark_comment: Hsla,

    // Compare mode
    pub compare_left: Hsla,
    pub compare_right: Hsla,
    pub compare_separator: Hsla,

    // Modal
    pub modal_overlay: Hsla,
}

impl Theme {
    pub fn from_name(name: ThemeName) -> Self {
        match name {
            ThemeName::Dark => Self::dark(),
            ThemeName::Light => Self::light(),
            ThemeName::Monokai => Self::monokai(),
        }
    }

    pub fn dark() -> Self {
        Self {
            // Backgrounds
            bg_primary: color(0x1e1e1e),
            bg_secondary: color(0x1a1a1a),
            bg_surface: color(0x252525),
            bg_elevated: color(0x2a2a2a),
            bg_hover: color(0x333333),
            bg_hover_secondary: color(0x3a3a3a),
            bg_hover_tertiary: color(0x444444),
            bg_selection: color(0x505050),

            // Borders
            border_primary: color(0x404040),
            border_secondary: color(0x333333),
            border_dropdown: color(0x555555),

            // Text
            text_primary: color(0xffffff),
            text_secondary: color(0xcccccc),
            text_muted: color(0x808080),
            text_dim: color(0x606060),
            text_on_accent: color(0x000000),

            // Accent
            accent_primary: color(0x4a9eff),
            accent_secondary: color(0xff8c00),
            accent_success: color(0x00ff00),

            // Search
            search_current_bg: color(0xff8c00),
            search_match_bg: color(0xffff00),

            // Warning / Error
            text_warning: color(0xffff00),
            text_warning_secondary: color(0xffaa00),
            text_error: color(0xff0000),
            text_diff: color(0xff6666),

            // Special
            text_modified: color(0xff6b6b),
            text_insert_mode: color(0x00cccc),
            bookmark_plain: color(0x00bfff),
            bookmark_comment: color(0x00ff88),

            // Compare mode
            compare_left: color(0x4a9eff),
            compare_right: color(0xff8c00),
            compare_separator: color(0x4a9eff),

            // Modal
            modal_overlay: color_a(0x00000080),
        }
    }

    pub fn light() -> Self {
        Self {
            // Backgrounds
            bg_primary: color(0xffffff),
            bg_secondary: color(0xf5f5f5),
            bg_surface: color(0xf0f0f0),
            bg_elevated: color(0xe8e8e8),
            bg_hover: color(0xdddddd),
            bg_hover_secondary: color(0xd0d0d0),
            bg_hover_tertiary: color(0xc8c8c8),
            bg_selection: color(0xb0c4de),

            // Borders
            border_primary: color(0xcccccc),
            border_secondary: color(0xdddddd),
            border_dropdown: color(0xaaaaaa),

            // Text
            text_primary: color(0x1e1e1e),
            text_secondary: color(0x444444),
            text_muted: color(0x666666),
            text_dim: color(0x999999),
            text_on_accent: color(0xffffff),

            // Accent
            accent_primary: color(0x0066cc),
            accent_secondary: color(0xcc6600),
            accent_success: color(0x008800),

            // Search
            search_current_bg: color(0xff9900),
            search_match_bg: color(0xffee00),

            // Warning / Error
            text_warning: color(0xcc8800),
            text_warning_secondary: color(0xcc6600),
            text_error: color(0xcc0000),
            text_diff: color(0xcc3333),

            // Special
            text_modified: color(0xd32f2f),
            text_insert_mode: color(0x008888),
            bookmark_plain: color(0x0088cc),
            bookmark_comment: color(0x00aa44),

            // Compare mode
            compare_left: color(0x0066cc),
            compare_right: color(0xcc6600),
            compare_separator: color(0x0066cc),

            // Modal
            modal_overlay: color_a(0x00000040),
        }
    }

    pub fn monokai() -> Self {
        Self {
            // Backgrounds
            bg_primary: color(0x272822),
            bg_secondary: color(0x1e1f1a),
            bg_surface: color(0x2d2e27),
            bg_elevated: color(0x383930),
            bg_hover: color(0x49483e),
            bg_hover_secondary: color(0x555549),
            bg_hover_tertiary: color(0x626254),
            bg_selection: color(0x49483e),

            // Borders
            border_primary: color(0x49483e),
            border_secondary: color(0x3e3d32),
            border_dropdown: color(0x75715e),

            // Text
            text_primary: color(0xf8f8f2),
            text_secondary: color(0xd0d0c8),
            text_muted: color(0x75715e),
            text_dim: color(0x5f5f50),
            text_on_accent: color(0x272822),

            // Accent
            accent_primary: color(0x66d9ef),
            accent_secondary: color(0xfd971f),
            accent_success: color(0xa6e22e),

            // Search
            search_current_bg: color(0xfd971f),
            search_match_bg: color(0xe6db74),

            // Warning / Error
            text_warning: color(0xe6db74),
            text_warning_secondary: color(0xfd971f),
            text_error: color(0xf92672),
            text_diff: color(0xf92672),

            // Special
            text_modified: color(0xf92672),
            text_insert_mode: color(0x66d9ef),
            bookmark_plain: color(0x66d9ef),
            bookmark_comment: color(0xa6e22e),

            // Compare mode
            compare_left: color(0x66d9ef),
            compare_right: color(0xfd971f),
            compare_separator: color(0x66d9ef),

            // Modal
            modal_overlay: color_a(0x00000080),
        }
    }
}
