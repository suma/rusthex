mod document;

use document::Document;
use gpui::{
    App, Application, Bounds, Context, Focusable, FocusHandle, KeyDownEvent, Pixels, Window, WindowBounds, WindowOptions,
    div, prelude::*, px, rgb, size, ScrollHandle,
};
use gpui_component::scroll::{Scrollbar, ScrollbarState, ScrollbarShow};
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq)]
enum EditPane {
    Hex,
    Ascii,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum HexNibble {
    High,  // Upper nibble (first hex digit)
    Low,   // Lower nibble (second hex digit)
}

struct HexEditor {
    document: Document,
    bytes_per_row: usize,
    scroll_handle: ScrollHandle,
    scrollbar_state: ScrollbarState,
    cursor_position: usize,
    focus_handle: FocusHandle,
    edit_pane: EditPane,
    hex_nibble: HexNibble,
    save_message: Option<String>,
    scroll_offset: Pixels, // Current scroll position in pixels
}

impl HexEditor {
    fn new(cx: &mut Context<Self>) -> Self {
        Self {
            document: Document::new(),
            bytes_per_row: 16,
            scroll_handle: ScrollHandle::new(),
            scrollbar_state: ScrollbarState::default(),
            cursor_position: 0,
            focus_handle: cx.focus_handle(),
            edit_pane: EditPane::Hex,
            hex_nibble: HexNibble::High,
            save_message: None,
            scroll_offset: px(0.0),
        }
    }

    fn load_file(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.document.load(path)?;
        self.cursor_position = 0;
        Ok(())
    }

    fn with_sample_data(cx: &mut Context<Self>) -> Self {
        let mut editor = Self::new(cx);
        // Sample data: Generate more data to test scrolling
        let mut data = Vec::new();

        // Add header text
        data.extend_from_slice(b"Rust Hex Editor - Sample Binary File\n");
        data.extend_from_slice(b"=====================================\n\n");

        // Add multiple lines of varied data
        for i in 0..100 {
            data.extend_from_slice(format!("Line {:03}: ", i).as_bytes());
            // Add printable characters
            for j in 0..16 {
                let byte = (0x41 + ((i + j) % 26)) as u8; // A-Z cycling
                data.push(byte);
            }
            data.push(b'\n');

            // Add some binary data
            for j in 0..16 {
                data.push(((i * 16 + j) % 256) as u8);
            }
        }

        editor.document = Document::with_data(data);
        editor
    }

    fn format_address(&self, offset: usize) -> String {
        format!("{:08X}", offset)
    }

    fn row_count(&self) -> usize {
        (self.document.len() + self.bytes_per_row - 1) / self.bytes_per_row
    }

    // Cursor movement methods
    fn move_cursor_left(&mut self) {
        if self.cursor_position > 0 {
            self.cursor_position -= 1;
            self.hex_nibble = HexNibble::High;
        }
    }

    fn move_cursor_right(&mut self) {
        if self.cursor_position < self.document.len().saturating_sub(1) {
            self.cursor_position += 1;
            self.hex_nibble = HexNibble::High;
        }
    }

    fn move_cursor_up(&mut self) {
        if self.cursor_position >= self.bytes_per_row {
            self.cursor_position -= self.bytes_per_row;
            self.hex_nibble = HexNibble::High;
        }
    }

    fn move_cursor_down(&mut self) {
        let new_pos = self.cursor_position + self.bytes_per_row;
        if new_pos < self.document.len() {
            self.cursor_position = new_pos;
            self.hex_nibble = HexNibble::High;
        }
    }

    // Toggle between Hex and ASCII pane
    fn toggle_pane(&mut self) {
        self.edit_pane = match self.edit_pane {
            EditPane::Hex => EditPane::Ascii,
            EditPane::Ascii => EditPane::Hex,
        };
        self.hex_nibble = HexNibble::High;
    }

    // Handle hex input (0-9, A-F)
    fn input_hex(&mut self, c: char) {
        if self.cursor_position >= self.document.len() {
            return;
        }

        // Parse hex digit
        let digit = match c.to_ascii_uppercase() {
            '0'..='9' => c as u8 - b'0',
            'A'..='F' => c.to_ascii_uppercase() as u8 - b'A' + 10,
            _ => return, // Invalid hex character
        };

        let current_byte = self.document.get_byte(self.cursor_position).unwrap();

        let new_byte = match self.hex_nibble {
            HexNibble::High => {
                // Update upper 4 bits
                self.hex_nibble = HexNibble::Low;
                (digit << 4) | (current_byte & 0x0F)
            }
            HexNibble::Low => {
                // Update lower 4 bits
                self.hex_nibble = HexNibble::High;
                (current_byte & 0xF0) | digit
            }
        };

        if let Err(e) = self.document.set_byte(self.cursor_position, new_byte) {
            eprintln!("Failed to set byte: {}", e);
            return;
        }

        // Auto-advance to next byte after completing a full byte edit
        if self.hex_nibble == HexNibble::High {
            self.move_cursor_right();
        }
    }

    // Handle ASCII input
    fn input_ascii(&mut self, c: char) {
        if self.cursor_position >= self.document.len() {
            return;
        }

        // Only accept printable ASCII characters
        if c >= ' ' && c <= '~' {
            if let Err(e) = self.document.set_byte(self.cursor_position, c as u8) {
                eprintln!("Failed to set byte: {}", e);
                return;
            }
            // Auto-advance to next byte
            self.move_cursor_right();
        }
    }

    // Save file to current path
    fn save_file(&mut self) -> std::io::Result<()> {
        self.document.save()?;
        if let Some(path) = self.document.file_path() {
            self.save_message = Some(format!("Saved to {}", path.display()));
        }
        Ok(())
    }

    // Save file to a new path
    fn save_file_as(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.document.save_as(path.clone())?;
        self.save_message = Some(format!("Saved to {}", path.display()));
        Ok(())
    }
}

impl Focusable for HexEditor {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl Render for HexEditor {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let row_count = self.row_count();

        // Get current scroll position (Phase 1: Track scroll offset)
        let scroll_position = self.scroll_handle.offset();
        self.scroll_offset = scroll_position.y;

        // Virtual scrolling: Calculate visible range
        // Phase 1 complete: scroll position tracking
        // TODO Phase 2: Calculate visible range based on viewport height
        // TODO Phase 3: Render only visible rows
        let max_rendered_rows = 1000;
        let rendered_row_count = row_count.min(max_rendered_rows);

        // Get display title
        let title = self.document.file_name()
            .unwrap_or("Rust Hex Editor")
            .to_string();

        div()
            .flex()
            .flex_col()
            .bg(rgb(0x1e1e1e))
            .size_full()
            .p_4()
            .track_focus(&self.focus_handle)
            .on_mouse_down(gpui::MouseButton::Left, cx.listener(|_this, _event, window, cx| {
                cx.focus_self(window);
            }))
            .on_key_down(cx.listener(|this: &mut Self, event: &KeyDownEvent, _window: &mut Window, cx: &mut Context<Self>| {
                // Check for Ctrl+S or Cmd+S (save)
                if event.keystroke.key == "s" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) {
                    match this.save_file() {
                        Ok(_) => {
                            eprintln!("File saved successfully");
                            cx.notify();
                        }
                        Err(e) => {
                            eprintln!("Failed to save file: {}", e);
                            this.save_message = Some(format!("Error: {}", e));
                            cx.notify();
                        }
                    }
                    return;
                }

                // Check for Ctrl+Z or Cmd+Z (undo)
                if event.keystroke.key == "z" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) && !event.keystroke.modifiers.shift {
                    if this.document.undo() {
                        this.save_message = Some("Undo".to_string());
                        cx.notify();
                    }
                    return;
                }

                // Check for Ctrl+Y or Cmd+Y or Ctrl+Shift+Z (redo)
                if (event.keystroke.key == "y" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)) ||
                   (event.keystroke.key == "z" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) && event.keystroke.modifiers.shift) {
                    if this.document.redo() {
                        this.save_message = Some("Redo".to_string());
                        cx.notify();
                    }
                    return;
                }

                match event.keystroke.key.as_str() {
                    "up" => {
                        this.move_cursor_up();
                        cx.notify();
                    }
                    "down" => {
                        this.move_cursor_down();
                        cx.notify();
                    }
                    "left" => {
                        this.move_cursor_left();
                        cx.notify();
                    }
                    "right" => {
                        this.move_cursor_right();
                        cx.notify();
                    }
                    "tab" => {
                        this.toggle_pane();
                        cx.notify();
                    }
                    key => {
                        // Handle character input
                        if key.len() == 1 {
                            let c = key.chars().next().unwrap();
                            match this.edit_pane {
                                EditPane::Hex => {
                                    if c.is_ascii_hexdigit() {
                                        this.input_hex(c);
                                        cx.notify();
                                    }
                                }
                                EditPane::Ascii => {
                                    if c >= ' ' && c <= '~' {
                                        this.input_ascii(c);
                                        cx.notify();
                                    }
                                }
                            }
                        }
                    }
                }
            }))
            .child(
                // Header
                div()
                    .flex()
                    .flex_col()
                    .pb_4()
                    .border_b_1()
                    .border_color(rgb(0x404040))
                    .child(
                        div()
                            .text_xl()
                            .text_color(rgb(0xffffff))
                            .child(format!("{}{}",
                                title.clone(),
                                if self.document.has_unsaved_changes() { " *" } else { "" }
                            ))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child(format!("{} bytes{}",
                                self.document.len(),
                                if self.document.has_unsaved_changes() { " (modified)" } else { "" }
                            ))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child(format!("Edit Mode: {} | Tab: switch | Ctrl+S: save | Ctrl+Z: undo | Ctrl+Y: redo",
                                match self.edit_pane {
                                    EditPane::Hex => "HEX",
                                    EditPane::Ascii => "ASCII",
                                }))
                    )
                    .when(self.save_message.is_some(), |header| {
                        header.child(
                            div()
                                .text_sm()
                                .text_color(rgb(0x00ff00))
                                .child(self.save_message.as_ref().unwrap().clone())
                        )
                    })
            )
            .child(
                // Content area with scrollbar
                div()
                    .flex()
                    .flex_1()
                    .pt_4()
                    .relative()
                    .overflow_hidden()
                    .child(
                        // Scrollable content
                        div()
                            .id("hex-content")
                            .absolute()
                            .top_0()
                            .left_0()
                            .right_0()
                            .bottom_0()
                            .overflow_y_scroll()
                            .track_scroll(&self.scroll_handle)
                            .pr(px(24.0))
                            .child(
                                div()
                                    .flex()
                                    .flex_col()
                                    .font_family("Monaco")
                                    .text_sm()
                                    .children((0..rendered_row_count).map(|row| {
                        let address = self.format_address(row * self.bytes_per_row);
                        let start = row * self.bytes_per_row;
                        let end = (start + self.bytes_per_row).min(self.document.len());
                        let cursor_pos = self.cursor_position;
                        let edit_pane = self.edit_pane;

                        div()
                            .flex()
                            .gap_4()
                            .mb_1()
                            .child(
                                // Address column
                                div()
                                    .w(px(80.0))
                                    .text_color(rgb(0x808080))
                                    .child(address)
                            )
                            .child(
                                // Hex column - each byte separately
                                div()
                                    .flex()
                                    .gap_1()
                                    .flex_1()
                                    .children((start..end).map(|byte_idx| {
                                        let byte = self.document.get_byte(byte_idx).unwrap();
                                        let hex_str = format!("{:02X}", byte);
                                        let is_cursor = byte_idx == cursor_pos;

                                        div()
                                            .when(is_cursor && edit_pane == EditPane::Hex, |div| {
                                                div.bg(rgb(0x4a9eff))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(is_cursor && edit_pane == EditPane::Ascii, |div| {
                                                div.bg(rgb(0x333333))
                                                    .text_color(rgb(0x00ff00))
                                            })
                                            .when(!is_cursor, |div| {
                                                div.text_color(rgb(0x00ff00))
                                            })
                                            .child(hex_str)
                                    }))
                            )
                            .child(
                                // ASCII column - each byte separately
                                div()
                                    .flex()
                                    .w(px(160.0))
                                    .children((start..end).map(|byte_idx| {
                                        let byte = self.document.get_byte(byte_idx).unwrap();
                                        let ascii_char = if byte >= 0x20 && byte <= 0x7E {
                                            byte as char
                                        } else {
                                            '.'
                                        };
                                        let is_cursor = byte_idx == cursor_pos;

                                        div()
                                            .when(is_cursor && edit_pane == EditPane::Ascii, |div| {
                                                div.bg(rgb(0xff8c00))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(is_cursor && edit_pane == EditPane::Hex, |div| {
                                                div.bg(rgb(0x333333))
                                                    .text_color(rgb(0xffffff))
                                            })
                                            .when(!is_cursor, |div| {
                                                div.text_color(rgb(0xffffff))
                                            })
                                            .child(ascii_char.to_string())
                                    }))
                            )
                    }))
                            )
                    )
                    .child(
                        // Scrollbar with visible background
                        div()
                            .absolute()
                            .top_0()
                            .right_0()
                            .bottom_0()
                            .w(px(12.0))
                            .bg(rgb(0x2a2a2a))
                            .child(
                                Scrollbar::vertical(&self.scrollbar_state, &self.scroll_handle)
                                    .scrollbar_show(ScrollbarShow::Always)
                            )
                    )
            )
    }
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = std::env::args().collect();

    Application::new().run(move |cx: &mut App| {
        // Initialize gpui-component theme for Scrollbar support
        gpui_component::theme::init(cx);

        let bounds = Bounds::centered(None, size(px(800.0), px(600.0)), cx);
        cx.open_window(
            WindowOptions {
                window_bounds: Some(WindowBounds::Windowed(bounds)),
                ..Default::default()
            },
            move |window, cx| {
                cx.new(move |cx| {
                    // If file path is provided, load it; otherwise use sample data
                    let editor = if args.len() > 1 {
                        let file_path = PathBuf::from(&args[1]);
                        let mut editor = HexEditor::new(cx);
                        match editor.load_file(file_path.clone()) {
                            Ok(_) => editor,
                            Err(e) => {
                                eprintln!("Failed to load file: {}", e);
                                HexEditor::with_sample_data(cx)
                            }
                        }
                    } else {
                        HexEditor::with_sample_data(cx)
                    };

                    // Set initial focus
                    cx.focus_self(window);
                    editor
                })
            },
        )
        .unwrap();
        cx.activate(true);
    });
}
