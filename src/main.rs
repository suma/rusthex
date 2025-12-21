use gpui::{
    App, Application, Bounds, Context, Focusable, FocusHandle, KeyDownEvent, Window, WindowBounds, WindowOptions,
    div, prelude::*, px, rgb, size, ScrollHandle,
};
use gpui_component::scroll::{Scrollbar, ScrollbarState, ScrollbarShow};
use std::path::PathBuf;

struct HexEditor {
    data: Vec<u8>,
    bytes_per_row: usize,
    file_path: Option<PathBuf>,
    scroll_handle: ScrollHandle,
    scrollbar_state: ScrollbarState,
    cursor_position: usize,
    focus_handle: FocusHandle,
}

impl HexEditor {
    fn new(cx: &mut Context<Self>) -> Self {
        Self {
            data: Vec::new(),
            bytes_per_row: 16,
            file_path: None,
            scroll_handle: ScrollHandle::new(),
            scrollbar_state: ScrollbarState::default(),
            cursor_position: 0,
            focus_handle: cx.focus_handle(),
        }
    }

    fn load_file(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.data = std::fs::read(&path)?;
        self.file_path = Some(path);
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

        editor.data = data;
        editor
    }

    fn format_address(&self, offset: usize) -> String {
        format!("{:08X}", offset)
    }

    fn row_count(&self) -> usize {
        (self.data.len() + self.bytes_per_row - 1) / self.bytes_per_row
    }

    // Cursor movement methods
    fn move_cursor_left(&mut self) {
        if self.cursor_position > 0 {
            self.cursor_position -= 1;
        }
    }

    fn move_cursor_right(&mut self) {
        if self.cursor_position < self.data.len().saturating_sub(1) {
            self.cursor_position += 1;
        }
    }

    fn move_cursor_up(&mut self) {
        if self.cursor_position >= self.bytes_per_row {
            self.cursor_position -= self.bytes_per_row;
        }
    }

    fn move_cursor_down(&mut self) {
        let new_pos = self.cursor_position + self.bytes_per_row;
        if new_pos < self.data.len() {
            self.cursor_position = new_pos;
        }
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

        // Get display title
        let title = if let Some(path) = &self.file_path {
            path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("Rust Hex Editor")
                .to_string()
        } else {
            "Rust Hex Editor".to_string()
        };

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
                    _ => {}
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
                            .child(title.clone())
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child(format!("{} bytes", self.data.len()))
                    )
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
                                    .children((0..row_count).map(|row| {
                        let address = self.format_address(row * self.bytes_per_row);
                        let start = row * self.bytes_per_row;
                        let end = (start + self.bytes_per_row).min(self.data.len());
                        let cursor_pos = self.cursor_position;

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
                                        let byte = self.data[byte_idx];
                                        let hex_str = format!("{:02X}", byte);
                                        let is_cursor = byte_idx == cursor_pos;

                                        div()
                                            .when(is_cursor, |div| {
                                                div.bg(rgb(0x4a9eff))
                                                    .text_color(rgb(0x000000))
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
                                        let byte = self.data[byte_idx];
                                        let ascii_char = if byte >= 0x20 && byte <= 0x7E {
                                            byte as char
                                        } else {
                                            '.'
                                        };
                                        let is_cursor = byte_idx == cursor_pos;

                                        div()
                                            .when(is_cursor, |div| {
                                                div.bg(rgb(0x4a9eff))
                                                    .text_color(rgb(0x000000))
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
                        match editor.load_file(file_path) {
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
