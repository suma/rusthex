use gpui::{
    App, Application, Bounds, Context, Window, WindowBounds, WindowOptions,
    div, prelude::*, px, rgb, size,
};
use std::path::PathBuf;

struct HexEditor {
    data: Vec<u8>,
    bytes_per_row: usize,
    file_path: Option<PathBuf>,
}

impl HexEditor {
    fn new() -> Self {
        Self {
            data: Vec::new(),
            bytes_per_row: 16,
            file_path: None,
        }
    }

    fn load_file(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.data = std::fs::read(&path)?;
        self.file_path = Some(path);
        Ok(())
    }

    fn with_sample_data() -> Self {
        let mut editor = Self::new();
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

    fn format_hex_line(&self, row: usize) -> String {
        let start = row * self.bytes_per_row;
        let end = (start + self.bytes_per_row).min(self.data.len());

        if start >= self.data.len() {
            return String::new();
        }

        self.data[start..end]
            .iter()
            .map(|b| format!("{:02X}", b))
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn format_ascii_line(&self, row: usize) -> String {
        let start = row * self.bytes_per_row;
        let end = (start + self.bytes_per_row).min(self.data.len());

        if start >= self.data.len() {
            return String::new();
        }

        self.data[start..end]
            .iter()
            .map(|&b| {
                if b >= 0x20 && b <= 0x7E {
                    b as char
                } else {
                    '.'
                }
            })
            .collect()
    }

    fn row_count(&self) -> usize {
        (self.data.len() + self.bytes_per_row - 1) / self.bytes_per_row
    }
}

impl Render for HexEditor {
    fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl IntoElement {
        let row_count = self.row_count();

        div()
            .flex()
            .flex_col()
            .bg(rgb(0x1e1e1e))
            .size_full()
            .p_4()
            .child(
                // Header
                div()
                    .flex()
                    .pb_4()
                    .border_b_1()
                    .border_color(rgb(0x404040))
                    .child(
                        div()
                            .text_xl()
                            .text_color(rgb(0xffffff))
                            .child("Rust Hex Editor")
                    )
            )
            .child(
                // Content area
                div()
                    .flex()
                    .flex_col()
                    .flex_1()
                    .pt_4()
                    .id("hex-content")
                    .overflow_scroll()
                    .child(
                        div()
                            .flex()
                            .flex_col()
                            .font_family("Monaco")
                            .text_sm()
                            .children((0..row_count).map(|row| {
                        let address = self.format_address(row * self.bytes_per_row);
                        let hex_line = self.format_hex_line(row);
                        let ascii_line = self.format_ascii_line(row);

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
                                // Hex column
                                div()
                                    .flex_1()
                                    .text_color(rgb(0x00ff00))
                                    .child(hex_line)
                            )
                            .child(
                                // ASCII column
                                div()
                                    .w(px(160.0))
                                    .text_color(rgb(0xffffff))
                                    .child(ascii_line)
                            )
                    }))
                    )
            )
    }
}

fn main() {
    Application::new().run(|cx: &mut App| {
        let bounds = Bounds::centered(None, size(px(800.0), px(600.0)), cx);
        cx.open_window(
            WindowOptions {
                window_bounds: Some(WindowBounds::Windowed(bounds)),
                ..Default::default()
            },
            |_, cx| {
                cx.new(|_| HexEditor::with_sample_data())
            },
        )
        .unwrap();
        cx.activate(true);
    });
}
