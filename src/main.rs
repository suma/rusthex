use gpui::{
    App, Application, Bounds, Context, Window, WindowBounds, WindowOptions,
    div, prelude::*, px, rgb, size,
};

struct HexEditor {
    // 将来的にデータモデルをここに追加
}

impl Render for HexEditor {
    fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl IntoElement {
        div()
            .flex()
            .bg(rgb(0x1e1e1e))
            .size_full()
            .justify_center()
            .items_center()
            .child(
                div()
                    .text_xl()
                    .text_color(rgb(0xffffff))
                    .child("Rust Hex Editor")
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
                cx.new(|_| HexEditor {})
            },
        )
        .unwrap();
        cx.activate(true);
    });
}
