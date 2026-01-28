# CLAUDE.md

以下日本語のみで書いてください。
This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

このプロジェクトは、RustのGUIライブラリであるgpuiライブラリを用いたバイナリエディタのプロジェクトです。

## 開発計画

1. 基本設計の決定

16進数表示とASCII表示の2ペイン構造
スクロール可能な仮想化リスト（大きなファイルに対応）
アドレス表示カラム、hex表示カラム、ASCII表示カラムの3カラムレイアウト

2. データモデルの実装

ファイルの読み込み・書き込み処理
メモリマップドファイルの検討（大容量ファイル対応）
編集バッファの管理（変更履歴、Undo/Redo）
バイト配列の効率的な管理

3. UI コンポーネントの開発

カスタムビューコンポーネント（HexView、AsciiView）
gpuiのテキストレンダリング機能を活用
カーソル位置の管理と表示
選択範囲のハイライト表示

4. 入力処理の実装

キーボード入力（16進数入力の検証）
マウスクリックによるバイト選択
カーソル移動（矢印キー、Page Up/Down）
コピー＆ペースト機能

5. パフォーマンス最適化

表示領域のみをレンダリング（仮想スクロール）
変更された部分のみを再描画
gpuiのGPUアクセラレーションを活用

6. 追加機能

検索機能（バイナリ/テキスト検索）
ファイル情報の表示
オフセット計算ツール
データインスペクタ（整数、浮動小数点数の表示）

## Build and Development Commands

### Building
```bash
cargo build          # Debug build
cargo build --release # Release build
```

### Running
```bash
cargo run           # Run the application
cargo run --release # Run optimized build
```

### Testing
```bash
cargo test          # Run all tests
cargo test <test_name> # Run a specific test
cargo test -- --nocapture # Run tests with stdout visible
```

### Code Quality
```bash
cargo check         # Quick compilation check without producing binary
cargo clippy        # Run linter
cargo fmt           # Format code
cargo fmt -- --check # Check formatting without applying
```

## 重要な実装ルール

### 縦方向のUIコンポーネント追加時

ヘッダー領域に新しいUIコンポーネント（検索バー、ブックマークバーなど）を追加する場合、**必ず**以下の対応が必要：

1. `calculate_header_height()` 関数に新しいコンポーネントの高さを追加する
2. マウスのY座標計算は `calculate_header_height()` を使用しているため、この関数を更新すれば自動的に反映される

```rust
// 例: 新しいバーを追加する場合
let new_bar = if self.tab().new_bar_visible {
    self.cached_line_height_sm + 20.0
} else {
    0.0
};

base_header + tab_bar + search_bar + bookmark_bar + new_bar
```

これを怠ると、マウスドラッグ選択時のY座標計算がずれる問題が発生する。

## Code Structure

- `src/main.rs` - Entry point with main() function
- No additional modules or library code currently defined

## Dependencies

Currently no external dependencies defined in Cargo.toml.
