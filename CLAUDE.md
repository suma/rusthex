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

## 開発スタイル: TDD（テスト駆動開発）

本プロジェクトではTDD（Test-Driven Development）を推進する。新しい機能の追加やバグ修正を行う際は、以下のサイクルに従うこと。

### Red → Green → Refactor サイクル

1. **Red**: まず失敗するテストを書く。期待する振る舞いをテストとして定義する
2. **Green**: テストが通る最小限の実装を行う
3. **Refactor**: テストが通る状態を維持しながらコードを整理する

### ガイドライン

- 新しい機能を実装する前に、その機能の振る舞いを検証するテストを先に書く
- バグを修正する際は、まずそのバグを再現するテストを書いてから修正する
- テストは小さく、1つの振る舞いのみを検証するようにする
- テストが通らない状態で別の機能の実装に着手しない
- リファクタリング時は既存テストがすべて通ることを確認してから行う
- テストの配置はRustの慣習に従う

## 重要な実装ルール

### 縦方向のUIコンポーネント追加時

ヘッダー領域に新しいUIコンポーネント（検索バー、ブックマークバーなど）を追加する場合、**必ず**以下の対応が必要：

1. `calculate_header_height()` 関数に新しいコンポーネントの高さを追加する
2. マウスのY座標計算は `calculate_header_height()` を使用しているため、この関数を更新すれば自動的に反映される

```rust
// 例: py_2 + text_sm + text_xs + border_b_1 のバーを追加する場合
let new_bar = if self.tab().new_bar_visible {
    self.cached_line_height_sm + self.cached_line_height_xs + 17.0
} else {
    0.0
};

base_header + tab_bar + search_bar + bookmark_bar + new_bar
```

これを怠ると、マウスドラッグ選択時のY座標計算がずれる問題が発生する。

### gpuiの行高さはphi（黄金比）ベース

gpui 0.2.2のデフォルト行高さは `font_size × φ (1.618034)` で計算される（`gpui/src/style.rs` の `TextStyle` デフォルト）。

**`ascent + descent` をフォントメトリクスから取得して使ってはならない。** この値はOS（macOS Core Text / Windows DirectWrite）によって異なり、gpuiの実際のレンダリング行高さと一致しない。

```rust
const PHI: f32 = 1.618034;

// 正しい: gpuiの実際の行高さに一致
self.cached_row_height = (font_size * PHI).round() + 4.0; // + mb_1 margin

// 間違い: プラットフォーム依存で実際のレンダリングとずれる
self.cached_row_height = ascent + descent.abs() + 4.0;
```

text_xl / text_sm / text_xs も同様に phi ベースで計算する:
- `text_xl` = rems(1.25) = 20px → 行高さ `(20 * PHI).round()` = 32px
- `text_sm` = rems(0.875) = 14px → 行高さ `(14 * PHI).round()` = 23px
- `text_xs` = rems(0.75) = 12px → 行高さ `(12 * PHI).round()` = 19px

### マウスドラッグは root div の on_mouse_move で処理する

gpuiの `on_mouse_move` はマウスが**その要素のヒットボックス上にある場合のみ**発火する（`hitbox.is_hovered()` チェック、`gpui/src/elements/div.rs`）。

hex-content div のような内部要素にドラッグ処理を配置すると、マウスが要素外（ヘッダーやステータスバー）に移動した時点でイベントが届かなくなる。

**ドラッグ選択は root div（`size_full()`）の `on_mouse_move` で処理すること。** root div はウィンドウ全体をカバーするため、マウスがどこにあってもハンドラが発火する。

### scroll_handle.bounds() はスクロールコンテナの境界を返す

`scroll_handle.bounds()` はウィンドウ全体のサイズではなく、`track_scroll()` を設定した要素（hex-content div）のレイアウト境界を返す。

この境界はflex layoutで配置された結果であり、**ヘッダーやステータスバーの高さは既に含まれていない**。ビューポート高さからヘッダー・ステータスバー分を二重に引いてはならない。

```rust
let viewport_height = self.tab().scroll_handle.bounds().size.height;

// 正しい: viewport_height は既にコンテンツ領域の高さ
let content_height = viewport_height.max(px(20.0));

// 間違い: 二重減算で visible_rows が過小になる
let content_height = px((viewport_height - non_content_height).max(20.0));
```

### gpuiのテキスト要素幅は `.ceil()` で切り上げられる

gpui 0.2.2 の `elements/text.rs` (419行目) では、テキスト要素のサイズ計算時に幅が `.ceil()` される：

```rust
size.width = size.width.max(line_size.width).ceil();
```

**明示的な `.w()` を持たない div がテキストのみを含む場合、その描画幅は `ceil(text_layout_width)` になる。** `em_advance * N` と一致しないため、座標のヒットテスト計算でずれが生じる。

```rust
// 正しい: gpuiの実際のdiv幅に一致
let hex_byte_width = (char_width * 2.0).ceil();       // "AB" → ceil(20.4) = 21px
let address_width = (char_width * addr_chars).ceil();  // "00000000" → ceil(81.6) = 82px
let ascii_char_width = char_width.ceil();              // "A" → ceil(10.2) = 11px

// 間違い: em_advance × 文字数で計算（実際の描画幅より小さい）
let hex_byte_width = char_width * 2.0;     // 20.4px（実際は21px）
let address_width = char_width * addr_chars; // 81.6px（実際は82px）
```

この差は1要素あたり小さいが、16バイト行で累積すると `0.6px × 16 = 9.6px` のずれとなり、ASCIIカラムのドラッグ選択位置が1〜2バイトずれる原因になる。`HitTestLayout` 構造体（`src/ui.rs`）に座標変換が集約されているので、修正箇所はそこに限定される。

### ソースコード中のコメントとgitコミットメッセージは英語で記述する

ソースコード内のコメント（`//` や `/* */`）およびgitのコミットメッセージは**英語**で記述すること。CLAUDE.mdの説明文やユーザーとの会話は日本語のままでよい。

### ドラッグハンドラの座標計算にハードコード値を使わない

ドラッグ時のX/Y座標計算では、フォントメトリクスに依存する値（アドレスカラム幅、行高さ等）をハードコードせず、`cached_char_width` や `cached_row_height` などのキャッシュ値を使用すること。

```rust
// 正しい: フォントに応じて動的に計算
let address_width = char_width * 8.0;
let hex_start = outer_padding + address_width + 16.0; // + gap_4

// 間違い: 特定の char_width (10.0) を仮定
let hex_start = 112.0; // 16 + 80 + 16
```

### 描画レイアウト変更時はドラッグハンドラのX座標計算を必ず同期する

行レイアウト（アドレスカラム・Hexカラム・ASCIIカラム）の描画部分に変更を加えた場合、**必ず** root div の `on_mouse_move` 内にあるドラッグハンドラのX座標→カーソル位置変換ロジックも同時に更新すること。Hex ペインと ASCII ペインの**両方**の計算パスを確認する必要がある。

描画とドラッグハンドラで座標計算の前提が食い違うと、マウスドラッグ選択時にカーソル位置がずれるバグが発生する。

確認すべき値の例：
- アドレスカラム幅（ブックマークインジケータ 12px + `ceil(char_width * addr_chars)`）
- Hexバイト幅（`ceil(char_width * 2) + gap_1` per byte）
- カラム間の gap（`gap_4` = 16px）
- ASCIIカラムの開始位置（上記すべての累積）
- ASCII文字幅（`ceil(char_width)` per char）

現在はこれらの計算は `HitTestLayout` 構造体（`src/ui.rs`）に集約されており、`render.rs` のドラッグハンドラは `HitTestLayout::byte_position_from_mouse()` を呼ぶだけでよい。

```rust
// 正しい: HitTestLayout を構築して座標変換を委譲
let layout = ui::HitTestLayout { outer_padding, header_height, ... };
let byte_pos = layout.byte_position_from_mouse(mouse_x, mouse_y, pane);

// 間違い: render.rs 内にインラインで座標計算を書く（描画と乖離しやすい）
```
