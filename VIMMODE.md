# Vim Mode

RustHex includes an optional Vim-like keybinding mode. Enable it by setting `vim_mode = true` in the `[editor]` section of `config.toml`. When disabled (the default), existing behavior is completely unchanged.

## Enabling

Add the following to `config.toml`:

```toml
[editor]
vim_mode = true
```

## Modes

Vim mode provides four modes. The current mode is displayed at the left edge of the status bar.

| Mode | Display | Color | Description |
|------|---------|-------|-------------|
| Normal | `NORMAL` | Green | Navigation and operator commands. Default on startup |
| Insert | `INSERT` | Blue | Byte input (existing Hex/ASCII input works as-is) |
| Visual | `VISUAL` | Orange | Range selection |
| Command | `:cmd█` | Accent | Ex command input |

## Keybindings

### Cmd/Ctrl Shortcuts (all modes)

Platform shortcuts such as Cmd+S (save), Cmd+O (open), Cmd+Z (undo), and Cmd+F (search) always work regardless of Vim mode.

---

### Normal Mode

#### Movement

| Key | Action |
|-----|--------|
| `h` | Move left 1 byte |
| `l` | Move right 1 byte |
| `j` | Move down 1 row (bytes_per_row bytes) |
| `k` | Move up 1 row |
| `0` | Go to start of current row |
| `$` | Go to end of current row |
| `w` | Jump forward to next 8-byte boundary |
| `b` | Jump backward to previous 8-byte boundary |
| `gg` | Go to start of file |
| `G` | Go to end of file |
| `{count}G` | Jump to byte offset (e.g. `256G`) |
| `Ctrl+f` | Page Down |
| `Ctrl+b` | Page Up |

#### Count Prefix

Type `1`-`9` to build a numeric prefix. The following motion command repeats that many times.

- `3j` — move down 3 rows
- `10l` — move right 10 bytes
- `0` — treated as a digit while building a count; otherwise moves to start of row

#### Mode Switching

| Key | Action |
|-----|--------|
| `i` | Enter Insert mode at current position |
| `a` | Enter Insert mode after current position (cursor moves right 1 byte) |
| `I` | Enter Insert mode at start of row |
| `A` | Enter Insert mode at end of row |
| `v` | Enter Visual mode (start selection at cursor) |
| `/` | Open search bar |
| `:` | Enter Command mode |

#### Editing

| Key | Action |
|-----|--------|
| `x` | Delete byte at cursor (copied to yank buffer) |
| `r` + key | Replace 1 byte. Hex pane: enter a hex digit (e.g. `ra` → `0xAA`). ASCII pane: write the character directly |
| `dd` | Delete entire row (bytes_per_row bytes) and yank |
| `yy` | Yank (copy) entire row |
| `cc` | Delete entire row, yank, and enter Insert mode |
| `d` + motion | Delete motion range and yank (e.g. `dw`, `d$`, `dG`) |
| `y` + motion | Yank motion range (e.g. `yw`, `y0`) |
| `c` + motion | Delete motion range, yank, and enter Insert mode |
| `p` | Paste yank buffer **after** cursor |
| `P` | Paste yank buffer **before** cursor |
| `u` | Undo |
| `Ctrl+r` | Redo |

#### Search Navigation

| Key | Action |
|-----|--------|
| `n` | Jump to next search result |
| `N` | Jump to previous search result |

---

### Insert Mode

In Insert mode, RustHex's existing input handling is used directly:

- **Hex pane**: type hex digits (0-9, a-f) to edit bytes
- **ASCII pane**: type printable ASCII characters to edit bytes
- Arrow keys, Page Up/Down, Home/End work as usual

| Key | Action |
|-----|--------|
| `Esc` | Return to Normal mode (cursor moves left 1 byte, per Vim convention) |

---

### Visual Mode

Press `v` to enter Visual mode. The cursor position at that point becomes the selection anchor. Move the cursor to extend the selection.

#### Movement (same as Normal mode)

`h`, `l`, `j`, `k`, `0`, `$`, `w`, `b`, `gg`, `G`, `Ctrl+f`, `Ctrl+b`

Count prefixes are supported.

#### Selection Operations

| Key | Action |
|-----|--------|
| `y` | Yank selection → return to Normal mode |
| `d` / `x` | Delete selection and yank → return to Normal mode |
| `Esc` | Clear selection → return to Normal mode |

---

### Command Mode

Press `:` to enter Command mode. A `:` prompt appears in the status bar where you can type commands.

#### Supported Commands

| Command | Action |
|---------|--------|
| `:w` | Save file |
| `:q` | Quit (warns if there are unsaved changes) |
| `:wq` / `:x` | Save and quit |
| `:q!` | Force quit (discard unsaved changes) |
| `:{decimal}` | Jump to byte offset (e.g. `:1024`) |
| `:0x{hex}` | Jump to hex byte offset (e.g. `:0xFF00`) |

#### Key Handling

| Key | Action |
|-----|--------|
| `Enter` | Execute command |
| `Backspace` | Delete one character (if empty, return to Normal mode) |
| `Esc` | Cancel and return to Normal mode |

---

## Operators + Motions

Vim's "verb + object" composition is supported.

**Operators (verbs)**: `d` (delete), `y` (yank), `c` (change)

**Motions (objects)**: `h`, `l`, `j`, `k`, `w`, `b`, `0`, `$`, `G`

### Examples

| Input | Action |
|-------|--------|
| `dw` | Delete to next 8-byte boundary |
| `d$` | Delete to end of row |
| `y3j` | Yank 3 rows down |
| `cw` | Delete to next 8-byte boundary and enter Insert mode |
| `dd` | Delete entire row (double-tap operator = line-wise operation) |

---

## Per-Tab State

Vim state (mode, count prefix, yank buffer, etc.) is independent per tab. Switching tabs preserves each tab's mode.

## Behavior During UI Input

When a UI input field is active (search bar, Go to Address bar, bookmark comment editor, pattern dropdown), Vim keybindings are bypassed and normal input handling is used.

## Yank Buffer

Vim mode has its own yank buffer. Bytes deleted or copied via `y`, `d`, `x` are stored here and can be pasted with `p` / `P`. This is independent of the system clipboard (Cmd+C / Cmd+V continue to use the system clipboard).
