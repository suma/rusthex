//! Windows native menu bar implementation
//!
//! gpui 0.2.2 uses Direct Composition with `WS_EX_NOREDIRECTIONBITMAP` and a
//! topmost visual (`CreateTargetForHwnd(hwnd, true)`), which covers any Win32
//! HMENU attached via `SetMenu`.  This module works around that by:
//! 1. Building Win32 popup HMENUs from gpui `Menu` definitions
//! 2. Rendering a custom menu bar strip in gpui (see `render_menu_bar`)
//! 3. Showing the popup via `TrackPopupMenu` on click
//! 4. Subclassing the window to intercept `WM_COMMAND` and queue gpui `Action`s

use std::cell::RefCell;
use std::collections::HashMap;

use gpui::{Action, Menu, MenuItem, Window};
use raw_window_handle::{HasWindowHandle, RawWindowHandle};
use windows::core::PCWSTR;
use windows::Win32::Foundation::{HWND, LPARAM, LRESULT, POINT, WPARAM};
use windows::Win32::Graphics::Gdi::InvalidateRect;
use windows::Win32::UI::Shell::{DefSubclassProc, SetWindowSubclass};
use windows::Win32::UI::WindowsAndMessaging::{
    AppendMenuW, CreatePopupMenu, GetCursorPos, TrackPopupMenu, MF_POPUP, MF_SEPARATOR, MF_STRING,
    TPM_LEFTALIGN, TPM_TOPALIGN, WM_COMMAND,
};

/// Unique ID for our window subclass (ASCII "RHEX").
const SUBCLASS_ID: usize = 0x5248_4558;

/// First command ID assigned to menu items.
const FIRST_COMMAND_ID: u16 = 1000;

/// State for the gpui-rendered menu bar (thread-local, no Send needed).
struct MenuBarState {
    hwnd: HWND,
    popups: Vec<(String, windows::Win32::UI::WindowsAndMessaging::HMENU)>,
}

thread_local! {
    /// Pending actions queued by the subclass proc, drained each render().
    static PENDING_ACTIONS: RefCell<Vec<Box<dyn Action>>> = RefCell::new(Vec::new());
    /// Command-ID -> cloned gpui Action mapping, populated at menu creation time.
    static MENU_ACTION_MAP: RefCell<HashMap<u16, Box<dyn Action>>> = RefCell::new(HashMap::new());
    /// Popup menus and HWND stored at install time.
    static MENU_BAR_DATA: RefCell<Option<MenuBarState>> = RefCell::new(None);
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Drain all pending actions that were queued by Win32 menu commands.
pub fn drain_pending_actions() -> Vec<Box<dyn Action>> {
    PENDING_ACTIONS.with(|v| v.borrow_mut().drain(..).collect())
}

/// Return the labels for the top-level menus (e.g. "File", "Edit", "View").
pub fn menu_labels() -> Vec<String> {
    MENU_BAR_DATA.with(|data| {
        data.borrow()
            .as_ref()
            .map_or_else(Vec::new, |state| {
                state.popups.iter().map(|(label, _)| label.clone()).collect()
            })
    })
}

/// Show a popup menu at the current cursor position.
///
/// Called from gpui's click handler when the user clicks a menu label.
/// `TrackPopupMenu` runs its own message pump, so the call blocks until the
/// user selects an item or dismisses the popup.  `WM_COMMAND` is sent to the
/// subclassed HWND and handled by `menu_subclass_proc`.
pub fn show_popup_menu(menu_index: usize) {
    MENU_BAR_DATA.with(|data| {
        let data = data.borrow();
        let Some(state) = data.as_ref() else { return };
        let Some((_, hmenu)) = state.popups.get(menu_index) else { return };

        unsafe {
            let mut pt = POINT::default();
            let _ = GetCursorPos(&mut pt);
            let _ = TrackPopupMenu(
                *hmenu,
                TPM_LEFTALIGN | TPM_TOPALIGN,
                pt.x,
                pt.y,
                Some(0),
                state.hwnd,
                None,
            );
        }
    })
}

/// Build popup menus from gpui `Menu` definitions and subclass the window.
///
/// Must be called while the `Window` reference is available (e.g. inside
/// `cx.open_window()`).  On non-Win32 platforms this is a no-op guarded by
/// `#[cfg(target_os = "windows")]` at the call-site.
pub fn install_native_menu(window: &Window, menus: &[Menu]) {
    let hwnd = match get_hwnd(window) {
        Some(h) => h,
        None => return,
    };

    unsafe {
        let mut next_id = FIRST_COMMAND_ID;
        let mut popups = Vec::new();

        for menu in menus {
            // Skip menus that contain only SystemMenu / Separator items
            // (e.g. the macOS-specific "rusthex" application menu with Services).
            if menu
                .items
                .iter()
                .all(|i| matches!(i, MenuItem::SystemMenu(_) | MenuItem::Separator))
            {
                continue;
            }

            let popup = match CreatePopupMenu() {
                Ok(p) => p,
                Err(_) => continue,
            };

            build_popup_items(popup, &menu.items, &mut next_id);
            popups.push((menu.name.to_string(), popup));
        }

        // Store popup menus for later use by show_popup_menu().
        // Do NOT call SetMenu() — the DirectComposition topmost visual would
        // cover the Win32 HMENU.  The menu bar is rendered in gpui instead.
        MENU_BAR_DATA.with(|data| {
            *data.borrow_mut() = Some(MenuBarState { hwnd, popups });
        });

        let _ = SetWindowSubclass(hwnd, Some(menu_subclass_proc), SUBCLASS_ID, 0);
    }
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

/// Extract a Win32 HWND from a gpui `Window` via `raw-window-handle`.
fn get_hwnd(window: &Window) -> Option<HWND> {
    let handle = HasWindowHandle::window_handle(window).ok()?;
    match handle.as_raw() {
        RawWindowHandle::Win32(win32) => Some(HWND(win32.hwnd.get() as *mut _)),
        _ => None,
    }
}

/// Convert HMENU to the `usize` that `AppendMenuW` expects for `MF_POPUP`.
fn hmenu_as_usize(h: windows::Win32::UI::WindowsAndMessaging::HMENU) -> usize {
    h.0 as usize
}

/// Encode a Rust `&str` to a null-terminated UTF-16 `Vec<u16>` for Win32 APIs.
fn to_wide(s: &str) -> Vec<u16> {
    s.encode_utf16().chain(std::iter::once(0)).collect()
}

/// Recursively populate a Win32 popup menu from gpui `MenuItem`s.
unsafe fn build_popup_items(
    popup: windows::Win32::UI::WindowsAndMessaging::HMENU,
    items: &[MenuItem],
    next_id: &mut u16,
) {
    for item in items {
        match item {
            MenuItem::Separator => {
                let _ = unsafe { AppendMenuW(popup, MF_SEPARATOR, 0, PCWSTR::null()) };
            }
            MenuItem::Action { name, action, .. } => {
                let cmd_id = *next_id;
                *next_id = next_id.saturating_add(1);

                MENU_ACTION_MAP.with(|map| {
                    map.borrow_mut().insert(cmd_id, action.boxed_clone());
                });

                let label = match shortcut_for_action(action.name()) {
                    Some(sc) => format!("{}\t{}", name, sc),
                    None => name.to_string(),
                };
                let wide = to_wide(&label);
                let _ = unsafe {
                    AppendMenuW(
                        popup,
                        MF_STRING,
                        cmd_id as usize,
                        PCWSTR::from_raw(wide.as_ptr()),
                    )
                };
            }
            MenuItem::Submenu(submenu) => {
                if let Ok(sub_popup) = unsafe { CreatePopupMenu() } {
                    unsafe { build_popup_items(sub_popup, &submenu.items, next_id) };
                    let wide = to_wide(&submenu.name);
                    let _ = unsafe {
                        AppendMenuW(
                            popup,
                            MF_STRING | MF_POPUP,
                            hmenu_as_usize(sub_popup),
                            PCWSTR::from_raw(wide.as_ptr()),
                        )
                    };
                }
            }
            // macOS-only system menus (Services etc.) have no Win32 equivalent.
            MenuItem::SystemMenu(_) => {}
        }
    }
}

/// Map gpui action type names to Windows-style shortcut display strings.
fn shortcut_for_action(action_name: &str) -> Option<&'static str> {
    match action_name {
        "rusthex::Open" => Some("Ctrl+O"),
        "rusthex::Save" => Some("Ctrl+S"),
        "rusthex::SaveAs" => Some("Ctrl+Shift+S"),
        "rusthex::SaveSelectionAs" => None,
        "rusthex::NewTab" => Some("Ctrl+T"),
        "rusthex::CloseTab" => Some("Ctrl+W"),
        "rusthex::Quit" => Some("Ctrl+Q"),
        "rusthex::Undo" => Some("Ctrl+Z"),
        "rusthex::Redo" => Some("Ctrl+Y"),
        "rusthex::Copy" => Some("Ctrl+C"),
        "rusthex::Paste" => Some("Ctrl+V"),
        "rusthex::SelectAll" => Some("Ctrl+A"),
        "rusthex::ToggleInsertMode" => Some("Ctrl+Shift+I"),
        "rusthex::GoToAddress" => Some("Ctrl+G"),
        "rusthex::ToggleSearch" => Some("Ctrl+F"),
        "rusthex::ToggleInspector" => Some("Ctrl+I"),
        "rusthex::ToggleBitmap" => Some("Ctrl+M"),
        "rusthex::TogglePatternPanel" => Some("Ctrl+P"),
        "rusthex::ToggleCompareMode" => Some("Ctrl+K"),
        "rusthex::CycleEncoding" => Some("Ctrl+Shift+E"),
        "rusthex::FindNext" => Some("F3"),
        "rusthex::FindPrev" => Some("Shift+F3"),
        _ => None,
    }
}

/// Window subclass procedure that intercepts `WM_COMMAND` from the menu bar.
unsafe extern "system" fn menu_subclass_proc(
    hwnd: HWND,
    msg: u32,
    wparam: WPARAM,
    lparam: LPARAM,
    _uid_subclass: usize,
    _ref_data: usize,
) -> LRESULT {
    if msg == WM_COMMAND {
        let cmd_id = (wparam.0 & 0xFFFF) as u16;
        let notification = ((wparam.0 >> 16) & 0xFFFF) as u16;

        // notification == 0 means the WM_COMMAND originates from a menu item.
        if notification == 0 {
            let found = MENU_ACTION_MAP.with(|map| {
                map.borrow().get(&cmd_id).map(|a| a.boxed_clone())
            });
            if let Some(action) = found {
                PENDING_ACTIONS.with(|v| v.borrow_mut().push(action));
                // Request a repaint so gpui calls render() and we can dispatch.
                let _ = unsafe { InvalidateRect(Some(hwnd), None, false) };
                return LRESULT(0);
            }
        }
    }

    unsafe { DefSubclassProc(hwnd, msg, wparam, lparam) }
}
