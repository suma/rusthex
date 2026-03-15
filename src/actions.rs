//! Action definitions for menu bar and keyboard shortcut integration

use gpui::actions;

actions!(
    rusthex,
    [
        // File
        NewWindow,
        Open,
        OpenInNewWindow,
        Save,
        SaveAs,
        SaveSelectionAs,
        OpenSelectionInNewTab,
        NewTab,
        CloseTab,
        CloseWindow,
        Quit,
        // Edit
        Undo,
        Redo,
        SelectAll,
        Copy,
        Paste,
        ToggleInsertMode,
        GoToAddress,
        // Copy variants
        CopyAsAscii,
        CopyAsHexString,
        CopyAsCArray,
        CopyAsPython,
        // View
        ToggleSearch,
        ToggleInspector,
        ToggleBitmap,
        TogglePatternPanel,
        ToggleCompareMode,
        CycleEncoding,
        ToggleLogPanel,
        ClearLog,
        // Application
        About,
        // Navigation
        NavigateBack,
        NavigateForward,
        // Search
        FindNext,
        FindPrev,
        // Analyze
        AnalyzeSelection,
        AnalyzeBinary,
    ]
);
