@echo off
setlocal

echo === RustHex Build Script for Windows ===

:: Rustがインストールされているか確認
where cargo >nul 2>nul
if %errorlevel% neq 0 (
    echo Error: Cargo not found. Please install Rust from https://rustup.rs/
    exit /b 1
)

:: ビルドタイプの選択
if "%1"=="release" (
    echo Building release version...
    cargo build --release
    if %errorlevel% equ 0 (
        echo Build successful: target\release\rusthex.exe
    )
) else if "%1"=="run" (
    echo Running application...
    cargo run
) else if "%1"=="test" (
    echo Running tests...
    cargo test
) else if "%1"=="check" (
    echo Running cargo check...
    cargo check
) else if "%1"=="clean" (
    echo Cleaning build artifacts...
    cargo clean
) else if "%1"=="fmt" (
    echo Formatting code...
    cargo fmt
) else if "%1"=="clippy" (
    echo Running clippy...
    cargo clippy
) else (
    echo Building debug version...
    cargo build
    if %errorlevel% equ 0 (
        echo Build successful: target\debug\rusthex.exe
    )
)

endlocal
