#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

EXE_PATH="${1:-_build/default/src/rails_run.exe}"
VERSION="${2:-dev}"
OUTPUT_ZIP="${3:-rails-${VERSION}-windows-x86_64.zip}"

STAGE_DIR="stage-windows"
rm -rf "$STAGE_DIR"
mkdir -p "$STAGE_DIR/rails/data"

echo "==> Preparing Windows release package..."
cp "$EXE_PATH" "$STAGE_DIR/rails/rails.exe"

cp -r "shaders" "$STAGE_DIR/rails/"
cp -r "sound" "$STAGE_DIR/rails/"
cp -r "music" "$STAGE_DIR/rails/"

if [ -f "data/FONTS.RR" ]; then
    cp "data/FONTS.RR" "$STAGE_DIR/rails/data/"
fi
if [ -f "data/SPRITES_extra.png" ]; then
    cp "data/SPRITES_extra.png" "$STAGE_DIR/rails/data/"
fi
if [ -f "data/TRACKS_extra.png" ]; then
    cp "data/TRACKS_extra.png" "$STAGE_DIR/rails/data/"
fi

cp "packaging/PUT_ORIGINAL_GAME_FILES_HERE.txt" "$STAGE_DIR/rails/data/"
cp "packaging/README_RELEASE.txt" "$STAGE_DIR/rails/README.txt"

if [ -f "packaging/rails.ico" ]; then
    cp "packaging/rails.ico" "$STAGE_DIR/rails/"
fi

# 1. Check if SDL2.dll already exists in MinGW sysroot
for dir in \
    /usr/x86_64-w64-mingw32/sys-root/mingw/bin \
    /c/.opam/.cygwin/root/usr/x86_64-w64-mingw32/sys-root/mingw/bin \
    "C:/.opam/.cygwin/root/usr/x86_64-w64-mingw32/sys-root/mingw/bin" \
    /mingw64/bin; do
    if [ ! -f "$STAGE_DIR/rails/SDL2.dll" ] && [ -f "$dir/SDL2.dll" ]; then
        echo "==> Using SDL2.dll from $dir"
        cp "$dir/SDL2.dll" "$STAGE_DIR/rails/"
    fi
done

# 2. Download official DLLs if not found above
TMP_SDL="./tmp_sdl_download"
SDL2_VER="2.30.10"
MIXER_VER="2.8.0"

if [ ! -f "$STAGE_DIR/rails/SDL2.dll" ]; then
    echo "==> Fetching official SDL2 DLL (v${SDL2_VER})..."
    mkdir -p "$TMP_SDL"
    curl -sL "https://github.com/libsdl-org/SDL/releases/download/release-${SDL2_VER}/SDL2-${SDL2_VER}-win32-x64.zip" -o "$TMP_SDL/sdl2.zip"
    unzip -q -o "$TMP_SDL/sdl2.zip" -d "$TMP_SDL/sdl2"
    cp "$TMP_SDL/sdl2/SDL2.dll" "$STAGE_DIR/rails/"
fi

if [ ! -f "$STAGE_DIR/rails/SDL2_mixer.dll" ]; then
    echo "==> Fetching official SDL2_mixer DLL (v${MIXER_VER})..."
    mkdir -p "$TMP_SDL"
    curl -sL "https://github.com/libsdl-org/SDL_mixer/releases/download/release-${MIXER_VER}/SDL2_mixer-${MIXER_VER}-win32-x64.zip" -o "$TMP_SDL/mixer.zip"
    unzip -q -o "$TMP_SDL/mixer.zip" -d "$TMP_SDL/mixer"
    cp "$TMP_SDL/mixer/SDL2_mixer.dll" "$STAGE_DIR/rails/"
    if [ -d "$TMP_SDL/mixer/optional" ]; then
        cp "$TMP_SDL/mixer/optional/"*.dll "$STAGE_DIR/rails/"
    fi
fi
rm -rf "$TMP_SDL"

# 3. Look for mingw runtime DLLs
for lib in libwinpthread-1.dll libgcc_s_seh-1.dll libstdc++-6.dll libffi-6.dll; do
    for dir in \
        /usr/x86_64-w64-mingw32/sys-root/mingw/bin \
        /c/.opam/.cygwin/root/usr/x86_64-w64-mingw32/sys-root/mingw/bin \
        "C:/.opam/.cygwin/root/usr/x86_64-w64-mingw32/sys-root/mingw/bin" \
        /mingw64/bin; do
        if [ -f "$dir/$lib" ]; then
            echo "==> Bundling $lib from $dir"
            cp "$dir/$lib" "$STAGE_DIR/rails/" 2>/dev/null || true
            break
        fi
    done
done

echo "==> Bundled files in release package:"
ls -la "$STAGE_DIR/rails"

echo "==> Creating release zip: $OUTPUT_ZIP..."
rm -f "$OUTPUT_ZIP"
if command -v zip >/dev/null 2>&1; then
    (cd "$STAGE_DIR" && zip -r -q "../${OUTPUT_ZIP}" rails)
elif command -v 7z >/dev/null 2>&1; then
    (cd "$STAGE_DIR" && 7z a "../${OUTPUT_ZIP}" rails)
elif command -v python3 >/dev/null 2>&1; then
    (cd "$STAGE_DIR" && python3 -m zipfile -c "../${OUTPUT_ZIP}" rails)
elif command -v python >/dev/null 2>&1; then
    (cd "$STAGE_DIR" && python -m zipfile -c "../${OUTPUT_ZIP}" rails)
elif command -v powershell.exe >/dev/null 2>&1; then
    powershell.exe -Command "Compress-Archive -Path '$STAGE_DIR/rails' -DestinationPath '$OUTPUT_ZIP' -Force"
fi

rm -rf "$STAGE_DIR"
echo "==> Windows release created: ${OUTPUT_ZIP}"
