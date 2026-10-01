#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

EXE_PATH="${1:-${ROOT_DIR}/_build/default/src/rails_run.exe}"
VERSION="${2:-dev}"
OUTPUT_ZIP="${3:-rails-${VERSION}-windows-x86_64.zip}"

STAGE_DIR="${ROOT_DIR}/stage-windows"
rm -rf "$STAGE_DIR"
mkdir -p "$STAGE_DIR/rails/data"

echo "==> Preparing Windows release package..."
cp "$EXE_PATH" "$STAGE_DIR/rails/rails.exe"

cp -r "${ROOT_DIR}/shaders" "$STAGE_DIR/rails/"
cp -r "${ROOT_DIR}/sound" "$STAGE_DIR/rails/"
cp -r "${ROOT_DIR}/music" "$STAGE_DIR/rails/"

if [ -f "${ROOT_DIR}/data/FONTS.RR" ]; then
    cp "${ROOT_DIR}/data/FONTS.RR" "$STAGE_DIR/rails/data/"
fi
if [ -f "${ROOT_DIR}/data/SPRITES_extra.png" ]; then
    cp "${ROOT_DIR}/data/SPRITES_extra.png" "$STAGE_DIR/rails/data/"
fi
if [ -f "${ROOT_DIR}/data/TRACKS_extra.png" ]; then
    cp "${ROOT_DIR}/data/TRACKS_extra.png" "$STAGE_DIR/rails/data/"
fi

cp "${ROOT_DIR}/packaging/PUT_ORIGINAL_GAME_FILES_HERE.txt" "$STAGE_DIR/rails/data/"
cp "${ROOT_DIR}/packaging/README_RELEASE.txt" "$STAGE_DIR/rails/README.txt"

if [ -f "${ROOT_DIR}/packaging/rails.ico" ]; then
    cp "${ROOT_DIR}/packaging/rails.ico" "$STAGE_DIR/rails/"
fi

# Fetch official SDL2 and SDL2_mixer DLLs if not already provided in STAGE_DIR
TMP_SDL="${ROOT_DIR}/tmp_sdl"
mkdir -p "$TMP_SDL"

SDL2_VER="2.30.10"
MIXER_VER="2.8.0"

if [ ! -f "$STAGE_DIR/rails/SDL2.dll" ]; then
    echo "==> Fetching official SDL2 DLL (v${SDL2_VER})..."
    curl -sL "https://github.com/libsdl-org/SDL/releases/download/release-${SDL2_VER}/SDL2-${SDL2_VER}-win32-x64.zip" -o "$TMP_SDL/sdl2.zip"
    unzip -q -o "$TMP_SDL/sdl2.zip" -d "$TMP_SDL/sdl2"
    cp "$TMP_SDL/sdl2/SDL2.dll" "$STAGE_DIR/rails/"
fi

if [ ! -f "$STAGE_DIR/rails/SDL2_mixer.dll" ]; then
    echo "==> Fetching official SDL2_mixer DLL (v${MIXER_VER})..."
    curl -sL "https://github.com/libsdl-org/SDL_mixer/releases/download/release-${MIXER_VER}/SDL2_mixer-${MIXER_VER}-win32-x64.zip" -o "$TMP_SDL/mixer.zip"
    unzip -q -o "$TMP_SDL/mixer.zip" -d "$TMP_SDL/mixer"
    cp "$TMP_SDL/mixer/SDL2_mixer.dll" "$STAGE_DIR/rails/"
    # Copy codec DLLs from optional/ if present
    if [ -d "$TMP_SDL/mixer/optional" ]; then
        cp "$TMP_SDL/mixer/optional/"*.dll "$STAGE_DIR/rails/"
    fi
fi

# Look for mingw runtime DLLs in MSYS2 or Cygwin MinGW locations
for lib in libwinpthread-1.dll libgcc_s_seh-1.dll libstdc++-6.dll; do
    for dir in \
        /mingw64/bin \
        /c/.opam/.cygwin/root/usr/x86_64-w64-mingw32/sys-root/mingw/bin \
        "C:/.opam/.cygwin/root/usr/x86_64-w64-mingw32/sys-root/mingw/bin" \
        /usr/x86_64-w64-mingw32/sys-root/mingw/bin; do
        if [ -f "$dir/$lib" ]; then
            echo "==> Bundling $lib from $dir"
            cp "$dir/$lib" "$STAGE_DIR/rails/" 2>/dev/null || true
            break
        fi
    done
done

rm -rf "$TMP_SDL"

echo "==> Creating release zip: $OUTPUT_ZIP..."
(cd "$STAGE_DIR" && zip -r -q "${ROOT_DIR}/${OUTPUT_ZIP}" rails)
rm -rf "$STAGE_DIR"
echo "==> Windows release created: ${OUTPUT_ZIP}"
