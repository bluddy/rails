Railroad Tycoon Remake (OCaml)
===============================

An open-source remake of the classic Railroad Tycoon (1990).

HOW TO RUN:
-----------
1. Copy your original Railroad Tycoon DOS files (.PIC, .PAN, .DTA) into the
   'data' folder.
2. Launch the game:
   - Windows: Double-click 'rails.exe'
   - Linux: Run './rails' or double-click the .AppImage
   - macOS: Open 'Rails.app' or run './rails'

COMMAND-LINE OPTIONS:
---------------------
  --zoom N         Display zoom multiplier (default: 3)
  --shader NAME    Shader from shaders/*.glsl (e.g. test, crt-hyllian, vga-1080p)
  --no-adjust-ar   Disable rectangular pixel aspect ratio adjustment
  --no-audio       Disable sound and music
  --load N         Load save slot N (0-9)
  --data-dir PATH  Custom path to original game data files
  --debug          Enable global debug logging
  --debug-module M Enable debug logging for specific module(s) (e.g. train,backend)

PROJECT & SOURCE:
-----------------
https://github.com/bluddy/rails
