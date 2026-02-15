# Rails

An OCaml remake of [Railroad Tycoon](https://en.wikipedia.org/wiki/Railroad_Tycoon_(video_game)) (1990). Build railroads, manage trains, compete with AI rivals, and grow your empire across historical regions.

[![OCaml](https://img.shields.io/badge/OCaml-5.x-blue.svg)](https://ocaml.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

See [FEATURES.md](docs/FEATURES.md) for a full list of implemented features.

## Prerequisites

- OCaml 5.x with opam
- Original Railroad Tycoon files.
  (Unfortunately Railroad Tycoon is not available for sale digitally, so you'll need to buy it off of ebay and such.)

## Data Files

The game expects original game assets in `./data/`.

## Installation

```bash
opam install . --deps-only
dune build
```

## Running

```bash
# New game
dune exec rails

# Load saved game (can also be done via the menu system)
dune exec rails -- --load 0

# With options
dune exec rails -- --zoom 3 --shader crt-hyllian2 --adjust-ar
```

### Command-line options

| Option | Description |
|--------|-------------|
| `--load N` | Load save slot 0–9 |
| `--zoom N` | Display zoom, multiply the original 320x200 resolution (default: 3) |
| `--shader NAME` | Shader from `shaders/*.glsl` (test, crt-hyllian, vga-1080p, etc.) |
| `--adjust-ar` | Adjust aspect ratio (rectangular pixels like DOS)|

## Project structure

- `src/rails_lib/` — Main library
  - `backend/` — Game logic (trains, stations, AI, economy)
  - `ui/` — Menus, rendering, fonts, textures
  - `anim/` — PANI animation format
  - `utils/` — Rendering, image loading, sound
- `shaders/` — GLSL shaders for display
- [technical_documentation.md](docs/technical_documentation.md) — Architecture and design details

## Development

```bash
dune runtest
dune build @doc
```

## License

MIT — see [LICENSE](LICENSE).

## Future
Unlike the original, the game is built with fronted-backend separation, so in theory it could be made to support >1 player.

