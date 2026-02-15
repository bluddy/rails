# Rails - Technical Documentation

This documentation is produced by AI.

## Overview

This is an OCaml implementation of a Railroad Tycoon-like simulation game. The codebase is structured with clean separation between game logic (backend) and presentation (UI/rendering), following functional programming principles with mutable state management where performance is critical.

## Architecture

### Core Architecture Pattern

The system follows a **Backend-Frontend** architecture with **Message Passing**:

- **Backend**: Pure game state and logic, accessed only through defined actions/messages
- **Frontend**: UI, rendering, and user interaction handling
- **State Management**: Central state container with both mutable and immutable components
- **Resource Management**: Asset loading and texture management system

### Main Entry Points

**`src/main_run.ml`** - Application entry point with multiple modes:
- `--font`: Font processing utilities  
- `--pic`: Convert .PIC files to PNG
- `--pani`: Play animation files
- `--city`: Dump city information
- `--load`: Load save files
- Default: Run the game

**`src/rails_lib/main.ml`** - Core game initialization and mode dispatch

### Game Loop Architecture

**`mainloop.ml`** implements the core game loop with:

```ocaml
type 'a t = {
  handle_event: 'a -> Event.t -> 'a * bool;
  handle_tick: 'a -> int -> 'a;
  render: 'a -> unit;
}
```

- **Event-driven architecture**: SDL events converted to custom Event.t types
- **Fixed timestep**: Separate tick rate (15 FPS) and render rate (30 FPS)
- **State threading**: Functional state updates with performance optimizations

## Data Structures & Records

### Core Game State

**`state.ml`** - Central application state:

```ocaml
type t = {
  (* Saveable state *)
  mutable backend: Backend.t;
  mutable ui: t Main_ui_d.t;
  
  (* Runtime state *)
  mode: module_t;
  mutable map_tex: Renderer.Texture.t;
  map_silhouette_tex: Renderer.Texture.t;
  textures: Textures.t;
  resources: Resources.t;
  fonts: Fonts.t;
  win: Renderer.window;
  random: Utils.Random.State.t;
}
```

**`backend/backend_d.ml`** - Core game backend state:

```ocaml
type t = {
  params: Params.t;
  mutable last_tick: int;
  pause: bool;
  mutable players: Player.t Owner.Map.t;
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  cities: Cities.t;
  engines: Engine.t list;
  mutable stations: Station_map.t;
  mutable blocks: Block_map.t;
  mutable dev_state: Tile_develop.t;
  stocks: Stock_market.t;
  ai: Ai.t;
  mutable ui_msgs: Ui_msg.t list;
  random: Utils.Random.State.t;
  seed: int;
}
```

### Player & Financial System

**`backend/player.ml`** - Player financial state:

```ocaml
type monetary = {
  cash: Money.t;
  bonds: Money.t;
  stockholders_equity: Money.t;
  owned_industry: Money.t;
  yearly_interest_payment: Money.t;
  net_worth: Money.t;
  profit: Money.t;
  in_receivership: bool;
  income_statement: Income_statement_d.t;
  total_income_statement: Income_statement_d.t;
  last_balance_sheet: Balance_sheet_d.t;
  num_bankruptcies: int;
  investor_anger: int;
}
```

**Design Choice**: All monetary values use `Money.t` (scaled integers) for precise financial calculations avoiding floating-point errors.

### Map & Terrain System

**`backend/tilemap.ml`** - World map representation:

```ocaml
type t = {
  seed: int;
  map: Tile.t array;
  heightmap: int array;
  width: int;
  height: int;
  region: Region.t;
}
```

**Pixel Types**: 16 different terrain types (Ocean, Clear, Woods, Harbor, CoalMine, Desert, Foothills, etc.)

**Map Generation**: Procedural generation in `mapgen.ml` using:
- Region-specific parameters
- Mountain range generation algorithms  
- City placement logic
- Resource distribution

### Transportation System

**`backend/train.ml`** - Train representation:

```ocaml
type stop = {
  x: int;
  y: int;
  consist_change: (Goods.t list) option;
}

type train_type = Local | Through | Express | Limited

module History = struct
  type elem = {
    x: int; y: int; dir: Dir.t; speed_factor: int;
  }
  type t = { history: elem array; mutable idx: int; }
end
```

**Design Choice**: Train history stored as circular buffer for efficient rendering of train cars following the engine.

**`backend/station.ml`** - Station types and economics:

```ocaml
type kind = [ `SignalTower | `Depot | `Station | `Terminal ]
```

Stations have different:
- **Construction costs**: SignalTower (25) → Depot (50) → Station (100) → Terminal (200)
- **Maintenance costs**: 1-4 per year
- **Service ranges**: 0-3 tiles

### Track & Graph System

**`backend/track_graph.ml`** - Railway network representation:

```ocaml
module Edge = struct
  type t = {
    nodes: locdpair;
    dist: int;
    mutable block: bool;
  }
end
```

**Graph Structure**:
- **Nodes**: Intersections and stations
- **Edges**: Track segments with distance and blocking state
- **Pathfinding**: Used for train routing and block signaling

**`backend/trackmap.ml`** - Low-level track placement and management

### Animation System

**`anim/pani.ml`** - Custom PANI animation format:

- **PANI Format**: Custom binary format for animations
- **LZW Compression**: Compressed image data with RLE
- **Frame Management**: Sequence of 320x200 images

**Implementation**: Direct binary parsing with OCaml pattern matching for efficient animation playback.

## Module Organization

### Backend Modules (Game Logic)

**Core Systems**:
- `backend.ml` (1012 lines) - Main backend interface and game state management
- `player.ml` (762 lines) - Player data, finances, corporate management  
- `ai.ml` (1067 lines) - AI opponent logic and decision-making
- `train.ml` (673 lines) - Train simulation, routing, and cargo management

**Map & Infrastructure**:
- `tilemap.ml` (496 lines) - World map and terrain system
- `station.ml` (524 lines) - Station management and economics
- `track_graph.ml` (414 lines) - Railway network graph algorithms
- `trackmap.ml` (266 lines) - Track placement and connectivity

**Economic Systems**:
- `stock_market.ml` (412 lines) - Stock trading and market simulation
- `goods.ml` (170 lines) - Cargo types and economics
- `freight.ml` (150 lines) - Freight car management
- `jobs.ml` (155 lines) - Job/contract system

**Game Mechanics**:
- `scan.ml` (195 lines) - Pathfinding and route analysis
- `block_map.ml` (390 lines) - Block signaling system
- `tile_develop.ml` (233 lines) - Dynamic map development

### UI Modules (Frontend)

**Core UI**:
- `main_ui.ml` (1524 lines) - Main game interface and interaction handling
- `mapview.ml` (976 lines) - Map rendering and viewport management  
- `menu.ml` (689 lines) - Menu system and navigation

**Game Screens**:
- `train_report.ml` (457 lines) - Train management interface
- `stock_broker.ml` (362 lines) - Stock trading interface
- `fiscal_period_end.ml` (288 lines) - End-of-year financial reports
- `station_report.ml` (210 lines) - Station economics interface

**Resources & Rendering**:
- `textures.ml` (1090 lines) - Texture loading and management
- `fonts.ml` (335 lines) - Font rendering system
- `resources.ml` (93 lines) - Asset loading and management

### Utility Modules

**Data Structures**:
- `utils/bitset.ml` - Efficient bit set implementation for flags/enums
- `utils/loc_map.ml` - Location-based spatial data structures
- `utils/int_id.ml` - Type-safe integer ID generation

**Graphics & I/O**:
- `utils/renderer.ml` - SDL2 rendering abstraction
- `utils/pic.ml` - Legacy .PIC format image loading
- `utils/png.ml` - PNG image processing
- `utils/lzw.ml` - LZW compression/decompression

## Key Implementation Details

### Performance Optimizations

**Mutable State Strategy**:
- **Selective Mutability**: Critical performance paths use mutable fields
- **Functional Core**: Most game logic remains purely functional
- **State Threading**: Efficient state updates through the game loop

**Memory Management**:
- **Circular Buffers**: Train history tracking avoids allocations
- **Texture Caching**: Pre-loaded texture atlases for efficient rendering
- **Event Pooling**: SDL event reuse to reduce GC pressure

### Serialization & Save System

**PPX Deriving**: Extensive use of `[@@deriving yojson]` for:
- **Save/Load**: Complete game state serialization
- **Network Protocol**: Future multiplayer support
- **Debugging**: Runtime state inspection

### Financial Precision

**Money.t Implementation**:
- **Fixed-Point Arithmetic**: All financial calculations use scaled integers
- **Precision**: Avoids floating-point rounding errors in economic simulation
- **Performance**: Integer arithmetic faster than floating-point

### AI System Design

**AI Architecture** (`ai.ml`):
- **State Machine**: AI opponents have different behavioral states
- **Economic Analysis**: AI evaluates profitable routes and investments
- **Adaptive Difficulty**: AI behavior scales with player performance

### Map Generation Algorithm

**Procedural Generation** (`mapgen.ml`):
- **Region Templates**: Different geographic regions (WestUS, etc.)
- **Mountain Placement**: Randomized mountain range generation
- **City Distribution**: Economic and geographic constraints for city placement
- **Resource Allocation**: Strategic placement of coal, oil, and other resources

### Track Graph Implementation

**Graph Algorithms**:
- **Canonical Edges**: Consistent edge representation for efficient lookups
- **Block Signaling**: Temporary edge blocking for train collision avoidance
- **Pathfinding**: Dijkstra-based routing with economic cost factors

## Testing Strategy

**Test Structure** (`src/tests/`):
- `test_common.ml` - Shared test utilities and dummy data creation
- `track_graph_test.ml` - Graph algorithm validation
- `tile_map_test.ml` - Map generation testing
- `block_map_test.ml` - Block signaling system testing

**Testing Approach**:
- **Property-Based**: Using dummy data generators for edge case testing
- **Integration Tests**: Full subsystem testing with realistic scenarios
- **Performance Tests**: Critical path benchmarking

## Dependencies

**Core Libraries**:
- `containers` - Enhanced standard library
- `tsdl` - SDL2 bindings for graphics and input
- `owl-base` - Numerical array operations for map data
- `imagelib` - Image format support
- `ocamlgraph` - Graph algorithms
- `yojson` - JSON serialization

**Development Tools**:
- `ppx_deriving` - Code generation for common patterns
- `ppx_yojson_conv` - JSON serialization automation
- `logs` - Structured logging system

## Design Patterns

### Functional-Imperative Hybrid

The codebase successfully balances functional programming principles with performance requirements:

- **Immutable Core**: Game rules and logic remain purely functional
- **Mutable Edges**: Performance-critical systems use selective mutability  
- **Message Passing**: Clean separation between backend and UI through defined interfaces

### Type Safety

- **Phantom Types**: Owner.t, Money.t provide compile-time safety
- **Variant Types**: Extensive use of algebraic data types for game states
- **Module Signatures**: Clear interfaces between major subsystems

### Resource Management

- **Asset Pipeline**: Efficient loading and caching of game assets
- **Texture Atlasing**: Combined textures for reduced draw calls
- **Memory Pools**: Reused objects for performance-critical paths
