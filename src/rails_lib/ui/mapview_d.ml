open! Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4
  [@@deriving show, yojson]

type options =
  [ `StationBoxes | `Resources]
  [@@ deriving enum, eq, show, yojson]

module Options = Bitset.Make(struct
    type t = options [@@deriving yojson]
    let to_enum = options_to_enum
    let of_enum = options_of_enum
    let last = `Resources
  end)

(* A smoke plume animation eminating from a smokestack *)
type smoke_plume = {
  mutable frame: int;
  mutable x: int;
  mutable y: int;
  dir: Dir.t;
} [@@deriving yojson]

let max_smoke_frame = 16

type train_history = (int * int * Ega.color) list
  [@@deriving eq, yojson]

type t =
  {
    center_x: int; (* in map coordinates *)
    center_y: int;
    cursor_x: int;
    cursor_y: int;
    zoom: zoom;
    dims: Utils.rect;
    build_mode: bool;
    survey: bool;
    mutable smoke_plumes: smoke_plume list;
   (* Used to provide the train effect for zoom2/3 *)
    draw_buffer: (int, train_history array) Utils.Hashtbl.t;
    (* Used for zoom2/3 for stationboxes *)
    tile_buffer: Tilebuffer.t;
    options: Options.t;
  }
  [@@deriving yojson]

