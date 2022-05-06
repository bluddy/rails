
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4
  [@@deriving eq, show]

type build_mode =
  [ `Build | `Remove ]
  [@@deriving eq, show]

type options =
  [ `StationBoxes | `Resources]
  [@@ deriving enum, eq, show]

module Options = Bitset.Make(struct type t = options let to_enum = options_to_enum let of_enum = options_of_enum let last = `Resources end)

type t =
  {
    center_x: int; (* in map coordinates *)
    center_y: int;
    cursor_x: int;
    cursor_y: int;
    zoom: zoom;
    width: int;
    height: int;
    build_mode: build_mode;
    options: Options.t;
  }
