
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4
  [@@deriving eq, show, yojson]

type options =
  [ `StationBoxes | `Resources]
  [@@ deriving enum, eq, show, yojson]

module Options = Bitset.Make(struct
    type t = options
    let to_enum = options_to_enum
    let of_enum = options_of_enum
    let last = `Resources
  end)

type t =
  {
    center_x: int; (* in map coordinates *)
    center_y: int;
    cursor_x: int;
    cursor_y: int;
    zoom: zoom;
    dims: Utils.rect; [@yojson.opaque]
    build_mode: bool;
    survey: bool;
    options: Options.t;
  }
  [@@deriving yojson]
