
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4
  [@@deriving eq, show]

type build_mode =
  [ `Build | `Remove ]
  [@@deriving eq, show]

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
  }
