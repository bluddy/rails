
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4
  [@@deriving eq]

type t =
  {
    center_x: int; (* in map coordinates *)
    center_y: int;
    cursor_x: int;
    cursor_y: int;
    cursor_track: [`NoTrack | `Track | `Station | `WoodBridge];
    zoom: zoom;
    width: int;
    height: int;
  }
