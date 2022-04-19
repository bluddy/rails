
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4

type t =
  {
    mutable center_x: int; (* in map coordinates *)
    mutable center_y: int; (* in map coordinates *)
    mutable zoom: zoom;
    width: int;
    height: int;
  }
