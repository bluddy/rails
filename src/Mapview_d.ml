
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4

type t =
  {
    mutable center_x: int;
    mutable center_y: int;
    mutable zoom: zoom;
  }
