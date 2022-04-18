
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4

type t =
  {
    center_x: int;
    center_y: int;
    mutable zoom: zoom;
  }
