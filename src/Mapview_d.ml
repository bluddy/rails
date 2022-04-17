
type zoom =
  | F1
  | F2
  | F3
  | F4

type t =
  {
    center_x: int;
    center_y: int;
    mutable zoom: zoom;
  }
