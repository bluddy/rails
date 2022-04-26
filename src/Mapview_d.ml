
type zoom =
  | Zoom1
  | Zoom2
  | Zoom3
  | Zoom4
  [@@deriving eq]

type t =
  {
    mutable center_x: int; (* in map coordinates *)
    mutable center_y: int;
    mutable cursor_x: int;
    mutable cursor_y: int;
    mutable zoom: zoom;
    width: int;
    height: int;
  }
  [@@deriving lens]
