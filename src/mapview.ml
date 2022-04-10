open Containers

type zoom = F1 | F2 | F3 | F4

type t =
  {
    zoom: zoom;
    center_x: int;
    center_y: int;
  }


