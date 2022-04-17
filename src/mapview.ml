open Containers

type zoominfo =
  {
    center_x: int;
    center_y: int;
  }
  
type zoom =
  | F1 of zoominfo
  | F2 of zoominfo
  | F3 of zoominfo
  | F4


type t =
  {
    zoom: zoom;
  }

let default = {zoom=F4}

let update = ()


