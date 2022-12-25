
type msg =
  [ `ShowMap
  ]

type station_map = {
  train: int;
  purpose: [`ShowRoute | `EditPriority | `EditStop of int];
  selected_station: (int * int) option;
  map_x: int; (* for scaling map *)
  map_y: int; (* for scaling map *)
  map_dim: int; (* for scaling map *)
}

type screen =
  | Normal
  | StationMap of station_map

type 'state t = {
  train: int;
  menu: (msg, 'state) Menu.Global.t;
  screen: screen;
}

