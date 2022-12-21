
type msg =
  [ `ShowMap
  ]

type station_map = {
  train: int;
  stop_to_update: [`ShowRoute | `EditPriority | `EditStop of int];
}

type screen =
  | Normal
  | StationMap of station_map

type 'state t = {
  train: int;
  menu: (msg, 'state) Menu.Global.t;
  screen: screen;
}

