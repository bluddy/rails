
type msg =
  [ `ShowMap
  ]

type station_map = {
  highlighted: int;
}

type screen =
  | Normal
  | StationMap of station_map

type 'state t = {
  train: int;
  menu: (msg, 'state) Menu.Global.t;
  screen: screen;
}

