
type msg =
  [ `ShowMap
  ]

type station_map = {
  train: int;
  state: [`ShowRoute | `EditPriority | `EditStop of int];
  selected_station: (int * int) option;
  map_x: int; (* for scaling map *)
  map_y: int; (* for scaling map *)
  map_dim: int; (* for scaling map *)
  mutable flash_time: int; (* flashing cursor *)
  mutable flash_on: bool;  (* flashing state *)
}

type screen =
  | Normal
  | StationMap of station_map

type 'state t = {
  train: int;
  menu: (msg, 'state) Menu.Global.t;
  car_menu: ([`AddCar of Goods.t | `Done | `Caboose], 'state) Menu.MsgBox.t;
  show_car_menu: int option;  (* stop number *)
  screen: screen;
}

