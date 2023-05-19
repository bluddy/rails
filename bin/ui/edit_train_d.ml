
type msg =
  [   `ShowMap
    | `Type of Train.train_type
    | `RetireTrain
    | `ReplaceEngine 
    | `EngineInfo of Engine.make
  ]

(* Place this here to avoid circular dependency or an extra file *)
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
  | EngineInfo of Engine_info.t

type 'state t = {
  train: int;
  menu: (msg, 'state) Menu.Global.t;
  car_menu: (([`AddCar of Goods.t | `Done | `Caboose], 'state) Menu.MsgBox.t * Backend.Action.stop) option; (* stop *)
  screen: screen;
}

