open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers
open Utils
open Utils.Infix
(* Backend data *)

let num_players = 4

(* Cycle counts to perform some tasks *)
let cycles_periodic_maintenance = 1024
let cycles_priority_delivery = 8
let cycles_background_update = 16

let cycles_ai_update = cycles_background_update
(* In the original game, we do slices of 1/32 stations. No need *)
let cycles_station_supply_demand = cycles_background_update * 32 (* 512 *)
(* Since we don't spread out the supply addition, decay happens in the same cycle *)
let cycles_supply_decay = 512

type train_arrival_msg = {
    player: int;
    time: int;
    train_name: string option;
    freight: Freight.complex;
    _type: Train.train_type;
    train_num: int;
    goods_amount: (Goods.t * int) list; (* goods delivered *)
    revenue: int; (* x 1000 *)
} [@@deriving yojson]

type stock_broker_msg = 
  | BondSold of {player: int; interest_rate: int}
  | BondRepaid of {player: int}
  | StockBought of {player:int; stock: int; cost: int}
  | StockSold of {player:int; stock: int; cost: int}
  | BankruptcyDeclared of {player: int}
  [@@deriving yojson]

type ui_msg =
  | TrainBuilt of Trainmap.Id.t
  | DemandChanged of {x: int; y: int; good: Goods.t; add: bool}
  | TrainArrival of train_arrival_msg
  | StockBroker of stock_broker_msg
  [@@deriving yojson]

type t = {
  mutable last_tick: int; (* last time we updated a cycle *)
  mutable cycle: int; (* counter used for all sorts of per-tick updates *)
  mutable time: int;  (* In-game time, resets at end of year *)
  mutable year: int;
  pause: bool;  (* pause the backend. Do not advance time *)
  year_start: int;
  fiscal_period: [`First | `Second];
  climate: Climate.t;
  west_us_route_done: bool;
  players: Player.t array; (* stats, money *)
  region: Region.t;
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  cities: Cities.t;
  engines: Engine.t list;
  mutable stations: Station_map.t;
  mutable blocks: Block_map.t; (* map blocks btw stations *)
  priority: (loc * loc * Goods.t) option;  (* priority shipment *)
  options: B_options.t;
  mutable ui_msgs: ui_msg list;
  random: Utils.Random.State.t;
  seed: int;
} [@@deriving yojson]

let update_player v player f =
  let p = v.players.(player) in
  let p' = f p in
  if p =!= p' then
    v.players.(player) <- p';
  ()


