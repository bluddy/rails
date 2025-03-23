open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers
open! Utils
open Utils.Infix
(* Backend data *)


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

type stock_broker_ui_msg = 
  | BondSold of {player: int; interest_rate: int}
  | BondRepaid of {player: int}
  | StockBought of {player:int; stock: int; cost: int}
  | StockSold of {player:int; stock: int; cost: int}
  | BankruptcyDeclared of {player: int}
  | Takeover of {player:int; stock: int}
  | MoneyTransferredFrom of {player:int; company:int; amount:int}
  | MoneyTransferredTo of {player:int; company:int; amount:int}
  | AiBondRepaid of {player:int; company: int}
  [@@deriving yojson]

type ui_msg =
  | TrainBuilt of Trainmap.Id.t
  | DemandChanged of {x: int; y: int; good: Goods.t; add: bool}
  | TrainArrival of train_arrival_msg
  | StockBroker of stock_broker_ui_msg
  | OpenStockBroker of {player: int}
  | PriorityShipmentCreated of {player: int; shipment:Priority_shipment.t}
  | PriorityShipmentDelivered of {player: int; shipment:Priority_shipment.t; bonus:int}
  | PriorityShipmentCanceled of {player: int}
  [@@deriving yojson]

type t = {
  mutable last_tick: int; (* last time we updated a cycle *)
  mutable cycle: int; (* ongoing counter used for all sorts of stuff *)
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


