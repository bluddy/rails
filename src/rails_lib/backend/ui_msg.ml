open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
open! Utils

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

type t =
  | TrainBuilt of Trainmap.Id.t
  | DemandChanged of {x: int; y: int; good: Goods.t; add: bool}
  | TrainArrival of train_arrival_msg
  | StockBroker of stock_broker_ui_msg
  | OpenStockBroker of {player: int}
  | PriorityShipmentCreated of {player: int; shipment:Priority_shipment.t}
  | PriorityShipmentDelivered of {player: int; shipment:Priority_shipment.t; bonus:int}
  | PriorityShipmentCanceled of {player: int}
  | IndustryBuilt of {player: int; tile: Tile.t}
  | NewCompany of {opponent: Opponent.name; city: loc}
  | AiConnected of {opponent: Opponent.name; ai_name: string; city1: loc; city2: loc}
  [@@deriving yojson]
