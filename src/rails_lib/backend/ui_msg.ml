open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
open! Utils

type train_arrival_msg = {
    player: Owner.t;
    time: int;
    train_name: string option;
    freight: Freight.complex;
    _type: Train.train_type;
    train_num: int;
    goods_amount: (Goods.t * int) list; (* goods delivered *)
    revenue: int; (* x 1000 *)
} [@@deriving yojson]

type stock_broker_ui_msg = 
  | BondSold of {player: Owner.t; interest_rate: int}
  | BondRepaid of {player: Owner.t}
  | StockBought of {player: Owner.t; stock: Owner.t; cost: int}
  | StockSold of {player: Owner.t; stock: Owner.t; cost: int}
  | BankruptcyDeclared of {player: Owner.t}
  | Takeover of {player:Owner.t; stock: Owner.t}
  | MoneyTransferredFrom of {player: Owner.t; company: Owner.t; amount: int}
  | MoneyTransferredTo of {player: Owner.t; company: Owner.t; amount: int}
  | AiBondRepaid of {player: Owner.t; company: Owner.t}
  [@@deriving yojson]

type t =
  | TrainBuilt of Trainmap.Id.t
  | DemandChanged of {player: Owner.t; x: int; y: int; good: Goods.t; add: bool}
  | TrainArrival of train_arrival_msg
  | StockBroker of stock_broker_ui_msg
  | OpenStockBroker of {player: Owner.t}
  | PriorityShipmentCreated of {player: Owner.t; shipment:Priority_shipment.t}
  | PriorityShipmentDelivered of {player: Owner.t; shipment:Priority_shipment.t; bonus:int}
  | PriorityShipmentCanceled of {player: Owner.t}
  | IndustryBuilt of {player: Owner.t; tile: Tile.t}
  | NewCompany of {opponent: Opponent.name; city: loc}
  | AiConnected of {opponent: Opponent.name; ai_name: string; src_name: string; tgt_name: string}
  | AiBuildOrderFailed of {player: Owner.t; ai_name: string; src_name: string; tgt_name: string}
  | AiBuySellOwnStock of {ai_idx: Owner.t; price: int; buy: bool; opponent: Opponent.name}
  | AiTakesOutBond of {player: Owner.t; ai_idx: Owner.t; opponent: Opponent.name}  (* msg to player only. payback is secret *)
  | AiSellsPlayerStock of {player: Owner.t; ai_idx: Owner.t; opponent: Opponent.name}
  | AiBuysPlayerStock of {player: Owner.t; ai_idx: Owner.t; takeover: bool; opponent: Opponent.name}
  [@@deriving yojson]

