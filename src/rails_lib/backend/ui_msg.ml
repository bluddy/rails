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
    revenue: Money.t; (* x 1000 *)
} [@@deriving yojson]

type stock_broker_ui_msg = 
  | BondSold of {player_idx: Owner.t; interest_rate: int}
  | BondRepaid of {player_idx: Owner.t}
  | StockBought of {player_idx: Owner.t; stock: Owner.t; cost: Money.t}
  | StockSold of {player_idx: Owner.t; stock: Owner.t; cost: Money.t}
  | BankruptcyDeclared of {player_idx: Owner.t}
  | Takeover of {player_idx:Owner.t; stock: Owner.t}
  | MoneyTransferredFrom of {player_idx: Owner.t; company: Owner.t; amount: Money.t}
  | MoneyTransferredTo of {player_idx: Owner.t; company: Owner.t; amount: Money.t}
  | AiBondRepaid of {player_idx: Owner.t; company: Owner.t}
  [@@deriving yojson]

type t =
  | TrainBuilt of Trainmap.Id.t
  | DemandChanged of {player_idx: Owner.t; x: int; y: int; good: Goods.t; add: bool}
  | TrainArrival of train_arrival_msg
  | StockBroker of stock_broker_ui_msg
  | OpenStockBroker of {player_idx: Owner.t}
  | PriorityShipmentCreated of {player_idx: Owner.t; shipment: Priority_shipment.t}
  | PriorityShipmentDelivered of {player_idx: Owner.t; shipment: Priority_shipment.t; bonus: Money.t}
  | PriorityShipmentCanceled of {player_idx: Owner.t}
  | IndustryBuilt of {player_idx: Owner.t; tile: Tile.t}
  | NewCompany of {opponent: Opponent.name; city: loc}
  | AiConnected of {opponent: Opponent.name; ai_name: string; src_name: string; tgt_name: string}
  | AiBuildOrderFailed of {player_idx: Owner.t; ai_name: string; src_name: string; tgt_name: string}
  | AiBuySellOwnStock of {ai_idx: Owner.t; price: Money.t; buy: bool; opponent: Opponent.name}
  | AiTakesOutBond of {player_idx: Owner.t; ai_idx: Owner.t; opponent: Opponent.name}  (* msg to player only. payback is secret *)
  | AiSellsPlayerStock of {player_idx: Owner.t; ai_idx: Owner.t; opponent: Opponent.name}
  | AiBuysPlayerStock of {player_idx: Owner.t; ai_idx: Owner.t; takeover: bool; opponent: Opponent.name}
  [@@deriving yojson]

