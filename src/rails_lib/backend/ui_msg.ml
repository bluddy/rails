open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
open! Utils

type train_arrival_msg = {
    player: Owner.t;
    time: int;
    train_name: string option;
    freight: Freight.complex;
    _type: Train.train_type;
    train_num: Train.Id.t;
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

module TID = Train.Id

type fiscal_period_end_msg =
  | TrainNoRevenue of TID.t
  | TrainNoMaintenance of TID.t
  | TrainNoSchedule of TID.t
  | TrainOldEngine of TID.t
  | StationHasHold of loc
  | ConsiderBankruptcy
  | RecordEarnings of Money.t
  | AvgSpeedRecord of int
  | TonMileRecord of int
  | RevenueRecord of Money.t
  [@@deriving yojson]

type t =
  | NewPlayerCompany of {num_shares: int}
  | TrainBuilt of TID.t
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
  | BridgeWashout of {player_idx: Owner.t; loc: loc; fixed: bool}
  | ClimateChange of {climate: Climate.t; reason: Climate.reason}
  | EngineDiscovered of Engine.t
  | ImpossibleRoute of {player_idx: Owner.t; train_idx: Train.Id.t; src: loc; dst: loc}
  | TrainAccident of {player_idx: Owner.t}
  | TrainBridgeAccident of {player_idx: Owner.t; engine: Engine.t}
  | FirstTrainArrives of {player_idx: Owner.t; station: loc}
  | RateWarDeclared of {player_idx: Owner.t; other_player_idx: Owner.t; station: loc}
  | PlayerTakesControlOfOther of {player_idx: Owner.t; other: Owner.t}
  | OwnerFired of {player_idx: Owner.t; by:[`Stockholders | `Management]}
  | BridgeCreated of {player_idx: Owner.t; kind: Bridge.t}
  | NewGoodPickedUp of {player_idx: Owner.t; good: Goods.t; station: loc; engine: Engine.make; cars: Goods.t list; buying: loc list}
  | NewGoodDelivery of {player_idx: Owner.t; good: Goods.t; src: loc; dst: loc; amount: int; revenue: Money.t; engine: Engine.make; cars: Goods.t list; speed: int}
  | SpeedRecord of {player_idx: Owner.t; speed: int; src: loc; dst: loc; train_idx: Train.Id.t }
  | FiscalPeriodEnd of Owner.t
  | FiscalPeriodEndMsgs of Owner.t * fiscal_period_end_msg list
  [@@deriving yojson]

