open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl

type expense =
  | LandExpense
  | TrackExpense
  | TunnelExpense
  | TrainExpense
  | InterestExpense
  | StationExpense
  [@@deriving yojson]


type monetary = {
  cash: int; (* x1000 *)
  bonds: int;
  yearly_interest_payment: int;
  net_worth: int;
  freight_income: (Freight.t, int) Hashtbl.t;
  other_income: int;
  expenses: (expense, int) Hashtbl.t;
  yearly_balance_sheet: Balance_sheet_d.t;
} [@@deriving yojson]

type t = {
  name: string option;
  trains: Trainmap.t;
  m: monetary;
  treasury_stock: int;
  other_rr_stock: int;
  shares: int;
  share_price: int;
  track_length: int;
  mutable dist_traveled: int;
  ton_miles: (int * int);
  freight_ton_miles: (Freight.t, int) Hashtbl.t;
  goods_delivered: Goods.Set.t;
} [@@deriving yojson]

let default difficulty =
  let trains = Trainmap.empty () in
  let m = {
    cash = 1000;
    bonds = 500;
    yearly_interest_payment=20;
    net_worth=50;
    freight_income=Hashtbl.create 10;
    other_income=0;
    expenses=Hashtbl.create 10;
    yearly_balance_sheet=Balance_sheet_d.default;
  } in
  {
    name=None;
    m;
    trains;
    treasury_stock=0;
    other_rr_stock = 0;
    shares=100;
    share_price=B_options.difficulty_to_enum difficulty + 7;
    track_length = 0;
    dist_traveled=0;
    ton_miles=(0, 0);
    freight_ton_miles=Hashtbl.create 10;
    goods_delivered=Goods.Set.empty;
  }

let get_cash v = v.m.cash

let decr_cash ~cash v =
  let m = {v.m with cash = v.m.cash - cash} in
  {v with m}

let incr_cash ~cash v =
  let m = {v.m with cash = v.m.cash + cash} in
  {v with m}

let pay expense cash v =
  Hashtbl.incr ~by:cash v.m.expenses expense;
  decr_cash ~cash v

let track_length v = v.track_length

let incr_dist_traveled ~dist v =
  v.dist_traveled <- v.dist_traveled + dist;
  v

let add_track ~length v =
  {v with track_length = v.track_length + length}

