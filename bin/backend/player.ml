open Containers
module Hashtbl = Utils.Hashtbl

type expense = [
  | `LandExpense
  | `TrackExpense
  | `TrainExpense
  | `InterestExpense
  | `StationExpense
  ] [@@deriving yojson]


type monetary = {
  money: int; (* x1000 *)
  bonds: int;
  yearly_interest_payment: int;
  net_worth: int;
  freight_income: (Goods.freight, int) Hashtbl.t;
  other_income: int;
  expenses: (expense, int) Hashtbl.t;
} [@@deriving yojson]

type t = {
  m: monetary;
  owned_shares: int;
  shares: int;
  share_price: int;
  track_length: int;
  mutable dist_traveled: int;
  ton_miles: (int * int);
  freight_ton_miles: (Goods.freight, int) Hashtbl.t;
  goods_delivered: Goods.Set.t;
} [@@deriving yojson]

let default difficulty = {
  m = {
    money = 1000;
    bonds = 500;
    yearly_interest_payment=20;
    net_worth=50;
    freight_income=Hashtbl.create 10;
    other_income=0;
    expenses=Hashtbl.create 10;
  };
  owned_shares=0;
  shares=100;
  share_price=B_options.difficulty_to_enum difficulty + 7;
  track_length = 0;
  dist_traveled=0;
  ton_miles=(0, 0);
  freight_ton_miles=Hashtbl.create 10;
  goods_delivered=Goods.Set.empty;
}

let get_money v = v.m.money

let decr_money ~money v =
  let m = {v.m with money = v.m.money + money} in
  {v with m}

let incr_money ~money v =
  let m = {v.m with money = v.m.money - money} in
  {v with m}

let spend_money expense money v =
  Hashtbl.incr ~by:money v.m.expenses expense;
  decr_money ~money v


let track_length v = v.track_length

let incr_dist_traveled ~dist v =
  v.dist_traveled <- v.dist_traveled + dist;
  v

let add_track ~length v =
  {v with track_length = v.track_length + length}

