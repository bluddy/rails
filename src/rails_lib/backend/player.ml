open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl

type monetary = {
  cash: int; (* all x1000 *)
  bonds: int; (* money *)
  stock: Stocks.t;
  stockholders_equity : int; (* not sure how this changes *)
  owned_industry: int;
  yearly_interest_payment: int;
  net_worth: int;
  in_receivership: bool; (* bankruptcy *)
  income_statement: Income_statement_d.t;
  total_income_statement: Income_statement_d.t;
  last_balance_sheet: Balance_sheet_d.t;
} [@@deriving yojson]

let default_monetary ~player difficulty =
  {
    cash = 1000;
    bonds = 500;
    stockholders_equity = -500;
    stock = Stocks.default_for_player ~player difficulty;
    owned_industry = 0;
    yearly_interest_payment=20;
    net_worth=500;
    in_receivership=false;
    income_statement=Income_statement_d.default;
    total_income_statement=Income_statement_d.default;
    last_balance_sheet=Balance_sheet_d.default;
}

type t = {
  name: string option; (* custom name *)
  trains: Trainmap.t;
  stations: Utils.loc list; (* Stations ordered by creation order *)
  m: monetary;
  track_length: int;
  mutable dist_traveled: int;
  ton_miles: (int * int);
  freight_ton_miles: (Freight.t, int) Hashtbl.t;
  goods_delivered: Goods.Set.t;
  ai: Opponent.t option;
} [@@deriving yojson]

let default ~player difficulty =
  let trains = Trainmap.empty () in
  {
    name=None;
    stations = [];
    m = default_monetary ~player difficulty;
    trains;
    track_length = 0;
    dist_traveled=0;
    ton_miles=(0, 0);
    freight_ton_miles=Hashtbl.create 10;
    goods_delivered=Goods.Set.empty;
    ai=None;
  }

let get_cash v = v.m.cash

let pay expense money (v:t) =
  let income_statement = Income_statement.deduct expense money v.m.income_statement in
  let cash = v.m.cash - money in
  {v with m = {v.m with cash; income_statement}}

let earn revenue money (v:t) =
  let income_statement = Income_statement.add_revenue revenue money v.m.income_statement in
  let cash = v.m.cash + money in
  {v with m = {v.m with cash; income_statement}}

let get_name v station_map cities = match v.name with
  | Some name -> name
  | _ -> 
    (* Getting the name if it's not custom is... complicated *)
    let first_two_proper_station_cities =
      List.fold_right (fun loc acc ->
        if List.length acc >= 2 then acc
        else
          let station = Station_map.get_exn loc station_map in
          match station.Station.info with 
          | Some info ->
            let x, y = info.city in
            let city, _ = Cities.find_exn cities x y in
            city::acc
          | _ -> acc)
      v.stations
      []
    in
    match first_two_proper_station_cities with
    | x::y::_ -> Printf.sprintf "%s & %s RR" y x
    | _ -> "RR"

let track_length v = v.track_length

let incr_dist_traveled ~dist v =
  v.dist_traveled <- v.dist_traveled + dist;
  v

let add_track ~length v =
  {v with track_length = v.track_length + length}

let remove_track ~length v =
  {v with track_length = v.track_length - length}

let add_station loc v =
  {v with stations=loc::v.stations}

let remove_station loc v =
  let stations = List.filter (fun loc2 -> not @@ Utils.equal_loc loc loc2) v.stations in
  [%up {v with stations}]

let sell_bond (v:t) region =
  let bond = match region with Region.WestUS -> 1000 | _ -> 500 in
  let bonds = v.m.bonds + bond in
  {v with m={v.m with bonds}}

let repay_bond (v:t) region =
  let bond = match region with Region.WestUS -> 1000 | _ -> 500 in
  let bonds = v.m.bonds - bond in
  {v with m={v.m with bonds}}

