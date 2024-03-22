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

type balance_sheet = {
  operating_funds: int;
  treasury_stock: int;
  other_rr_stock: int;
  facilities: int;
  industries: int;
  real_estate: int;
  track: int;
  rolling_stock: int;
  outstanding_loans: int;
  stockholders_equity: int;
} [@@deriving yojson]

let default_balance_sheet = {
  operating_funds=1000;
  treasury_stock=0;
  other_rr_stock=0;
  facilities=0;
  industries=0;
  real_estate=0;
  track=0;
  rolling_stock=0;
  outstanding_loans = -500;
  stockholders_equity = -500;
}

type monetary = {
  cash: int; (* all x1000 *)
  bonds: int; (* money *)
  stock: Stocks.t;
  stockholders_equity : int;
  owned_industry: int;
  yearly_interest_payment: int;
  net_worth: int;
  freight_income: (Freight.t, int) Hashtbl.t;
  other_income: int;
  expenses: (expense, int) Hashtbl.t;
  last_balance_sheet: balance_sheet;
} [@@deriving yojson]

let default_monetary ~player difficulty =
  {
    cash = 1000;
    bonds = 500;
    stockholders_equity = -500;
    stock = Stocks.default_for_player ~player difficulty;
    owned_industry = 0;
    yearly_interest_payment=20;
    net_worth=50;
    freight_income=Hashtbl.create 10;
    other_income=0;
    expenses=Hashtbl.create 10;
    last_balance_sheet=default_balance_sheet;
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

