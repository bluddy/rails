open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module C = Constants

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

let get_total_shares v = Stocks.total_shares v.m.stock

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

let has_bond (v:t) = v.m.bonds > 0

let get_interest_rate v climate region =
  Climate.interest_rate climate region v.m.bonds

let check_sell_bond v climate region =
  let interest_rate = get_interest_rate v climate region in
  interest_rate < C.max_interest_rate && not v.m.in_receivership
    

let sell_bond (v:t) climate region =
  if check_sell_bond v climate region then (
    let bonds = v.m.bonds + C.bond_value in
    let cash = v.m.cash + C.bond_value in
    let interest_rate = Climate.interest_rate climate region v.m.bonds in
    let base_payment = C.bond_value / 100 in
    let interest_increase = base_payment * interest_rate in
    let yearly_interest_payment = v.m.yearly_interest_payment + interest_increase in
    let v = {v with m = {v.m with bonds; cash; yearly_interest_payment}} in
    pay InterestFees base_payment v
  ) else v

let check_repay_bond (v:t) =
  let has_bond = v.m.bonds > 0 in
  let has_cash = v.m.cash > C.bond_value in
  has_bond && (has_cash || not v.m.in_receivership)

let repay_bond (v:t) =
  if check_repay_bond v then
    let num_bonds = v.m.bonds / C.bond_value in
    let interest_saving = v.m.yearly_interest_payment / num_bonds in
    let yearly_interest_payment = v.m.yearly_interest_payment - interest_saving in
    let bonds = v.m.bonds - C.bond_value in
    let v = pay InterestFees C.bond_value v in
    (* Get rid of bankruptcy if needed *)
    let in_receivership = if bonds = 0 then false else v.m.in_receivership in
    {v with m = {v.m with bonds; yearly_interest_payment; in_receivership}}
  else v

let check_bankruptcy (v:t) =
  not v.m.in_receivership &&
  v.m.bonds > C.min_bonds_for_bankruptcy &&
  v.m.cash < C.max_cash_for_bankruptcy 

let declare_bankruptcy (v:t) =
  {v with m = {v.m with in_receivership = true}}

let can_buy_stock (v:t) target_idx ~target_player ~difficulty =
  let enough_money = v.m.cash >= v.m.stock.share_price * C.num_buy_shares in
  (* TODO: In original code, it's < total_shares - 10. Not sure why *)
  let can_buy = Stocks.owned_shares v.m.stock target_idx < get_total_shares target_player in
  (* Test if we have an 'anti-trust' problem *)
  let max_owned_companies = Utils.clip (B_options.difficulty_to_enum difficulty) ~min:1 ~max:3 in
  let stock = Stocks.add_shares v.m.stock ~target_idx ~num_shares:C.num_buy_shares in
  if Stocks.num_owned_companies stock > max_owned_companies then
    `Anti_trust_violation max_owned_companies
  else
    if enough_money && can_buy then `Ok else `Error

let _sell_buy_calculations v target_player ~target_idx ~add_stock ~buy =
  let share_price = target_player.m.stock.share_price in
  let non_treasury_shares = Stocks.non_treasury_shares target_player.m.stock + add_stock in
  let price_change =
    let delta = if buy then 1 else 0 in
    (share_price / non_treasury_shares) + delta
  in
  let cost = share_price * C.num_buy_shares in
  let price_change = if buy then price_change else -price_change in
  let share_price2 = share_price + price_change in
  let num_shares = if buy then C.num_buy_shares else -C.num_buy_shares in
  let stock = Stocks.add_shares v.m.stock ~target_idx ~num_shares in
  let cost = if buy then -cost else cost in
  let cash = v.m.cash + cost in
  cash, stock, cost, share_price2

let buy_stock (v:t) target_player player_idx ~target_idx ~difficulty =
  match can_buy_stock v target_idx ~target_player ~difficulty with
  | `Ok when player_idx = target_idx ->
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:0 ~buy:true in
    Some({v with m={v.m with cash; stock={stock with share_price}}}, cost, None)

  | `Ok -> (* different companies *)
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:C.num_buy_shares ~buy:true in
    Some({v with m={v.m with cash; stock}}, cost, Some share_price)

  | _ -> None

let can_sell_stock (v:t) target_idx =   
  Stocks.owned_shares v.m.stock target_idx > 0

let set_share_price (v:t) share_price =
  {v with m={v.m with stock={v.m.stock with share_price}}}

let sell_stock (v:t) target_player player_idx ~target_idx =
  match can_sell_stock v target_idx with
  | true when player_idx = target_idx ->
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:0 ~buy:false in
    Some({v with m={v.m with cash; stock={stock with share_price}}}, cost, None)

  | true ->
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:C.num_buy_shares ~buy:false in
    Some({v with m={v.m with cash; stock}}, cost, Some share_price)

  | false -> None



