open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils.Infix

module C = Constants
module Vector = Utils.Vector
module Hashtbl = Utils.Hashtbl

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

type route = (Utils.loc * Utils.loc)
             [@@deriving yojson]

type player = {
  idx: int;
  cash: int; (* all x1000 *)
  bonds: int;
  opponent: Opponent.t;
  build_order: (Utils.loc * Utils.loc) option;  (* order given to subservient company *)
  yearly_income: int; (* rough estimation of 8 * yearly income *)
  net_worth: int;
} [@@deriving yojson]

  (* Global AI data *)
type t = {
  routes: route Vector.vector;
  cities_to_ai: int Loc_map.t;
} [@@deriving yojson]

let default_t = {
  routes = Vector.create ();
  cities_to_ai = Loc_map.empty;
}

let owned_by_player stocks company =
  Stock_market.controls_company C.player ~target:company stocks

    (* Update valuation only for AI players *)
let update_valuation player stocks v =
  let loans = v.bonds / 10 in
  let cash = v.cash / 10 in
  let stock_value = Stock_market.total_owned_stock_value player stocks in
  let net_worth = cash - loans + v.yearly_income * 2 + stock_value in
  {v with net_worth}

  (* Simulate earning money *)
let route_earn_money route_num stocks ai_player v =
  let city1, city2 = Vector.get v.routes route_num in
  let value = route_value city1 city2 in
  let total_shares = Stock_market.total_shares v.idx stocks in
  if owned_by_player stocks v.idx then



  else
  ()



let route_value city1 city2 ~tilemap ~year ~region =
  let get_demand_supply (x, y) =
    let demand, supply = Tilemap.collect_demand_supply tilemap ~x ~y ~range:2 in
    (Hashtbl.sum (fun _ i -> i) demand) + (Hashtbl.sum (fun _ i -> i) supply)
  in
  let total = get_demand_supply city1 + get_demand_supply city2 in
  let age = year - C.ref_year_ai_route_value in
  (* More demanding over time *)
  let value = total / age in
  if Region.is_west_us region then
    (* east-west routes have more value *)
    let dx = abs @@ fst city1 - fst city2 in
    let dy = abs @@ snd city1 - snd city2 in
    ((3 * dx + dy) * value) / ((dx + dy) * 2)
  else value







