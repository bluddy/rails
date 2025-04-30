open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils.Infix

module C = Constants
module Vector = Utils.Vector
module Hashtbl = Utils.Hashtbl
module IntMap = Utils.IntMap

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

type route = (Utils.loc * Utils.loc)
             [@@deriving yojson]

 (* An AI Player *)
type ai_player = {
  idx: int;
  cash: int; (* all x1000 *)
  bonds: int;
  opponent: Opponent.t;
  build_order: (Utils.loc * Utils.loc) option;  (* order given to subservient company *)
  yearly_income: int; (* rough estimation of 8 * yearly income *)
  net_worth: int;
  cities: Utils.loc * Utils.loc; (* first route and name *)
  revenue_ytd: int;
} [@@deriving yojson]

type city_info = {
  player: int;
  rate_war: bool;
} [@@deriving yojson]

  (* Global AI data *)
type t = {
  routes: route Vector.vector;  (* Routes for all AIs *)
  cities_to_ai: city_info Loc_map.t;
  players: ai_player IntMap.t;
} [@@deriving yojson]

let default () = {
  routes=Vector.create ();
  cities_to_ai=Loc_map.empty;
  players=IntMap.empty;
}

let num_routes v = Vector.length v.routes

let get_route i v = Vector.get v.routes i

let random_route_idx random v = 
  Random.int (num_routes v) random

let owned_by_player stocks company =
  Stock_market.controls_company C.player ~target:company stocks

let player_of_city city v = Loc_map.get city v.cities_to_ai |> Option.map (fun x -> x.player)

    (* Update valuation only for AI players *)
let update_valuation player stocks v =
  let loans = v.bonds / 10 in
  let cash = v.cash / 10 in
  let stock_value = Stock_market.total_owned_stock_value player stocks in
  let net_worth = cash - loans + v.yearly_income * 2 + stock_value in
  {v with net_worth}

let home_town v = fst v.cities

let has_rate_war city v = (Loc_map.get_exn city v.cities_to_ai).rate_war

let route_value city1 city2 ~tilemap ~(params:Params.t) =
  let get_demand_supply (x, y) =
    let demand, supply = Tilemap.collect_demand_supply tilemap ~x ~y ~range:2 in
    (Hashtbl.sum (fun _ i -> i) demand) + (Hashtbl.sum (fun _ i -> i) supply)
  in
  let total = get_demand_supply city1 + get_demand_supply city2 in
  let age = params.year - C.ref_year_ai_route_value in
  (* More demanding over time *)
  let value = total / age in
  if Region.is_west_us params.region then
    (* east-west routes have more value *)
    let dx = abs @@ fst city1 - fst city2 in
    let dy = abs @@ snd city1 - snd city2 in
    ((3 * dx + dy) * value) / ((dx + dy) * 2)
  else value

  (* Simulate earning money on a route *)
let route_earn_money route_idx stocks ~params main_player_net_worth ~tilemap v =
  let city1, city2 = Vector.get v.routes route_idx in
  let player_idx = Option.get_exn_or "AI player idx not found" @@ player_of_city city1 v in
  let ai_player = IntMap.find player_idx v.players in
  let value = route_value city1 city2 ~tilemap ~params in
  (* NOTE: I think there's a bug in the original code here. It didn't check both
     cities for a rate war but just one *)
  let div = if has_rate_war city1 v || has_rate_war city2 v then 6 else 3 in
  let value = value / div in
  let value =
    value * (ai_player.opponent.management + Climate.to_enum params.climate + 3)
  in
  (* Higher difficulty -> earns more *)
  let div = if owned_by_player stocks ai_player.idx then 10
            else 10 - B_options.difficulty_to_enum params.options.difficulty in
  let value = value / div in
  let value =
    (* NOTE: what about checking city1? *)
    if Utils.equal_loc (home_town ai_player) city2 &&
        main_player_net_worth >= ai_player.net_worth
    then value * 2 else value
  in
  let revenue_ytd = ai_player.revenue_ytd + value in
  let cash = if ai_player.cash < C.ai_max_cash then
    ai_player.cash + value else ai_player.cash
  in
  let ai_player = {ai_player with revenue_ytd; cash} in
  let players = IntMap.add player_idx ai_player v.players in
  {v with players}

let ai_routines stocks ~params ~main_player_net_worth ~tilemap random v =
  let earn_random_route v =
    if Random.int 100 random <= num_routes v then
      let route_idx = random_route_idx random v in
      route_earn_money route_idx stocks ~params main_player_net_worth ~tilemap v
    else v
  in
  ()




