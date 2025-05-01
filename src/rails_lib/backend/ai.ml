open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils.Infix

module C = Constants
module Vector = Utils.Vector
module Hashtbl = Utils.Hashtbl
module IntMap = Utils.IntMap
module IntSet = Utils.IntSet

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

type route = (int * int)
             [@@deriving yojson]

 (* An AI Player *)
type ai_player = {
  idx: int;
  opponent: Opponent.t;
  city1: int; (* home city *)
  city2: int option; (* first route. Determines name *)
  cash: int; (* all x1000 *)
  bonds: int;
  build_order: (Utils.loc * Utils.loc) option;  (* order given to subservient company *)
  yearly_income: int; (* rough estimation of 8 * yearly income *)
  net_worth: int;
  revenue_ytd: int;
  expand_counter: int;  (* When the AI player wants to expand *)
} [@@deriving yojson]

  (* Global AI data *)
type t = {
  routes: route Vector.vector;  (* Routes for all AIs *)
  ai_of_city: int IntMap.t; (* Each city can only have one ai *)
  rate_war_at_city: IntSet.t;
  ais: ai_player IntMap.t; (* AI player info *)
} [@@deriving yojson]

let default () = {
  routes=Vector.create ();
  ai_of_city=IntMap.empty;
  rate_war_at_city=IntSet.empty;
  ais=IntMap.empty;
}

let num_routes v = Vector.length v.routes

let get_route i v = Vector.get v.routes i

let random_route_idx random v = 
  Random.int (num_routes v) random

let owned_by_player stocks company =
  Stock_market.controls_company C.player ~target:company stocks

    (* Update valuation only for AI players *)
let update_valuation player stocks v =
  let loans = v.bonds / 10 in
  let cash = v.cash / 10 in
  let stock_value = Stock_market.total_owned_stock_value player stocks in
  let net_worth = cash - loans + v.yearly_income * 2 + stock_value in
  {v with net_worth}

let ai_of_city city v = IntMap.get city v.ai_of_city

let city_rate_war city v = IntSet.mem city v.rate_war_at_city

let get_ai idx v = IntMap.get idx v.ais

let ai_exists idx v = IntMap.mem idx v.ais

let route_value city1 city2 ~tilemap ~(params:Params.t) =
  let get_demand_supply (x, y) =
    Tilemap.demand_supply_sum tilemap ~x ~y ~range:2
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
let route_earn_money route_idx stocks ~params main_player_net_worth ~tilemap ~cities v =
  let city1, city2 = Vector.get v.routes route_idx in
  let player_idx = Option.get_exn_or "AI player idx not found" @@ ai_of_city city1 v in
  let ai_player = IntMap.find player_idx v.ais in
  let city1_loc, city2_loc = Cities.get_idx city1 cities, Cities.get_idx city2 cities in
  let value = route_value city1_loc city2_loc ~tilemap ~params in
  let div = if city_rate_war city1 v || city_rate_war city2 v then 6 else 3 in
  let value = value / div in
  let value =
    value * (ai_player.opponent.management + Climate.to_enum params.climate + 3)
  in
  (* Higher difficulty -> earns more *)
  let div = if owned_by_player stocks ai_player.idx then 10
            else 10 - B_options.difficulty_to_enum params.options.difficulty in
  let value = value / div in
  let value = if ai_player.city1 = city2 && main_player_net_worth >= ai_player.net_worth
              then value * 2 else value
  in
  let revenue_ytd = ai_player.revenue_ytd + value in
  let cash = if ai_player.cash < C.ai_max_cash then
    ai_player.cash + value else ai_player.cash
  in
  let ai_player = {ai_player with revenue_ytd; cash} in
  let ais = IntMap.add player_idx ai_player v.ais in
  {v with ais}

let ai_routines ~stocks ~params ~main_player_net_worth ~tilemap ~trackmap ~cities random ~cycle ~station_map v =
  let earn_random_route v =
    if Random.int 100 random <= num_routes v then
      let route_idx = random_route_idx random v in
      route_earn_money route_idx stocks ~params main_player_net_worth ~tilemap ~cities v
    else v
  in
  let random_city () =
      let rec empty_city_loop () =
        let city_idx = Cities.random_idx random cities in
        if IntMap.mem city_idx v.ai_of_city then
          empty_city_loop ()
        else
          city_idx
      in
      empty_city_loop ()
  in
  let random_ai () = Random.int C.max_ai_players random in

  (* Earn 2x in random routes *)
  let v = earn_random_route v in
  let v = earn_random_route v in
  let city_idx = random_city () in
  let (x, y) as loc = Cities.get_idx city_idx cities in
  if Trackmap.has_track loc trackmap then v else (* Proceed only if no track at city *)
  let ai_idx = random_ai () in
  (* We now have a target city and a company *)
  if not @@ ai_exists ai_idx v then
      (* New company creation test at this city *)
      let demand_supply = Tilemap.demand_supply_sum tilemap ~x ~y ~range:2 in
      let age = (params.year - C.ref_year_ai_build_value) / 2 in
      let value = demand_supply / age in
      let cycles_value = 100 - (cycle mod 8192) / 128 in
      if cycles_value >= value then v else
      let closest_station = Station_map.find_nearest station_map loc in
      let create = match closest_station with
        | Some _ -> true
        | None when ai_idx = 0 -> true (* No player station but first opponent can still exist *)
        | _ -> false (* don't create another AI if no player station *)
      in
      if not create then v else
      let create = match closest_station with
       | Some station when Station.is_proper_station station ->
           (* Make sure we're not too close *)
           let station_loc = Station.get_loc station in
           let dx, dy = Utils.dxdy loc station_loc in
           min dx dy > C.min_dist_btw_stations
       | _ -> true (* We don't care if no station or signaltower *)
      in
      if not create then v else
      let rec create_leader_loop () =
        let leader = Opponent.random_of_region params.region random in
        let exists = IntMap.fold (fun _ ai acc -> acc || Opponent.equal_name ai.opponent.name leader) v.ais false in
        if exists then create_leader_loop () else leader
      in
      let leader = create_leader_loop () in
      v
  else v
      

(* 

  else
    let player_controlled = Stock_market.controls_company C.player ~target:ai_idx stocks in
    let city, city_x, city_y, done =
      match get_ai ai_idx v with
      | Some info when player_controlled && Option.is_none info.build_order ->
          (* player_controller and no order-> bail here *)
          city, city_x, city_y, true
      | _ -> (*TODO: deal with player command *)
          if exists ai_player then
            let ai_player =
              if cycles <> 0 then
                let expand =
                  let mult = if ai_player.net_worth >= player_net_worth then 1 else 2 in
                  ai_player.opponent.expansionist + mult * 2
                in
                {ai_player with expand_counter=ai_player.expand_counter + expand}
              else
                ai_player
          else
*)

