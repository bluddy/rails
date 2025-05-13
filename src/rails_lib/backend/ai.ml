open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils.Infix

module C = Constants
module Vector = Utils.Vector
module Hashtbl = Utils.Hashtbl
module IntMap = Utils.IntMap
module IntSet = Utils.IntSet
module List = Utils.List

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
  build_order: (int * int) option;  (* order given to subservient company (city_idx, city_idx)*)
  track_length: int;
  yearly_interest: int;
  net_worth: int;
  revenue_ytd: int;
  expand_ctr: int;  (* When the AI player wants to expand *)
} [@@deriving yojson]

  (* Global AI data *)
type t = {
  routes: route Vector.vector;  (* Routes for all AIs *)
  ai_of_city: int IntMap.t; (* Each city can only have one ai *)
  rate_war_at_city: IntSet.t;
  ais: ai_player IntMap.t; (* AI player info *)
  ai_track: Utils.loc list;
  mutable financial_ctr: int; (* Affects ai financial actions *)
  last_ai_to_buy_player_stock: int option;
} [@@deriving yojson]

(* last_ai_to_buy is a weird one.
   The game only seems to allow one company to own your stock at a time.
   So saving the last ai to buy is kinda silly
   *)

let default () = {
  routes=Vector.create ();
  ai_of_city=IntMap.empty;
  rate_war_at_city=IntSet.empty;
  ais=IntMap.empty;
  ai_track=[];
  financial_ctr=0;
  last_ai_to_buy_player_stock=None;
}

let num_routes v = Vector.length v.routes

let get_route i v = Vector.get v.routes i

let random_route_idx random v = 
  Random.int (num_routes v) random

let owned_by_player stocks company =
  Stock_market.controls_company C.player ~target:company stocks

let new_ai_idx v =
  match IntMap.max_binding_opt v.ais with
  | Some (i, _) -> i + 1
  | _ -> 0

    (* Update valuation only for AI players *)
let update_valuation player stocks v =
  let loans = v.bonds / 10 in
  let cash = v.cash / 10 in
  let stock_value = Stock_market.total_owned_stock_value player stocks in
  let net_worth = cash - loans + v.track_length * 2 + stock_value in
  {v with net_worth}

let ai_of_city city v = IntMap.get city v.ai_of_city

let city_rate_war city v = IntSet.mem city v.rate_war_at_city

let get_ai idx v = IntMap.get idx v.ais

let get_ai_exn idx v = IntMap.find idx v.ais

let modify_ai idx v f =
  let ai = get_ai_exn idx v in
  let ai2 = f ai in
  if ai2 === ai then v
  else
    let ais = IntMap.add idx ai2 v.ais in
    {v with ais}

let get_track_len player v =
  get_ai_exn player v |> fun x -> x.track_length

let name player ~cities v =
  let p = get_ai_exn player v in
  let city1_s = Cities.name_of_idx p.city1 cities in
  match p.city2 with
  | None -> city1_s ^ " RR"
  | Some city2 -> 
    let city2_s = Cities.name_of_idx city2 cities in
    Printf.sprintf "%s & %s RR" city1_s city2_s

let ai_exists idx v = IntMap.mem idx v.ais

let _route_value city1 city2 ~tilemap ~(params:Params.t) =
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
let _route_earn_money route_idx ~stocks ~params main_player_net_worth ~tilemap ~cities v =
  let city1, city2 = Vector.get v.routes route_idx in
  let player_idx = Option.get_exn_or "AI player idx not found" @@ ai_of_city city1 v in
  let ai_player = IntMap.find player_idx v.ais in
  let city1_loc, city2_loc = Cities.loc_of_idx city1 cities, Cities.loc_of_idx city2 cities in
  let value = _route_value city1_loc city2_loc ~tilemap ~params in
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

let _try_to_create_ai ~tilemap ~stations ~(params:Params.t) ~city_idx ~ai_idx ~stocks loc random v =
  (* New company creation test at this city *)
  let demand_supply = Tilemap.demand_supply_sum_of_loc loc tilemap ~range:2 in
  let age = (params.year - C.ref_year_ai_build_value) / 2 in
  let value = demand_supply / age in
  let cycles_value = 100 - (params.cycle mod 8192) / 128 in
  if cycles_value >= value then `Update v else
  let find_closest_player_station_check_distance ~loc ~stations ~ai_idx =
    let closest_station = Station_map.find_nearest stations loc in
    let create = match closest_station with
      | Some _ -> true
      | None when ai_idx = 0 -> true (* No player station but first opponent can still exist *)
      | _ -> false (* don't create another AI if no player station *)
    in
    if not create then None, false else
    let create = match closest_station with
     | Some station when Station.is_proper_station station ->
         (* Make sure we're not too close *)
         let dx, dy = Utils.dxdy loc (Station.get_loc station) in
         dx > C.min_dist_btw_stations || dy > C.min_dist_btw_stations
     | _ -> true (* We don't care if no station or signaltower *)
    in
    closest_station, create
  in
  let closest_station, create = find_closest_player_station_check_distance ~loc ~stations ~ai_idx in
  if not create then `Update v else
  let rec create_leader_loop () =
    let leader = Opponent.random_of_region params.region random in
    let exists = IntMap.fold (fun _ ai acc -> acc || Opponent.equal_name ai.opponent.name leader) v.ais false in
    if exists then create_leader_loop () else leader
  in
  let leader = create_leader_loop () in
  let opponent = Opponent.t_of_leader leader in
  let create =
    (* Now check that our leader can spawn this far from our station
       The more build skill, the closer he wants to be? *)
    match closest_station with
    | Some station ->
       let dist = Utils.classic_dist loc (Station.get_loc station) in
       let value = 300 / (opponent.build_skill + 2) in
       if value >= dist then true else false
    | _ -> true
  in
  if not create then `Update v else
  let yearly_interest =
    5 * (8 - opponent.financial_skill - Climate.to_enum params.climate)
  in
  let ai = {
    idx = new_ai_idx v;
    opponent;
    city1=city_idx;
    city2=None;
    cash=900;
    bonds=500;
    build_order=None;
    track_length=5;
    yearly_interest;
    net_worth=50;
    revenue_ytd = (params.time + 2000) / 20;
    expand_ctr=20;
  } in
  let ais = IntMap.add ai.idx ai v.ais in
  let ai_of_city = IntMap.add city_idx ai_idx v.ai_of_city in
  let v = {v with ais; ai_of_city} in
  Tilemap.set_tile_at_loc loc Tile.EnemyStation tilemap;
  let stocks = Stock_market.add_ai_player ~player:ai.idx ~num_fin_periods:params.num_fiscal_periods stocks in
  let ui_msg = Ui_msg.NewCompany{opponent=opponent.name; city=loc} in
  `CreateAI(tilemap, v, stocks, ui_msg)

let new_ai_text name city cities =
  Printf.sprintf
  "New Railroad company\n\
   chartered in %s!\n\
   President: %s.\n"
   (Cities.name_of_loc city cities)
   (Opponent.show_name name)

let _dir_from_dx_dy dx dy =
  let adx, ady = abs dx, abs dy in
  if 2 * ady < adx then
    (* dx >> dy *)
    if dx <= 0 then Dir.Right else Left
  else if 2 * adx < ady then
      (* dy >> dx *)
      if dy <= 0 then Up else Down
  else
      match dx <= 0, dy <= 0 with
      | true, true -> UpLeft
      | true, false -> DownLeft
      | false, true -> UpRight
      | false, false -> DownRight

type tgt_src = [`Tgt | `Src] [@@deriving eq]

(* This function starts with the station locations for src and targets *)
let _build_track_btw_stations tgt_loc src_loc ~company ~trackmap ~tilemap random ~ai_track =
  (* Get general dir from deltas *)
  let is_id = function `id -> true | _ -> false in
  let shift = function `ccw -> Dir.ccw | `id -> Fun.id | `cw -> Dir.cw in

  let rec connect_stations ~trackmap ~ai_track tgt_loc src_loc tgt_at_station src_at_station =
    let dx, dy = Utils.s_dxdy src_loc tgt_loc in
    let tgt_real_dir = _dir_from_dx_dy dx dy in
    let src_real_dir = Dir.opposite tgt_real_dir in
    let real_dist = Utils.classic_dist src_loc tgt_loc in

    if real_dist = 0 then Some(trackmap, ai_track) else

    let idx_vars = [
        `Tgt, (tgt_loc, tgt_real_dir, src_loc, tgt_at_station);
        `Src, (src_loc, src_real_dir, tgt_loc, src_at_station);
      ]
    in

    let search_for_min_cost_dir () =
      List.map (fun (spec, (((x, y) as loc), real_dir, ((x2, y2) as loc2), _)) ->
        let dx, dy = Utils.s_dxdy loc loc2 in
        let dir = _dir_from_dx_dy dx dy in

        let costs = List.map (fun dir_adjust ->
          let cost, is_river =
            let dir = (shift dir_adjust) dir in
            let x, y = Dir.adjust dir x y in
            let tile = Tilemap.get_tile tilemap x y in
            let is_river = match tile with River _ | Landing _ -> `IsRiver | _ -> `NoRiver in
            if real_dist <= 2 && not @@ is_id dir_adjust then 999, is_river else
            let cost, x, y, is_river = match tile with
             | Tile.Harbor _ | Ocean _ -> 999, x, y, `NoRiver
             | River _ | Landing _ ->
                (* Try crossing with bridge *)
                let x, y = Dir.adjust dir x y in
                begin match Tilemap.get_tile tilemap x y with
                | Tile.River _ -> 99 + 32, x, y, `NoRiver
                | Tile.Ocean _ | Tile.Harbor _ -> 999, x, y, `NoRiver
                | _ -> 32, x, y, `IsRiver
                end
              | _ -> 0, x, y, `NoRiver
            in
            let cost = if Trackmap.has_track (x, y) trackmap then cost + 64 else cost in
            let dir_diff = Dir.diff dir real_dir in
            (* Check 90 degrees *)
            let cost = if dir_diff > 2 then cost + 99 else cost in
            let cost = if dir_diff = 0 && real_dist > 4 then cost - 20 else cost in
            (* Penalize indirect solutions with distance *)
            let cost = if is_id dir_adjust then cost else cost + 512 / (real_dist + 4) in
            (* Account randomly for height diff *)
            let h1 = Tilemap.get_tile_height tilemap x y in
            let h2 = Tilemap.get_tile_height tilemap x2 y2 in
            let h_diff = abs(h1 - h2) in
            let roll = Random.int (2 * h_diff) random in
            let cost = cost + roll in
            cost, is_river
          in
          (dir_adjust, is_river), cost)
        [`ccw; `id; `cw]
        in
        let min_cost = List.min_f snd costs |> snd in
        spec, min_cost
      )
      idx_vars
    in
    let costs = search_for_min_cost_dir () in
    let get_river i = List.nth costs i |> snd |> fst |> snd in
    let min_idx = match get_river 0, get_river 1 with
      (* Highest priority -> crossing river *)
      | `IsRiver, _ -> `Tgt
      | _, `IsRiver -> `Src
      (* Then, close *)
      | _ when real_dist < 2 -> `Tgt
      (* Then, go by minimum *)
      | _ -> List.min_f (fun x -> x |> snd |> snd) costs |> snd |> fst
    in
    let dir_adjust = List.assoc ~eq:equal_tgt_src min_idx costs |> fst |> fst in
    let loc1, dir1, loc2, at_station = List.assoc ~eq:equal_tgt_src min_idx idx_vars in

    (* Build track going out *)
    let dir = shift dir_adjust @@ dir1 in
    let real_dist = Utils.classic_dist src_loc tgt_loc in

    let rec build_one_track ~trackmap ~ai_track loc1 loc2 at_station =
      let t = Trackmap.get_loc loc1 trackmap in
      let track_modify = match t, at_station with
        (* We don't care about crossing our own track (AI doesn't follow the rules.
           However, we can't do it right when leaving a station *)
        | Some track, `NotAtStation when track.player = company -> `Modify track
        (* We can build from a player station *)
        | Some track, `AtStation when track.player = C.player -> `Modify track
        | Some _, _ -> `NoModify
        | None, _ -> `Modify (Track.empty company @@ Track `Single)
      in
      let trackmap, ai_track = match track_modify with
        | `NoModify -> trackmap, ai_track
        | `Modify track ->
           let track = Track.add_dir track ~dir in
           let trackmap = Trackmap.set_loc loc1 track trackmap in
           trackmap, loc1::ai_track
      in

      (* Test dir diagonals *)
      let diag_surrounded =
        if Dir.is_diagonal dir then
          (* Problem if we have track on *both* sides of us *)
          let loc1 = Dir.adjust_loc (Dir.ccw dir) loc1 in
          let has_track_ccw = Trackmap.has_track loc1 trackmap in
          let loc1 = Dir.adjust_loc (Dir.cw dir) loc1 in
          let has_track_cw = Trackmap.has_track loc1 trackmap in
          has_track_ccw && has_track_cw
        else false
      in

      (* Move *)
      let loc1 = Dir.adjust_loc dir loc1 in
      let at_station = `NotAtStation in
      let tile = Tilemap.tile_at loc1 tilemap in
      let track = Trackmap.get_loc loc1 trackmap in
      let build_track_of_kind kind =
        (* Add track facing opposite way *)
        let track = Track.empty company kind
         |> Track.add_dir ~dir:(Dir.opposite dir) in
        let trackmap = Trackmap.set_loc loc1 track trackmap in
        trackmap, loc1::ai_track, at_station, `Ok
      in
      let trackmap, ai_track, at_station, ok = match tile, track with
       (* These things are only allowed if we're almost there *)
       | (Tile.Ocean _ | EnemyStation), _ when real_dist > 1 -> trackmap, ai_track, at_station, `Fail
       | _, Some _track when real_dist > 1 -> trackmap, ai_track, at_station, `Fail
       | EnemyStation, _ -> trackmap, ai_track, `AtStation, `Ok
       | (River _ | Landing _), _ -> build_track_of_kind @@ Track.Bridge Wood
       | _ when diag_surrounded && real_dist > 1 -> trackmap, ai_track, at_station, `Fail
       | _ -> build_track_of_kind @@ Track.Track `Single
      in
      match tile, min_idx, ok with
      (* Make sure we finish bridge in same direction *)
      | _, _, `Fail -> None
      | (River _ | Landing _), _, _ -> build_one_track ~trackmap ~ai_track loc1 loc2 at_station 
      (* Unflip src/tgt *)
      | _, `Tgt, _ -> connect_stations ~ai_track ~trackmap loc1 loc2 at_station src_at_station
      | _, `Src, _ -> connect_stations ~ai_track ~trackmap loc2 loc1 tgt_at_station at_station
    in
    build_one_track ~trackmap ~ai_track loc1 loc2 at_station 
  in
  connect_stations ~ai_track ~trackmap src_loc tgt_loc `AtStation `AtStation

  (* src: always AI-owned. tgt: sometimes player-owned
     We always supply a tgt_city, but sometimes it's really a tgt_station's
     location
   *)
  (* TODO: adjust player-owned flag logic for station *)
let _build_station tgt_city src_city ~tgt_station ~cities ~stations ~trackmap
                  ~tilemap ~company ~stocks ~params random v =
  let src_loc = Cities.loc_of_idx src_city cities in
  let src_name = Cities.name_of_loc src_loc cities in
  let tgt_loc, tgt_name = match tgt_station with
    | None -> Cities.loc_of_idx tgt_city cities, Cities.name_of_idx tgt_city cities
    | Some loc ->
      let station = Station_map.get_exn loc stations in
      loc, Station.get_name station
  in
  let ai_controlled_by_player = owned_by_player stocks company in
  let ret = _build_track_btw_stations tgt_loc src_loc ~company ~trackmap ~tilemap random ~ai_track:v.ai_track in
  let ai_name = name company ~cities v in
  let ai_player = IntMap.find company v.ais in
  match ret with
    | Some (trackmap, ai_track) -> (* Built *)
        let ui_msg = 
          let opponent = (get_ai_exn company v).opponent.name in
          Ui_msg.AiConnected {opponent; ai_name; src_name; tgt_name}
        in
        let update_station f = Station_map.update tgt_loc (Option.map f) stations in
        let rate_war_at_city, stations = match ai_controlled_by_player, tgt_station with
          | true, Some _ ->
              v.rate_war_at_city, update_station Station.set_to_union_station
          | false, Some _ ->
              (* Rate war *)
              let rate_war_at_city = IntSet.add tgt_city v.rate_war_at_city in
              let stations = update_station (Station.set_rate_war true `Half) in
              rate_war_at_city, stations
          | _ ->
            v.rate_war_at_city, stations
        in
        (* Set AI player snd city *)
        let city2 = match (get_ai_exn company v).city2 with
          | None -> Some tgt_city
          | x -> x
        in
        (* Compute cost of building route *)
        let dist = Utils.classic_dist src_loc tgt_loc in
        let cash =
          let v = (Climate.to_enum params.Params.climate) - ai_player.opponent.expansionist + 6 in
          let cost = v * dist * 3 + 100 in
          ai_player.cash - cost
        in
        let track_length = ai_player.track_length + dist * 5 in
        let expand_ctr = 0 in
        let ai_of_city = IntMap.add tgt_city company v.ai_of_city in
        Tilemap.set_tile_at_loc tgt_loc Tile.EnemyStation tilemap; (* Even draw for union station, apparently *)
        Vector.push v.routes (src_city, tgt_city); (* Add AI route *)
        let ai_player = {ai_player with city2; cash; track_length; expand_ctr} in
        let ais = IntMap.add company ai_player v.ais in
        let v = {v with ais; ai_of_city; ai_track; routes=v.routes; rate_war_at_city} in
        (* TODO: if not owned by player and connect to player, send rate war animation *)
        trackmap, tilemap, v, stations, Some ui_msg
    | None ->
      (* Failed to build *)
        let expand_ctr =
          if get_track_len company v > 64 then
            ai_player.expand_ctr / 2
          else ai_player.expand_ctr
        in
        let build_order, ui_msg =
          if ai_controlled_by_player then
            let ui_msg = Ui_msg.AiBuildOrderFailed{player=C.player; ai_name; src_name; tgt_name} in
            None, Some ui_msg
          else
            ai_player.build_order, None
        in
        let ai_player2 = [%up {ai_player with build_order; expand_ctr}] in
        let v = if ai_player2 === ai_player then v else {v with ais=IntMap.add company ai_player2 v.ais} in
        trackmap, tilemap, v, stations, ui_msg

let new_route_text ai_name src_name tgt_name =
  Printf.sprintf
  "%s\n\
  connects %s\n\
  to %s"
  ai_name src_name tgt_name

let build_order_fail_text ai_name src_name tgt_name =
  Printf.sprintf
  "%s\n\
  Survey from %s\n\
  to %s unsuccessful.\n"
  ai_name src_name tgt_name

let _try_to_build_station ~tilemap ~stations ~trackmap ~cities ~params ~city_idx ~ai_idx ~stocks ~player_net_worth loc random v =
  (* Use target city and company to expand *)
  let ai_player = get_ai_exn ai_idx v in
  match ai_of_city city_idx v with
  | Some company when company <> ai_idx -> `Update v
  | _ ->
    let owned_by_player = owned_by_player stocks ai_idx in
    (* If owned by player, do nothing but orders *)
    if owned_by_player && Option.is_none ai_player.build_order then `Update v else
    let ai_player =
      let expand_ctr =
        let value = if ai_player.net_worth >= player_net_worth then 2 else 4 in
        ai_player.expand_ctr + value * ai_player.opponent.expansionist
      in
      {ai_player with expand_ctr}
    in
    let src_city, tgt_city = match ai_player.build_order with
      | Some (city1, city2) when owned_by_player -> city1, city2
      | _ ->
        let find_closest_ai_city_to_other_city ~src_city ~ai =
          let src_loc = Cities.loc_of_idx src_city cities in
          IntMap.fold (fun tgt_city city_ai acc ->
            if ai = city_ai && not @@ city_rate_war tgt_city v then
              let tgt_loc = Cities.loc_of_idx tgt_city cities in
              let dist = Utils.classic_dist src_loc tgt_loc in
              match acc with
                | Some (min_dist, _) ->
                      if dist < min_dist then Some(dist, tgt_city) else acc
                | None -> Some (dist, tgt_city)
            else acc)
          v.ai_of_city
          None
          |> Option.map snd
        in
        let src_city = find_closest_ai_city_to_other_city ~src_city:city_idx ~ai:ai_idx
          |> Option.get_exn_or "missing closest AI city"
        in
        src_city, city_idx
    in
    let src_loc, tgt_loc = Cities.loc_of_idx src_city cities, Cities.loc_of_idx tgt_city cities in
    let dist = Utils.classic_dist src_loc tgt_loc in
    let is_home_city = src_city = ai_player.city1 in
    let combined_check =
        owned_by_player ||
        let first_check = ai_player.track_length <= 48 || not @@ Climate.strong params.Params.climate || is_home_city in
        let demand_supply =Tilemap.demand_supply_sum_of_loc src_loc tilemap ~range:2 in 
        let city_check = demand_supply > 0 in
        let route_value_dist_check =
          let route_value = _route_value src_loc tgt_loc ~tilemap ~params in
          let mult = if is_home_city then 2 else 1 in
          let route_value = route_value * mult in
          let max_dist = (6 * route_value) / (Climate.plus_4 params.climate) + (ai_player.cash / 32) in
          max_dist >= dist * 3
        in
        let bond_check = ai_player.opponent.financial_skill * 500 >= ai_player.bonds in 
        (first_check && city_check && route_value_dist_check && bond_check)
    in
    if not combined_check then `Update v else
    let cash_check =
      let build_cost = Climate.plus_4 params.climate * dist * 3 + 100 in
      ai_player.cash > build_cost
    in
    let will_check = ai_player.expand_ctr >= dist in
    if not (cash_check && will_check) then `Update v else
    let station_check =
      let closest_station = Station_map.find_nearest stations loc in
      match closest_station with
      | Some station when Station.is_proper_station station ->
         (* Make sure we're not too close *)
         let station_loc = Station.get_loc station in
         let dx, dy = Utils.dxdy loc station_loc in
         if dx > C.min_dist_btw_stations || dy > C.min_dist_btw_stations then
           `CanBuild
         else
            `TooClose station_loc
      | _ -> `CanBuild (* We don't care if no station or signaltower *)
    in
    match station_check with
      | `TooClose station_loc when owned_by_player ->
          `Build (_build_station tgt_city src_city ~tgt_station:(Some station_loc) ~cities
            ~stations ~trackmap ~tilemap ~company:ai_idx ~stocks ~params random v)

      | `TooClose ((x, y) as station_loc) when B_options.cutthroat params.options -> 
        (* Rate war can only happen with cutthroat *)
        let track_check = ai_player.track_length >= 75 in
        (* Not sure why this is done *)
        let player_share_check = Stock_market.owned_shares ~owner:C.player ~owned:ai_idx stocks < 60 in
        let value_check =
          let station = Station_map.get_exn station_loc stations in
          let lost_goods = Station.total_lost_supply station in
          let picked_up_goods = Station.total_picked_up_goods station in 
          let factor = if lost_goods <= picked_up_goods then 4 else 2 in
          (* High build skill -> lower value, better chance of building *)
          let value = factor * ((dist * 4) / (ai_player.opponent.build_skill + 2)) in
          let age = (params.year - C.ref_year_ai_build_value) / 2 in
          let demand_supply =Tilemap.demand_supply_sum_of_loc station_loc tilemap ~range:2 in 
          value < demand_supply / age
        in
        (* Find new target city (closest to station ) *)
        let tgt_city_loc = Cities.find_close cities x y ~range:999 |> Option.get_exn_or "can't find any city" in
        let tgt_city = Cities.idx_of_loc tgt_city_loc cities |> Option.get_exn_or "can't find idx of city loc" in
        let tgt_city_check = ai_of_city tgt_city v |> Option.is_some in
        if track_check && player_share_check && value_check && tgt_city_check then
          `Build (_build_station tgt_city src_city ~tgt_station:(Some station_loc) ~cities
            ~stations ~trackmap ~tilemap ~company:ai_idx ~stocks ~params random v)
        else `Update v

      | `TooClose _ -> `Update v  (* Too close to build normally *)

      | `CanBuild ->
          `Build (_build_station tgt_city src_city ~tgt_station:None ~cities
            ~stations ~trackmap ~tilemap ~company:ai_idx ~stocks ~params random v)

let ai_routines ~stocks ~params ~player_net_worth ~tilemap ~trackmap ~cities random ~stations v =
  let earn_random_route v =
    if Random.int 100 random <= num_routes v then
      let route_idx = random_route_idx random v in
      _route_earn_money route_idx ~stocks ~params player_net_worth ~tilemap ~cities v
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
  let loc = Cities.loc_of_idx city_idx cities in
  if Trackmap.has_track loc trackmap then `Update v else (* Proceed only if no track at city *)
  let ai_idx = random_ai () in
  (* We now have a target city and a company *)
  if not @@ ai_exists ai_idx v then
    _try_to_create_ai ~tilemap ~stations ~params ~city_idx ~ai_idx ~stocks loc random v
  else
    _try_to_build_station ~tilemap ~stations ~trackmap ~params ~city_idx ~cities ~ai_idx ~stocks ~player_net_worth loc random v


let ai_financial ~ai_idx ~stocks ~cycle ~player_cash v =
  (* Player-owned ais don't make financial decisions *)
  if not (ai_exists ai_idx v) || owned_by_player stocks ai_idx then `Nothing else
  let ai_player = get_ai_exn ai_idx v in
  financial_ctr <- v.financial_ctr + 1;
  let company_is_last_active = match v.last_ai_to_buy_player_stock with
    | Some idx when ai_idx = idx -> true
    | _ -> false
  in
  let last_ai_to_buy_player_stock =
    if cycle land 0xC0 = 0 then None else v.last_ai_to_buy_player_stock
  in
  let player_has_more_cash = player_cash > ai_player.cash in
  let player_in_ai_shares = Stock_marker.owned_shares ~owner:C.player ~owned:ai_idx stocks in
  let ai_treasury_shares = Stock_market.treasury_shares ai_idx stocks in
  let ai_controls_itself = Stock_market.controls_own_company ai_idx stocks in
  let ai_doing_badly =
    (* NOTE: this seems like a bug. This bonus is too small compared to number
             of shares *)
    let bonus = if player_has_more_cash then 1 else 0 in
    not (ai_controls_itself || player_in_ai_shares + bonus <= ai_treasury_shares)
  in
  `Nothing






