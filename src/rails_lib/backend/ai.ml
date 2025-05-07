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
  build_order: (Utils.loc * Utils.loc) option;  (* order given to subservient company *)
  yearly_income: int; (* rough estimation of 8 * yearly income *)
  yearly_interest: int;
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
  ai_track: Utils.loc list;
} [@@deriving yojson]

let default () = {
  routes=Vector.create ();
  ai_of_city=IntMap.empty;
  rate_war_at_city=IntSet.empty;
  ais=IntMap.empty;
  ai_track=[];
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
  let net_worth = cash - loans + v.yearly_income * 2 + stock_value in
  {v with net_worth}

let ai_of_city city v = IntMap.get city v.ai_of_city

let city_rate_war city v = IntSet.mem city v.rate_war_at_city

let get_ai idx v = IntMap.get idx v.ais

let modify_ai idx v f =
  let ai = get_ai idx v in
  let ai2 = f ai in
  if ai2 === ai then v
  else
    {v with ais=IntMap.add idx ai2 v.ais}

let get_income player v = get_ai player v |> fun x -> x.yearly_income

let name player ~cities v =
  let p = get_ai player v

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
let _route_earn_money route_idx ~stocks ~params main_player_net_worth ~tilemap ~cities v =
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

let _find_closest_player_station_check_distance ~loc ~station_map ~ai_idx =
  let closest_station = Station_map.find_nearest station_map loc in
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
       min dx dy > C.min_dist_btw_stations
   | _ -> true (* We don't care if no station or signaltower *)
  in
  closest_station, create

let _try_to_create_ai ~tilemap ~station_map ~(params:Params.t) ~city_idx ~ai_idx ~stocks loc random v =
  (* New company creation test at this city *)
  let x, y = loc in
  let demand_supply = Tilemap.demand_supply_sum tilemap ~x ~y ~range:2 in
  let age = (params.year - C.ref_year_ai_build_value) / 2 in
  let value = demand_supply / age in
  let cycles_value = 100 - (params.cycle mod 8192) / 128 in
  if cycles_value >= value then `Update v else
  let closest_station, create = _find_closest_player_station_check_distance ~loc ~station_map ~ai_idx in
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
    yearly_income=5;
    yearly_interest;
    net_worth=50;
    revenue_ytd = (params.time + 2000) / 20;
    expand_counter=20;
  } in
  let v = {v with ais=IntMap.add ai.idx ai v.ais} in
  let stocks = Stock_market.add_ai_player ~player:ai.idx ~num_fin_periods:params.num_fiscal_periods stocks in
  let ui_msg = Ui_msg.NewCompany{opponent=opponent.name; city=loc} in
  `CreateAI(v, stocks, ui_msg)

let ai_routines ~stocks ~params ~main_player_net_worth ~tilemap ~trackmap ~cities random ~station_map v =
  let earn_random_route v =
    if Random.int 100 random <= num_routes v then
      let route_idx = random_route_idx random v in
      _route_earn_money route_idx ~stocks ~params main_player_net_worth ~tilemap ~cities v
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
  let loc = Cities.get_idx city_idx cities in
  if Trackmap.has_track loc trackmap then `Update v else (* Proceed only if no track at city *)
  let ai_idx = random_ai () in
  (* We now have a target city and a company *)
  if not @@ ai_exists ai_idx v then
    _try_to_create_ai ~tilemap ~station_map ~params ~city_idx ~ai_idx ~stocks loc random v
  else
    let target_city = ()
    in
    `Update v
      
let new_ai_text name city cities =
  Printf.sprintf
  "New Railroad company\n\
   chartered in %s!\n\
   President: %s.\n"
   (Cities.get_name cities (fst city) (snd city))
   (Opponent.show_name name)

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

let _build_station src_city ~tgt_station_or_city ~cities ~trackmap ~tilemap random v =
  let src_loc = Cities.get_idx src_city cities in
  let tgt_loc = match tgt_station_or_city with
    | `City idx -> Cities.get_idx idx cities
    | `Station loc -> loc
  in
  let ret = _build_track_btw_stations tgt_loc src_loc ~company ~trackmap ~tilemap random v in
  let trackmap, v = match ret with
    | Some (trackmap, ai_track) ->
        trackmap, {v with ai_track}
    | None ->
        let v =
          if get_income company v > 64 then
            modify_ai company v (fun p ->
            {p with expand_counter=p.expand_counter / 2})
          else v
        in
        trackmap, v
  in
  let dist = Utils.classic_dist src_loc tgt_loc in
  ()

