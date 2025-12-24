open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils.Infix

module C = Constants
module Vector = Utils.Vector
module Hashtbl = Utils.Hashtbl
module IntMap = Utils.IntMap
module IntSet = Utils.IntSet
module List = Utils.List
module U = Utils
module M = Money
module Random = Utils.Random

module LocMap = U.LocMap
module LocSet = U.LocSet

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

type route = {
  owner: Owner.t;
  src: U.loc;
  dst: U.loc;
  track: U.loc array; (* track for this route *)
} [@@deriving yojson]

 (* An AI Player *)
type ai_player = {
  idx: Owner.t;
  opponent: Opponent.t;
  city1: U.loc; (* home city *)
  city2: U.loc option; (* first route. Determines name *)
  cash: Money.t; (* all x1000 *)
  bonds: Money.t;
  build_order: (U.loc * U.loc) option;  (* order given to subservient company (city_idx, city_idx)*)
  track_length: int; (* Each piece of track is 5 *)
  yearly_interest: Money.t;
  net_worth: Money.t;
  revenue_ytd: Money.t;
  expand_ctr: int;  (* When the AI player wants to expand *)
} [@@deriving yojson]

  (* Global AI data *)
type t = {
  routes: route Vector.vector;  (* Routes for all AIs *)
  route_history: int list; (* routes length at each period *)
  ai_of_city: Owner.t LocMap.t; (* Each city can only have one ai *)
  rate_war_at_city: LocSet.t;
  ais: ai_player Owner.Map.t; (* AI player info *)
  mutable financial_ctr: int; (* Affects ai financial actions *)
  last_ai_to_buy_player_stock: Owner.t option;
} [@@deriving yojson]

(* last_ai_to_buy is a weird one.
   The game only seems to allow one company to own your stock at a time.
   So saving the last ai to buy is kinda silly
   *)

let default () = {
  routes=Vector.create ();
  route_history=[];
  ai_of_city=LocMap.empty;
  rate_war_at_city=LocSet.empty;
  ais=Owner.Map.empty;
  financial_ctr=0;
  last_ai_to_buy_player_stock=None;
}

let num_routes v = Vector.length v.routes

let get_route i v = Vector.get v.routes i

let random_route_idx random v = 
  if num_routes v > 0 then
    Random.int (num_routes v) random |> Option.some
  else None

let owned_by_player stocks company =
  Stock_market.controls_company C.player ~target:company stocks

let new_ai_idx () = Owner.create_ai ()

(* Update valuation for AI players *)
let _update_net_worth stocks ai_player =
  let open M in
  let loans = ai_player.bonds / 10 in
  let cash = ai_player.cash / 10 in
  let stock_value = Stock_market.total_owned_stock_value ai_player.idx stocks in
  let net_worth = cash - loans +~ Int.(ai_player.track_length * 2) + stock_value in
  {ai_player with net_worth}

let new_share_price_ player stocks v =
  let total_shares_div_10 = Stock_market.total_shares player stocks / 10 in
  let open M in
  let price =
    ((v.revenue_ytd / 3) + v.net_worth) / total_shares_div_10
    |> M.to_int |> Utils.clip ~min:1 ~max:999 |> M.of_int
  in
  Log.debug (fun f ->
    let old_price = Stock_market.share_price player stocks in
    f "update_net_worth old_price: %d, total_shares: %d, revenue_ytd: %d, net_worth: %d"
      (M.to_int old_price) total_shares_div_10 (M.to_int v.revenue_ytd) (M.to_int v.net_worth));
  price


let ai_of_city city v = LocMap.get city v.ai_of_city

let ai_of_city_map v = v.ai_of_city

let remove_ai_of_city city v =
  {v with ai_of_city=LocMap.remove city v.ai_of_city}

let city_has_rate_war city v = LocSet.mem city v.rate_war_at_city

let set_city_rate_war city v =
  let rate_war_at_city = LocSet.add city v.rate_war_at_city in
  [%up{v with rate_war_at_city}]

let end_city_rate_war city v =
  let rate_war_at_city = LocSet.remove city v.rate_war_at_city in
  [%up{v with rate_war_at_city}]

let get_ai idx v = Owner.Map.get idx v.ais

let get_ai_exn idx v = Owner.Map.find idx v.ais

let get_opponent idx v = get_ai idx v |> Option.map (fun player -> player.opponent)

let ai_iter v = Owner.Map.keys v.ais

let modify_ai idx v f =
  let ai = get_ai_exn idx v in
  let ai2 = f ai in
  if ai2 === ai then v
  else
    let ais = Owner.Map.add idx ai2 v.ais in
    {v with ais}

let get_cash idx v = get_ai_exn idx v |> fun player -> player.cash

let get_bonds idx v = get_ai_exn idx v |> fun player -> player.bonds

let get_net_worth idx v = get_ai_exn idx v |> fun player -> player.net_worth

let get_track_length idx v = get_ai_exn idx v |> fun player -> player.track_length

let add_cash idx cash v =
  modify_ai idx v (fun ai_player -> {ai_player with cash=M.(ai_player.cash + cash)})

let get_name player ~cities v =
  let p = get_ai_exn player v in
  let city1_s = Cities.name_of_loc p.city1 cities in
  match p.city2 with
  | None -> city1_s ^ " RR"
  | Some city2 -> 
    let city2_s = Cities.name_of_loc city2 cities in
    Printf.sprintf "%s & %s RR" city1_s city2_s

let ai_exists idx v = Owner.Map.mem idx v.ais

let nth_or_none n v =
  Owner.Map.nth_key n v.ais

let _random_ai_or_none random v =
  (* Roll. If we get a player, return it *)
  let roll = Random.int C.max_ai_players random in
  nth_or_none roll v

let _route_value city1 city2 ~tilemap ~(params:Params.t) =
  let get_demand_supply loc =
    Tilemap.demand_supply_sum loc ~range:2 tilemap 
  in
  let total = get_demand_supply city1 + get_demand_supply city2 in
  let age = params.year - C.ref_year_ai_route_value in
  (* More demanding over time *)
  let value = total / age in
  let value =
    if Region.is_west_us params.region then
      (* east-west routes have more value *)
      let dx = abs @@ fst city1 - fst city2 in
      let dy = abs @@ snd city1 - snd city2 in
      ((3 * dx + dy) * value) / ((dx + dy) * 2)
    else value
  in
  value |> M.of_int

  (* Simulate earning money on a route *)
let _route_earn_money route_idx ~stocks ~params main_player_net_worth ~tilemap v =
  let {src; dst; _} = Vector.get v.routes route_idx in
  let city1, city2 = src, dst in
  let player_idx = Option.get_exn_or "AI player idx not found" @@ ai_of_city city1 v in
  let ai_player = Owner.Map.find player_idx v.ais in
  let value = _route_value city1 city2 ~tilemap ~params in
  let div = if city_has_rate_war city1 v || city_has_rate_war city2 v then 6 else 3 in
  let value = M.div value div in
  let value = M.mult value (ai_player.opponent.management + Climate.to_enum params.climate + 3) in
  (* Higher difficulty -> earns more *)
  let div = if owned_by_player stocks ai_player.idx then 10
            else 10 - B_options.difficulty_to_enum params.options.difficulty in
  let value = M.div value div in
  let value = if U.equal_loc ai_player.city1 city2 && Money.(main_player_net_worth >= ai_player.net_worth)
              then M.(value * 2) else value
  in
  let open M in
  let revenue_ytd = ai_player.revenue_ytd + value in
  let cash = if ai_player.cash < C.ai_max_cash then
    ai_player.cash + value else ai_player.cash
  in
  let ai_player = {ai_player with revenue_ytd; cash} in
  let ais = Owner.Map.add player_idx ai_player v.ais in
  {v with ais}

let _try_to_create_ai ?(force=false) ~tilemap ~stations ~first_ai ~(params:Params.t) ~city ~stocks random v =
  (* New company creation test at this city *)
  (* Log.debug (fun f -> f "_try_to_create_ai. first_ai: %b. city: %s" first_ai (Utils.show_loc city)); *)
  let player_idx = C.player in
  let demand_supply = Tilemap.demand_supply_sum city tilemap ~range:2 in
  let age = (params.year - C.ref_year_ai_build_value) / 2 in
  let value = demand_supply / age in
  (* at 8192, reach min of 36 *)
  let cycles_value = 100 - (params.cycle mod 8192) / 128 in
  (* Log.debug (fun f -> f "_try_to_create_ai 1 age %d, value %d, cycles_value %d" age value cycles_value); *)
  if cycles_value >= value && not force then `Update v else
  let find_closest_player_station_check_distance () =
    let closest_station = Station_map.find_nearest ~player_idx city stations in
    let create = match closest_station with
      | Some _ -> true
      | None when first_ai-> true (* No player station but first opponent can still exist *)
      | _ -> false (* don't create another AI if no player station *)
    in
    if not create then None, false else
    let create = match closest_station with
     | Some station when Station.is_proper_station station ->
         (* Make sure we're not too close *)
         let dx, dy = Utils.dxdy city (Station.get_loc station) in
         dx > C.min_dist_btw_stations || dy > C.min_dist_btw_stations
     | _ -> true (* We don't care if no station or signaltower *)
    in
    closest_station, create
  in
  let closest_station, create =
    find_closest_player_station_check_distance () in
  (* Log.debug (fun f -> f "_try_to_create_ai 2"); *)
  if not create && not force then `Update v else
  let rec create_leader_loop () =
    let leader = Opponent.random_of_region params.region random in
    let exists = Owner.Map.fold (fun _ ai acc -> acc || Opponent.equal_name ai.opponent.name leader) v.ais false in
    if exists then create_leader_loop () else leader
  in
  let leader = create_leader_loop () in
  let opponent = Opponent.t_of_leader leader in
  let create =
    (* Now check that our leader can spawn this far from our station
       The more build skill, the closer he wants to be? *)
    match closest_station with
    | Some station ->
       let dist = Utils.classic_dist city (Station.get_loc station) in
       let value = 300 / (opponent.build_skill + 2) in
       if value >= dist then true else false
    | _ -> true
  in
  if not create && not force then `Update v else
  let yearly_interest =
    5 * (8 - opponent.financial_skill - Climate.to_enum params.climate) |> M.of_int
  in
  let ai = {
    idx = new_ai_idx ();
    opponent;
    city1=city;
    city2=None;
    cash=M.of_int 900;
    bonds=M.of_int 500;
    build_order=None;
    track_length=5;
    yearly_interest;
    net_worth=M.of_int 50;
    revenue_ytd = (params.time + 2000) / 20 |> M.of_int;
    expand_ctr=20;
  } in
  let ais = Owner.Map.add ai.idx ai v.ais in
  let ai_of_city = LocMap.add city ai.idx v.ai_of_city in
  let v = {v with ais; ai_of_city} in
  let tile = Tilemap.get_tile city tilemap in
  Tilemap.set_tile city (Tile.EnemyStation{owner=ai.idx; over=tile}) tilemap;
  let stocks = Stock_market.add_ai_player ai.idx ~num_fin_periods:params.num_fiscal_periods stocks in
  let ui_msg = Ui_msg.NewCompany{opponent=opponent.name; city} in
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
let _build_track_btw_stations tgt_loc src_loc ~company ~tracks ~tilemap random =
  (* Get general dir from deltas *)
  let is_id = function `id -> true | _ -> false in
  let shift = function `ccw -> Dir.ccw | `id -> Fun.id | `cw -> Dir.cw in
  let ai_track = [] in

  let rec connect_stations ~tracks ~ai_track tgt_loc src_loc tgt_at_station src_at_station =
    let dx, dy = Utils.s_dxdy src_loc tgt_loc in
    let tgt_real_dir = _dir_from_dx_dy dx dy in
    let src_real_dir = Dir.opposite tgt_real_dir in
    let real_dist = Utils.classic_dist src_loc tgt_loc in

    if real_dist = 0 then Some(tracks, ai_track) else

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
            let tile = Tilemap.get_tile_xy x y tilemap in
            let is_river = match tile with River _ | Landing _ -> `IsRiver | _ -> `NoRiver in
            if real_dist <= 2 && not @@ is_id dir_adjust then 999, is_river else
            let cost, x, y, is_river = match tile with
             | Tile.Harbor _ | Ocean _ -> 999, x, y, `NoRiver
             | River _ | Landing _ ->
                (* Try crossing with bridge *)
                let x, y = Dir.adjust dir x y in
                begin match Tilemap.get_tile_xy x y tilemap with
                | Tile.River _ -> 99 + 32, x, y, `NoRiver
                | Tile.Ocean _ | Tile.Harbor _ -> 999, x, y, `NoRiver
                | _ -> 32, x, y, `IsRiver
                end
              | _ -> 0, x, y, `NoRiver
            in
            let cost = if Trackmap.has_track (x, y) tracks then cost + 64 else cost in
            let dir_diff = Dir.diff dir real_dir in
            (* Check 90 degrees *)
            let cost = if dir_diff > 2 then cost + 99 else cost in
            let cost = if dir_diff = 0 && real_dist > 4 then cost - 20 else cost in
            (* Penalize indirect solutions with distance *)
            let cost = if is_id dir_adjust then cost else cost + 512 / (real_dist + 4) in
            (* Account randomly for height diff *)
            let h1 = Tilemap.get_tile_height_xy x y tilemap in
            let h2 = Tilemap.get_tile_height_xy x2 y2 tilemap in
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

    let rec build_one_track ~tracks ~ai_track loc1 loc2 at_station =
      let t = Trackmap.get loc1 tracks in
      let track_modify = match t, at_station with
        (* We don't care about crossing our own track (AI doesn't follow the rules.
           However, we can't do it right when leaving a station *)
        | Some track, `NotAtStation when Owner.(track.player = company) -> `Modify track
        (* We can build from a player station *)
        | Some track, `AtStation when Owner.(track.player = C.player) -> `Modify track
        | Some _, _ -> `NoModify
        | None, _ -> `Modify (Track.empty company @@ Track `Single)
      in
      let tracks, ai_track = match track_modify with
        | `NoModify -> tracks, ai_track
        | `Modify track ->
           let track = Track.add_dir track ~dir in
           let tracks = Trackmap.set loc1 track tracks in
           tracks, loc1::ai_track
      in

      (* Test dir diagonals *)
      let diag_surrounded =
        if Dir.is_diagonal dir then
          (* Problem if we have track on *both* sides of us *)
          let loc1 = Dir.adjust_loc (Dir.ccw dir) loc1 in
          let has_track_ccw = Trackmap.has_track loc1 tracks in
          let loc1 = Dir.adjust_loc (Dir.cw dir) loc1 in
          let has_track_cw = Trackmap.has_track loc1 tracks in
          has_track_ccw && has_track_cw
        else false
      in

      (* Move *)
      let loc1 = Dir.adjust_loc dir loc1 in
      let at_station = `NotAtStation in
      let tile = Tilemap.get_tile loc1 tilemap in
      let track = Trackmap.get loc1 tracks in
      let build_track_of_kind kind =
        (* Add track facing opposite way *)
        let track = Track.empty company kind
         |> Track.add_dir ~dir:(Dir.opposite dir) in
        let tracks = Trackmap.set loc1 track tracks in
        tracks, loc1::ai_track, at_station, `Ok
      in
      let tracks, ai_track, at_station, ok = match tile, track with
       (* These things are only allowed if we're almost there *)
       | (Tile.Ocean _ | EnemyStation _), _ when real_dist > 1 -> tracks, ai_track, at_station, `Fail
       | _, Some _track when real_dist > 1 -> tracks, ai_track, at_station, `Fail
       | EnemyStation _, _ -> tracks, ai_track, `AtStation, `Ok
       | (River _ | Landing _), _ -> build_track_of_kind @@ Track.Bridge Wood
       | _ when diag_surrounded && real_dist > 1 -> tracks, ai_track, at_station, `Fail
       | _ -> build_track_of_kind @@ Track.Track `Single
      in
      match tile, min_idx, ok with
      (* Make sure we finish bridge in same direction *)
      | _, _, `Fail -> None
      | (River _ | Landing _), _, _ -> build_one_track ~tracks ~ai_track loc1 loc2 at_station 
      (* Unflip src/tgt *)
      | _, `Tgt, _ -> connect_stations ~ai_track ~tracks loc1 loc2 at_station src_at_station
      | _, `Src, _ -> connect_stations ~ai_track ~tracks loc2 loc1 tgt_at_station at_station
    in
    build_one_track ~tracks ~ai_track loc1 loc2 at_station 
  in
  connect_stations ~ai_track ~tracks src_loc tgt_loc `AtStation `AtStation

  (* src: always AI-owned. tgt: sometimes player-owned
     We always supply a tgt_city, but sometimes it's really a tgt_station's
     location
   *)
  (* TODO: adjust player-owned flag logic for station *)
let _build_station tgt_city src_city ~tgt_station ~cities ~stations ~tracks
                  ~tilemap ~company ~stocks ~params random v =
  let player_idx = C.player in
  let src_loc = src_city in
  let src_name = Cities.name_of_loc src_loc cities in
  let tgt_loc, tgt_name = match tgt_station with
    | None -> tgt_city, Cities.name_of_loc tgt_city cities
    | Some loc ->
      let station = Station_map.get_exn loc stations in
      loc, Station.get_name station
  in
  let ai_controlled_by_player = owned_by_player stocks company in
  let ret = _build_track_btw_stations tgt_loc src_loc ~company ~tracks ~tilemap random in
  let ai_name = get_name company ~cities v in
  let ai_player = Owner.Map.find company v.ais in
  match ret with
    | Some (tracks, ai_track) -> (* Built *)
        let ui_msg = 
          let opponent = (get_ai_exn company v).opponent.name in
          Ui_msg.AiConnected {opponent; ai_name; src_name; tgt_name}
        in
        let update_station f = Station_map.update tgt_loc f stations in
        let rate_war_at_city, stations, msgs = match ai_controlled_by_player, tgt_station with
          | true, Some _ ->
              let msg = Ui_msg.UnionStation{player_idx; station=tgt_loc} in
              v.rate_war_at_city, update_station Station.set_to_union_station, [msg]
          | false, Some _ ->
              (* Rate war *)
              let rate_war_at_city = LocSet.add tgt_city v.rate_war_at_city in
              let stations = update_station Station.set_rate_war in
              let msg = Ui_msg.RateWarDeclared{player_idx; other_player_idx=company; station=tgt_loc} in
              rate_war_at_city, stations, [msg]
          | _ ->
            v.rate_war_at_city, stations, []
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
          let cost = v * dist * 3 + 100 |> M.of_int in
          M.(ai_player.cash - cost)
        in
        let track_length = ai_player.track_length + dist * 5 in
        let expand_ctr = 0 in
        let ai_of_city = LocMap.add tgt_city company v.ai_of_city in
        let tile = Tilemap.get_tile tgt_loc tilemap in
        Tilemap.set_tile tgt_loc (Tile.EnemyStation{owner=company; over=tile}) tilemap; (* Even draw for union station, apparently *)
        let route = {owner=company; src=src_city; dst=tgt_city; track=List.rev ai_track |> Array.of_list} in
        Vector.push v.routes route; (* Add AI route *)
        let ai_player = {ai_player with city2; cash; track_length; expand_ctr} in
        let ais = Owner.Map.add company ai_player v.ais in
        let v = {v with ais; ai_of_city; routes=v.routes; rate_war_at_city} in

        tracks, tilemap, v, stations, ui_msg::msgs
    | None ->
      (* Failed to build *)
        let expand_ctr =
          if get_track_length company v > 64 then
            ai_player.expand_ctr / 2
          else ai_player.expand_ctr
        in
        let build_order, msgs =
          if ai_controlled_by_player then
            let ui_msg = Ui_msg.AiBuildOrderFailed{player_idx=C.player; ai_name; src_name; tgt_name} in
            None, [ui_msg]
          else
            ai_player.build_order, []
        in
        let ai_player2 = [%up {ai_player with build_order; expand_ctr}] in
        let v = if ai_player2 === ai_player then v else {v with ais=Owner.Map.add company ai_player2 v.ais} in
        tracks, tilemap, v, stations, msgs

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

let _try_to_build_station ~tilemap ~stations ~tracks ~cities ~params ~city ~ai_idx ~stocks ~player_net_worth random v =
  (* Use target city and company to expand *)
  let ai_player = get_ai_exn ai_idx v in
  let player_idx = C.player in
  match ai_of_city city v with
  | Some company when Owner.(company <> ai_idx) -> `Update v
  | _ ->
    let owned_by_player = owned_by_player stocks ai_idx in
    (* If owned by player, do nothing but orders *)
    if owned_by_player && Option.is_none ai_player.build_order then `Update v else
    let ai_player =
      let expand_ctr =
        let value = if M.(ai_player.net_worth >= player_net_worth) then 2 else 4 in
        ai_player.expand_ctr + value * ai_player.opponent.expansionist
      in
      {ai_player with expand_ctr}
    in
    let src_city, tgt_city = match ai_player.build_order with
      | Some (city1, city2) when owned_by_player -> city1, city2
      | _ ->
        let find_closest_ai_city_to_other_city ~src_city ~ai =
          LocMap.fold (fun tgt_city city_ai acc ->
            if Owner.(ai = city_ai) && not @@ city_has_rate_war tgt_city v then
              let dist = Utils.classic_dist src_city tgt_city in
              match acc with
                | Some (min_dist, _) ->
                      if dist < min_dist then Some(dist, tgt_city) else acc
                | None -> Some (dist, tgt_city)
            else acc)
          v.ai_of_city
          None
          |> Option.map snd
        in
        let src_city = find_closest_ai_city_to_other_city ~src_city:city ~ai:ai_idx
          |> Option.get_exn_or "missing closest AI city"
        in
        src_city, city
    in
    let src_loc, tgt_loc = src_city, tgt_city in
    let dist = Utils.classic_dist src_loc tgt_loc in
    let is_home_city = U.equal_loc src_city ai_player.city1 in
    let combined_check =
        owned_by_player ||
        let first_check = ai_player.track_length <= 48 || not @@ Climate.strong params.Params.climate || is_home_city in
        let demand_supply =Tilemap.demand_supply_sum src_loc tilemap ~range:2 in 
        let city_check = demand_supply > 0 in
        let route_value_dist_check =
          let route_value = _route_value src_loc tgt_loc ~tilemap ~params in
          let mult = if is_home_city then 2 else 1 in
          let route_value = M.mult route_value mult in
          let max_dist = M.((route_value * 6) / (Climate.plus_4 params.climate) + (ai_player.cash / 32)) |> M.to_int in
          max_dist >= dist * 3
        in
        let bond_check = ai_player.opponent.financial_skill * 500 >= (ai_player.bonds |> M.to_int) in 
        (first_check && city_check && route_value_dist_check && bond_check)
    in
    if not combined_check then `Update v else
    let cash_check =
      let build_cost = Climate.plus_4 params.climate * dist * 3 + 100 |> M.of_int in
      M.(ai_player.cash > build_cost)
    in
    let will_check = ai_player.expand_ctr >= dist in
    if not (cash_check && will_check) then `Update v else
    let station_check =
      let closest_station = Station_map.find_nearest ~player_idx city stations in
      match closest_station with
      | Some station when Station.is_proper_station station ->
         (* Make sure we're not too close *)
         let station_loc = Station.get_loc station in
         let dx, dy = Utils.dxdy city station_loc in
         if dx > C.min_dist_btw_stations || dy > C.min_dist_btw_stations then
           `CanBuild
         else
            `TooClose station_loc
      | _ -> `CanBuild (* We don't care if no station or signaltower *)
    in
    match station_check with
      | `TooClose station_loc when owned_by_player ->
          `Build (_build_station tgt_city src_city ~tgt_station:(Some station_loc) ~cities
            ~stations ~tracks ~tilemap ~company:ai_idx ~stocks ~params random v)

      | `TooClose station_loc when B_options.cutthroat params.options -> 
        (* Rate war can only happen with cutthroat *)
        let track_check = ai_player.track_length >= 75 in
        (* Not sure why this is done *)
        let player_share_check = Stock_market.owned_shares C.player ~owned:ai_idx stocks < 60 in
        let value_check =
          let station = Station_map.get_exn station_loc stations in
          let lost_goods = Station.total_lost_supply station in
          let picked_up_goods = Station.total_picked_up_goods station in 
          let factor = if lost_goods <= picked_up_goods then 4 else 2 in
          (* High build skill -> lower value, better chance of building *)
          let value = factor * ((dist * 4) / (ai_player.opponent.build_skill + 2)) in
          let age = (params.year - C.ref_year_ai_build_value) / 2 in
          let demand_supply =Tilemap.demand_supply_sum station_loc tilemap ~range:2 in 
          value < demand_supply / age
        in
        (* Find new target city (closest to station ) *)
        let tgt_city = Cities.find_close station_loc cities ~range:999 |> Option.get_exn_or "can't find any city" in
        let tgt_city_check = ai_of_city tgt_city v |> Option.is_some in
        if track_check && player_share_check && value_check && tgt_city_check then
          `Build (_build_station tgt_city src_city ~tgt_station:(Some station_loc) ~cities
            ~stations ~tracks ~tilemap ~company:ai_idx ~stocks ~params random v)
        else `Update v

      | `TooClose _ -> `Update v  (* Too close to build normally *)

      | `CanBuild ->
          `Build (_build_station tgt_city src_city ~tgt_station:None ~cities
            ~stations ~tracks ~tilemap ~company:ai_idx ~stocks ~params random v)

(* Main AI routines for building track *)
let ai_track_routines ?(force_create=false) ~stocks ~params ~player_net_worth ~tilemap ~tracks ~cities random ~stations v =
  let earn_random_route v =
    if Random.int 100 random <= num_routes v then
      match random_route_idx random v with
      | Some route_idx -> _route_earn_money route_idx ~stocks ~params player_net_worth ~tilemap v
      | None -> v
    else v
  in
  let random_city () =
      let rec empty_city_loop () =
        let city_idx = Cities.random random cities in
        if LocMap.mem city_idx v.ai_of_city then
          empty_city_loop ()
        else
          city_idx
      in
      empty_city_loop ()
  in
  (* Earn 2x in random routes *)
  let v = earn_random_route v in
  let v = earn_random_route v in
  let city = random_city () in
  if Trackmap.has_track city tracks then `Update v else (* Proceed only if no track at city *)
  let first_ai = Owner.Map.is_empty v.ais in
  match _random_ai_or_none random v with
  | None ->
    _try_to_create_ai ~force:force_create ~tilemap ~stations ~params ~city ~stocks ~first_ai random v
  | Some ai_idx ->
    _try_to_build_station ~tilemap ~stations ~tracks ~params ~city ~cities ~ai_idx ~stocks ~player_net_worth random v

let _ai_financial_decision ~ai_idx ~stocks ~cycle ~player_cash ~(params:Params.t) v =
  (* Player-owned ais don't make financial decisions *)
  let player_idx = C.player in
  if not (ai_exists ai_idx v) || owned_by_player stocks ai_idx then `Nothing else
  let ai_player = get_ai_exn ai_idx v in
  v.financial_ctr <- v.financial_ctr + 1;
  let company_is_last_active = match v.last_ai_to_buy_player_stock with
    | Some idx when Owner.(ai_idx = idx) -> true
    | _ -> false
  in
  let last_ai_to_buy_player_stock =
    if cycle land 0xC0 = 0 then None else v.last_ai_to_buy_player_stock
  in
  let player_has_more_cash = M.(player_cash > ai_player.cash) in
  let player_in_ai_shares = Stock_market.owned_shares player_idx ~owned:ai_idx stocks in
  let ai_treasury_shares = Stock_market.treasury_shares ai_idx stocks in
  let ai_controls_itself = Stock_market.controls_own_company ai_idx stocks in
  let ai_doing_badly =
    (* NOTE: this seems like a bug. This bonus is too small compared to number
             of shares *)
    let bonus = if player_has_more_cash then 1 else 0 in
    not (ai_controls_itself || player_in_ai_shares + bonus <= ai_treasury_shares)
  in
  let player_valuation =
    (* NOTE: what about stock ownership? *)
    M.(player_cash + Stock_market.treasury_share_value player_idx stocks)
  in
  let player_share_price = Stock_market.share_price player_idx stocks in
  let player_total_shares = Stock_market.total_shares player_idx stocks / 10 in
  let ai_in_player_shares ai_idx stocks =
    Stock_market.owned_shares ai_idx ~owned:player_idx stocks
  in
  let ai_takeover_loans_plus_shares, player_loans_to_own_self_plus_shares,
      shares_to_control_player, shares_for_player_to_control_self =
    let clip_100 = Utils.clip ~min:0 ~max:99 in
    let ai_in_player_shares = ai_in_player_shares ai_idx stocks / 10 in
    let shares_to_control_player = player_total_shares / 2 - ai_in_player_shares + 1
        |> clip_100
    in
    let cash_to_control_player = M.(player_share_price * shares_to_control_player * 10) in
    let missing_cash = M.(cash_to_control_player - ai_player.cash) in
    let num_loans_needed = M.(missing_cash /~ C.bond_value) + 1 |> clip_100 in
    let ai_loans_plus_shares = num_loans_needed + shares_to_control_player in

    let player_treasury_shares = Stock_market.treasury_shares player_idx stocks in
    let shares_for_player_to_control_self =
      player_total_shares / 2 - player_treasury_shares |> clip_100
    in
    let player_cash_to_own_self = M.(player_share_price * shares_for_player_to_control_self * 10) in
    let player_num_loans =
      M.((player_cash_to_own_self - player_cash) /~ C.bond_value) + 1 |> clip_100
    in
    let player_loans_plus_shares = player_num_loans + shares_for_player_to_control_self in
    ai_loans_plus_shares, player_loans_plus_shares, shares_to_control_player, shares_for_player_to_control_self
  in
  let ai_try_takeover =
    match last_ai_to_buy_player_stock with
    | None when M.(player_valuation > M.of_int 250) -> 
         player_loans_to_own_self_plus_shares > ai_takeover_loans_plus_shares
    | _-> false
  in
  let ai_num_bonds = Region.num_bonds params.region (M.to_int ai_player.bonds) in
  let bond_interest =
    ai_num_bonds - ai_player.opponent.financial_skill - (Climate.to_enum params.climate) + 8 |> M.of_int
  in
  let avoid_bonds =
    if M.(ai_player.bonds = zero)
       || player_in_ai_shares >= ai_treasury_shares then false else
      let num_loans_approx = M.(ai_player.bonds /~ C.bond_value) in
      let bond_sum_val =
        Iter.fold (fun acc idx ->
          let div = if Region.is_west_us params.region then 2 else 1 in
          let value = idx / div - ai_player.opponent.financial_skill - (Climate.to_enum params.climate) in
          acc + (value * 5 + 40))
        0
        Iter.(0 -- num_loans_approx)
        |> M.of_int
      in
      M.(ai_player.yearly_interest > bond_sum_val)
  in
  let ai_share_price = Stock_market.share_price ai_idx stocks in
  let ai_can_afford_own_share = M.(ai_player.cash > ai_share_price * 10) in

  let take_out_bond =
    let enough_cash_vs_bond = M.((of_int 6 - bond_interest) * 100 > ai_player.cash) in
    let reject_bond2 = avoid_bonds || M.(bond_interest >= M.of_int 10) in
    if enough_cash_vs_bond && reject_bond2 then false else
    let ai_can_afford_player_share = M.(player_share_price * 10 < ai_player.cash) in
    if not ai_can_afford_own_share && ai_doing_badly && reject_bond2 then false else
    let reject_bond4 = ai_try_takeover || ai_can_afford_player_share || reject_bond2 in
    let bond_level_tolerated = ai_player.opponent.financial_skill >= M.(ai_player.bonds /~ C.bond_value) in
    if (ai_takeover_loans_plus_shares <> shares_for_player_to_control_self
      || ai_takeover_loans_plus_shares <= shares_to_control_player
      || not bond_level_tolerated) && reject_bond4 then false else
    if reject_bond2 then false
    else true
  in
  if take_out_bond then `TakeOutBond(bond_interest) else
  let ai_total_shares = Stock_market.total_shares ai_idx stocks in
  let earnings_per_share = M.((ai_player.revenue_ytd * 100) / ai_total_shares) in
  (* Why add 10 here? *)
  let buy_own_shares =
    (ai_doing_badly || M.(earnings_per_share / 20 > ai_share_price) || M.(ai_player.cash > of_int 20000))
    && ai_can_afford_own_share
    && (player_in_ai_shares + ai_treasury_shares < ai_total_shares)
    && ai_treasury_shares + 10 < ai_total_shares
  in
  if buy_own_shares then `BuyOwnShares else
  let ai_be_active =
    let divide = if company_is_last_active then 2 else 3 in
    (* TODO: why 10? *)
    let ai_share_advantage = ai_treasury_shares - player_in_ai_shares - 10 in
    let ai_share_adv_value = M.(ai_share_price * ai_share_advantage * (B_options.difficulty_enum params.options)) in
    let ai_value = M.(ai_share_adv_value / divide + ai_player.cash - ai_player.bonds) in
    (* TODO: is this a bug? What is this amount? Should it be ai_in_player_shares? *)
    let player_value = M.mult player_share_price (player_total_shares / 2 - ai_treasury_shares + 10) in
    M.(ai_value > player_value)
  in
  let buy_player_shares =
    let ai_can_afford_player_share = M.(player_share_price * 10 <= ai_player.cash) in
    let player_controls_self = Stock_market.controls_own_company player_idx stocks in
    let ai_controls_player = Stock_market.controls_company ai_idx ~target:player_idx stocks in
    let first_year = params.year = params.year_start in
    let other_ai_in_player_shares =
      Stock_market.other_companies_in_player_shares player_idx ~exclude_owner:ai_idx stocks
    in
    (* Only cutthroat will buy player shares *)
    (ai_try_takeover || ai_be_active) && ai_can_afford_player_share
    && B_options.cutthroat params.options && (not player_controls_self)
    && (not ai_controls_player) && Option.is_none last_ai_to_buy_player_stock
    && (not first_year) && other_ai_in_player_shares = 0 && ai_player.track_length > 32
  in
  if buy_player_shares then `BuyPlayerShares else
  let decent_money_situation = M.(ai_player.cash + (of_int @@ Climate.to_enum params.climate) * 500 > of_int 3000) in
  if avoid_bonds || decent_money_situation then `PayBackBond else
  let ai_sell_own_stock =
    let ai_more_self_shares = player_in_ai_shares + 20 < ai_treasury_shares in
    let ai_has_bond = M.(ai_player.bonds > of_int 0) in
    let worth_selling = M.(earnings_per_share / 16 < ai_share_price) in
    let odd_year = params.year land 1 <> 0 in
    if ai_more_self_shares && ai_has_bond && v.financial_ctr land 0x3C = 0 && odd_year && worth_selling then true else
    if company_is_last_active && ai_be_active && ai_treasury_shares > 0 then true
    else false
  in
  if ai_sell_own_stock then `SellOwnShares else
  if ai_in_player_shares ai_idx stocks > 0 && company_is_last_active
    && M.(ai_player.bonds > zero || Option.is_some last_ai_to_buy_player_stock || ai_player.cash < of_int 100)
    then `SellPlayerShares
    else `Nothing

let financial_text ~cities ~region ui_msg v =
  let name ai_idx = get_name ai_idx ~cities v in
  match ui_msg with
  | Ui_msg.AiBuySellOwnStock{ai_idx; price; buy; _} ->
      Printf.sprintf
        "%s\n\
        %s %d,000 shares of\n\
        treasury stock.\n\
        Price %s to %s.00/share.\n"
        (name ai_idx)
        (if buy then "adds" else "sells")
        C.num_buy_shares
        (if buy then "rises" else "falls")
        (Money.print ~region ~ks:false price) 
  | AiTakesOutBond{ai_idx; _} ->
      Printf.sprintf
        "%s\n\
        takes out %s loan.\n" (* NOTE: always 500, even in west us *)
        (name ai_idx)
        (Money.print ~region C.bond_value)
  | AiSellsPlayerStock {ai_idx; _} ->
      Printf.sprintf
        "%s\n\
        sells %d,000 shares of\n\
        your stock.\n"
        (name ai_idx)
        C.num_buy_shares
  | AiBuysPlayerStock {ai_idx; takeover; _} ->
      Printf.sprintf
        "%s\n\
        buys %d,000 shares of\n\
        your stock!\n\
        %s"
        (name ai_idx)
        C.num_buy_shares
        (if takeover then "Your RR has been\nTAKEN OVER!\n" else "")
  | _ -> ""

let ai_financial_routines ~ai_idx ~stocks ~cycle ~player_cash ~(params:Params.t) v =
  let player_idx = C.player in
  match _ai_financial_decision  ~ai_idx ~stocks ~cycle ~player_cash ~params v with
  | `BuyOwnShares ->
    let cost, stocks = Stock_market.ai_buy_own_stock ~ai_idx stocks in
    let price = Stock_market.share_price ai_idx stocks in
    let v = modify_ai ai_idx v (fun ai_player ->
      let cash = M.(ai_player.cash - cost) in
      {ai_player with cash})
    in
    let opponent = (get_ai_exn ai_idx v).opponent.name in
    v, stocks, player_cash, [Ui_msg.AiBuySellOwnStock{opponent; ai_idx; price; buy=true}]

  | `SellOwnShares ->
    let profit, stocks = Stock_market.ai_sell_own_stock ~ai_idx stocks in
    let price = Stock_market.share_price ai_idx stocks in
    let v = modify_ai ai_idx v (fun ai_player ->
      let cash = M.(ai_player.cash + profit) in
      {ai_player with cash})
    in
    let opponent = (get_ai_exn ai_idx v).opponent.name in
    v, stocks, player_cash, [Ui_msg.AiBuySellOwnStock{opponent; ai_idx; price; buy=false}]

  | `BuyPlayerShares ->
    let cost, stocks = Stock_market.ai_buy_player_stock ~ai_idx ~player_idx stocks in
    let takeover = Stock_market.controls_company ai_idx ~target:C.player stocks in
    let stocks = if takeover then Stock_market.hostile_takeover ~ai_idx ~player_idx stocks else stocks in
    let v = modify_ai ai_idx v (fun ai_player ->
      let cash = M.(ai_player.cash - cost) in
      let cash = if takeover then M.(cash + player_cash) else cash in
      {ai_player with cash})
    in
    let v = {v with last_ai_to_buy_player_stock=Some ai_idx} in
    let player_cash = if takeover then M.zero else player_cash in
    let opponent = (get_ai_exn ai_idx v).opponent.name in
    v, stocks, player_cash, [Ui_msg.AiBuysPlayerStock{opponent; player_idx; ai_idx; takeover}]

  | `SellPlayerShares ->
    let profit, stocks = Stock_market.ai_sell_player_stock ~ai_idx ~player_idx stocks in
    let v = modify_ai ai_idx v (fun ai_player ->
      let cash = M.(ai_player.cash + profit) in
      {ai_player with cash})
    in
    let opponent = (get_ai_exn ai_idx v).opponent.name in
    v, stocks, player_cash, [Ui_msg.AiSellsPlayerStock{opponent; player_idx; ai_idx}]

  | `PayBackBond ->
      let v = modify_ai ai_idx v (fun ai_player ->
        let open M in
        let cash = ai_player.cash - C.bond_value in
        let num_loans = ai_player.bonds /~ C.bond_value in
        let interest_delta = ai_player.yearly_interest / num_loans in
        let yearly_interest = ai_player.yearly_interest - interest_delta in
        let bonds = ai_player.bonds - C.bond_value in
        let cash = cash - of_int 5 in (* Why? *)
        {ai_player with cash; bonds; yearly_interest}
      )
      in
      v, stocks, player_cash, []

  | `TakeOutBond(bond_interest) ->
    let v = modify_ai ai_idx v (fun ai_player ->
      let open M in
      let cash = ai_player.cash + C.bond_value - of_int 5 in
      let yearly_interest = ai_player.yearly_interest + bond_interest * 5 in
      let bonds = ai_player.bonds + C.bond_value in
      {ai_player with cash; yearly_interest; bonds}
    )
    in
    let ai_owns_some_player_stock = Stock_market.owned_shares ai_idx ~owned:player_idx stocks > 0 in
    let ui_msg = if ai_owns_some_player_stock then
      let opponent = (get_ai_exn ai_idx v).opponent.name in
      [Ui_msg.AiTakesOutBond{opponent; player_idx; ai_idx}] else []
    in
    v, stocks, player_cash, ui_msg

  | `Nothing -> v, stocks, player_cash, []

let ai_track_routines ?force_create ~stocks ~params ~player_net_worth ~tilemap ~tracks ~cities random ~stations v =
  match ai_track_routines ?force_create ~stocks ~params ~player_net_worth ~tilemap ~tracks ~cities random ~stations v with
  | `Build(tracks, tilemap, v, stations, msgs) ->
      tracks, tilemap, stations, stocks, v, msgs
  | `CreateAI(tilemap, v, stocks, ui_msg) ->
      tracks, tilemap, stations, stocks, v,  [ui_msg]
  | `Update v ->
      tracks, tilemap, stations, stocks, v, []

let end_of_year_maintenance_interest v =
  let ais =
    Owner.Map.map (fun ai_player ->
      let track_maintenance = ai_player.track_length / 8 |> Money.of_int in
      let open Money in
      let cash = ai_player.cash - track_maintenance - ai_player.yearly_interest in
      let cash = if cash < zero then cash + cash / 8 else cash in
      {ai_player with cash}
    ) v.ais
  in
  {v with ais}

let fiscal_period_end_stock_eval stocks v =
  let ais = Owner.Map.map (_update_net_worth stocks) v.ais in
  let stocks, ui_msgs = Owner.Map.fold (fun player_idx ai_player (stocks, ui_msgs) ->
    let old_share_price = Stock_market.share_price player_idx stocks in
    let new_share_price = new_share_price_ player_idx stocks ai_player in
    let split = M.(new_share_price >= of_int 100) in
    let stocks = Stock_market.set_share_price player_idx new_share_price stocks in
    let stocks = if split then Stock_market.split_stock player_idx stocks else stocks in
    let stocks = Stock_market.update_history_with_stock_value player_idx stocks in
    let fired =
      let new_price_ok = M.(new_share_price > of_int 5) in
      let small_track = ai_player.track_length < 2 in
      let low_price = M.(old_share_price < of_int 4) in
      if new_price_ok then small_track else low_price in
    let fired = if fired then `Fired else `Normal in
    let msg = Ui_msg.SharePriceChange{
      player_idx;
      from_=old_share_price;
      to_=new_share_price;
      share_price_growth=0;
      split;
      fired;
      } in
    (stocks, msg::ui_msgs))
    v.ais
    (stocks, [])
  in
  {v with ais}, stocks, ui_msgs

let update_track_history v =
  (* We do this every year rather than every fiscal period *)
  let length = Vector.length v.routes in
  {v with route_history=length::v.route_history}

let calc_profit player_idx v =
  let ai_player = get_ai_exn player_idx v in
  let maintenance = get_track_length player_idx v / 4 in
  M.(ai_player.revenue_ytd - of_int maintenance - ai_player.yearly_interest/2)

let get_route_history v = v.route_history

let get_revenue_ytd v = v.revenue_ytd

let get_yearly_interest v = v.yearly_interest

let get_build_order ai_idx v =
  (get_ai_exn ai_idx v).build_order

let set_build_order ai_idx src dst v =
  modify_ai ai_idx v (fun ai_player->
    {ai_player with build_order=Some(src, dst)})

let get_city_connections city v =
  Vector.fold (fun acc route ->
  if U.equal_loc route.src city then route.dst :: acc
  else if U.equal_loc route.dst city then route.src :: acc
  else acc)
  []
  v.routes

(* Recursively delete city, routes + another max 2 cities if they're isolated *)
let _delete_city_rate_war city ai_idx tilemap v =
  let connected_cities = get_city_connections city v in
  let cities_to_delete =
    List.filter (fun city ->
      List.is_empty @@ get_city_connections city v)
    connected_cities
  in
  let v = List.fold_left (fun acc city ->
    remove_ai_of_city city acc) v cities_to_delete
  in
  (* Delete all routes with these cities *)
  let has_city city = List.mem ~eq:U.equal_loc city cities_to_delete in
  Vector.filter_in_place (fun route ->
    not @@ has_city route.src || has_city route.dst) v.routes;
  (* Delete from map *)
  List.iter (fun city ->
    match Tilemap.find_enemy_station_near city ~player_idx:ai_idx tilemap with
    | Some loc ->
      let tile = Tilemap.get_tile loc tilemap in
      begin match tile with
      | EnemyStation{over; owner} when Owner.(owner = ai_idx) -> Tilemap.set_tile city over tilemap
      | _ -> ()
      end
    | None -> failwith "No city station found")
    cities_to_delete;
  v
  
let rate_war_ai_loss city ai_idx tilemap v =
  if not @@ city_has_rate_war city v then v else
  v
  |> end_city_rate_war city
  |> _delete_city_rate_war city ai_idx tilemap

let dissolve_ai ai_idx v =
  let ai_cities = LocMap.to_iter v.ai_of_city
    |> Iter.filter (fun (_loc, owner) -> Owner.(owner = ai_idx))
    |> Iter.map fst
    |> LocSet.of_iter
  in
  let ai_of_city = LocMap.filter (fun _ ai -> Owner.(ai <> ai_idx)) v.ai_of_city in
  let routes = Vector.filter_in_place (fun route -> Owner.(route.owner <> ai_idx)) v.routes; v.routes in
  (* TODO: how to fix route_history? Fix all of history mechanic *)
  let rate_war_at_city = LocSet.diff v.rate_war_at_city ai_cities in
  let ais = Owner.Map.remove ai_idx v.ais in
  let last_ai_to_buy_player_stock = match v.last_ai_to_buy_player_stock with
    | Some ai when Owner.(ai = ai_idx) -> None
    | x -> x
  in
  {v with routes; ai_of_city; rate_war_at_city; ais; last_ai_to_buy_player_stock}
  

