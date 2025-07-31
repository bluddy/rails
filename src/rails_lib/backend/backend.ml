open Containers
open! Utils.Infix
open Backend_d

let src = Logs.Src.create "backend" ~doc:"Backend"
module Log = (val Logs.src_log src: Logs.LOG)

module G = Track_graph
module C = Constants
module UIM = Ui_msg
module Bool = Utils.Bool
module IntMap = Utils.IntMap
module M = Money
module Random = Utils.Random

(* This is the backend. All game-modifying functions go through here *)

(* The actual game (server) state
   Observers can observe data in the backend,
   but actions can only be taken via messages (Backend.Action)
 *)

type t = Backend_d.t [@@deriving yojson]

let default = {
  params = Params.make ();
  last_tick=0;
  pause=false;
  players=Owner.Map.empty;
  map=Tilemap.default;
  track=Trackmap.empty 0 0;
  graph=Track_graph.make ();
  cities=Cities.default;
  engines=[];
  stations=Station_map.empty;
  blocks=Block_map.make ();
  dev_state=Tile_develop.default;
  stocks=Stock_market.default;
  ai=Ai.default ();
  delayed_fn=None;
  ui_msgs=[];
  random=Random.get_state ();
  seed=0;
} 

let make region resources ~reality_levels ~difficulty ~random ~seed = 
  let map = List.assoc ~eq:(Stdlib.(=)) region resources.Resources.res_maps in
  let map = Tilemap.of_ndarray ~region ~seed map in
  let width, height = Tilemap.get_width map, Tilemap.get_height map in
  let cities =
    let h = Hashtbl.create 100 in
    List.assoc ~eq:(Stdlib.(=)) region resources.res_cities
    |> List.iter (fun (name, x, y) ->
        Hashtbl.replace h (y * width + x) (name, Random.int Station.num_suffix random)
      );
    Cities.make h width height
  in
  let track = Trackmap.empty width height in
  let stations = Station_map.empty in
  let players = Owner.Map.singleton C.player @@ Player.default C.player in
  let year_start = Region.start_year region in
  let graph = Track_graph.make () in
  let engines = Engine.of_region region |> Engine.randomize_year random in
  let params = Params.make ~year_start ~reality_levels ~difficulty ~region () in
  let stocks = Stock_market.default
    |> Stock_market.add_human_player C.player params in
  {
    params;
    last_tick=0;
    players;
    map;
    cities;
    track;
    blocks=Block_map.make ();
    graph;
    stations;
    engines;
    ui_msgs = [];
    random;
    seed;
    pause=false;
    delayed_fn=None;
    dev_state=Tile_develop.default;
    stocks;
    ai = Ai.default ();
  }

let get_speed v = v.params.options.speed

let get_difficulty v = B_options.difficulty v.params.options

let _set_speed v speed = {v with params={v.params with options={v.params.options with speed}}}

let map_height v = Tilemap.get_height v.map

let map_width v = Tilemap.get_width v.map

let get_tile x y v = Tilemap.get_tile_xy x y v.map

let get_track x y v = Trackmap.get_xy x y v.track

let get_cities v = Cities.to_list v.cities

let get_station loc v = Station_map.get loc v.stations

let get_region v = v.params.region

let get_year v = v.params.year

let get_time v = v.params.time

let get_params v = v.params

let get_cycle v = v.params.cycle

let get_climate v = v.params.climate

let get_period v = Params.current_period v.params

let get_map v = v.map

let get_options v = v.params.options

let get_tile_height x y v = Tilemap.get_tile_height_xy x y v.map

let get_trains player_idx v =
  Player.get player_idx v.players |> Player.get_trains

let get_train idx player_idx v =
  Trainmap.get idx @@ get_trains player_idx v

let iter_cities f v = Cities.iter f v.cities

let find_close_city x y ~range v = Cities.find_close_xy x y v.cities ~range

let send_ui_msg v msg =
  (* Mutation. Line up ui msg for when we can send it *)
  v.ui_msgs <- msg::v.ui_msgs

let get_player player_idx v = Player.get player_idx v.players

(* Functions that turn either to a player or to AI *)
let get_player_ai player_f ai_f player_idx v =
  if Owner.is_human player_idx then
    get_player player_idx v |> player_f
  else
    ai_f player_idx v.ai

let get_cash player_idx v = get_player_ai Player.get_cash Ai.get_cash player_idx v

let get_bonds player_idx v = get_player_ai Player.bonds Ai.get_bonds player_idx v

let get_net_worth player_idx v = get_player_ai Player.net_worth Ai.get_net_worth player_idx v

let get_name player_idx v =
  get_player_ai (Player.get_name v.stations v.cities) (Ai.get_name ~cities:v.cities) player_idx v

let get_handle player_idx v =
  get_player_ai (Player.get_handle v.stations v.cities) (Ai.get_name ~cities:v.cities) player_idx v

let get_track_length player_idx v =
  (* Track length for ui and such *)
  (get_player_ai Player.track_length Ai.get_track_length player_idx v)
  * Region.dist_mult v.params.region

let players_and_ai v =
  Iter.append (Owner.Map.keys v.players) (Ai.ai_iter v.ai)

let is_illegal = function `Illegal -> true | _ -> false

let check_build_station ?(union_station=false) ?(rate_war=false) x y player_idx station_type v =
  (* rate_war is only used by the internal system *)
  let loc = (x, y) in
  match Trackmap.check_build_station loc player_idx station_type v.track with
  | `Ok -> Tilemap.check_build_station ~rate_war ~union_station loc v.map
  | x -> x

let _build_station ?(union_station=false) ?(rate_war=false) ((x,y) as loc) station_type player_idx v =
  let is_ok = match check_build_station ~rate_war x y player_idx station_type v with `Ok -> true | _ -> false in
  if not is_ok then v else
  let before = Scan.scan v.track loc player_idx in
  let track, build_new_track_dir = Trackmap.build_station v.track loc station_type in
  let after = Scan.scan track loc player_idx in
  let graph = G.Track.handle_build_station x y v.graph before after in
  let trains = get_trains player_idx v in
  let blocks = Block_map.handle_build_station player_idx graph v.blocks track trains loc after in
  let station = match station_type with
  | `SignalTower ->
    Station.make_signaltower x y ~year:v.params.year player_idx
  | _ ->
    let city_xy = find_close_city ~range:100 x y v |> Option.get_exn_or "error" in
    let first = not @@ Station_map.have_engine_shop v.stations in
    (* Get suffix if needed *)
    let city_name, suffix =
      let (x,y) = city_xy in
      let name, offset = Cities.find_exn x y v.cities in
      let count =
        Station_map.fold (fun station count ->
          match Station.get_city station with
          | Some (city_x, city_y) ->
            if city_x = x && city_y = y then count + 1
            else count
          | _ -> count)
        v.stations
        ~init:0
      in
      if count = 0 then name, None
      else
        let suffix_n = (offset + count) mod Station.num_suffix in
        name, Station.suffix_of_enum suffix_n
    in
    Station.make x y ~year:v.params.year ~city_xy ~suffix ~city_name ~kind:station_type player_idx ~first
  in
  let loc = (x, y) in
  let stations = Station_map.add loc station v.stations in
  (* Initialize supply and demand *)
  ignore @@ Station.update_supply_demand v.map v.params station;
  let players = Player.update v.players player_idx (fun player ->
    let player = player
      |> Player.add_station loc
      |> Player.pay `StructuresEquipment (Station.price_of ~union_station station_type)
    in
    match build_new_track_dir with
    | Some dir -> Player.update_and_pay_for_track x y ~dir ~len:1 ~climate:v.params.climate v.map player
    | _ -> player)
  in
  [%up {v with players; stations; graph; track; blocks}]

let check_build_tunnel loc ~dir player_idx v =
  let check length = Trackmap.check_build_stretch loc ~dir player_idx ~length v.track in
  match Tilemap.check_build_tunnel loc ~dir v.map with
  | `Tunnel(length, _, _) when not(check length) -> `HitsTrack
  | x -> x

let _build_tunnel loc ~dir player_idx v =
  match check_build_tunnel loc ~dir player_idx v with
  | `Tunnel(length, _, cost) ->
    let before = Scan.scan v.track loc player_idx in
    let track = Trackmap.build_tunnel loc ~dir player_idx ~length v.track in
    let after = Scan.scan track loc player_idx in
    let graph = G.Track.handle_build_track_simple v.graph before after in
    let blocks =
      let trains = get_trains player_idx v in
      Block_map.handle_build_track player_idx graph track trains v.blocks before after
    in
    let players = Player.update v.players player_idx @@ Player.pay `BridgeTunnel cost in
    [%up {v with graph; track; blocks; players}]
  | _ -> v

let check_build_track loc ~dir player_idx v =
  (* First check the tilemap, then the trackmap *)
  match Tilemap.check_build_track loc ~dir v.params v.map with
  | `Bridge when Trackmap.check_build_stretch loc ~dir player_idx ~length:2 v.track -> `Bridge
  | (`Ok | `RateWar _ | `Ferry | `Tunnel _ | `HighGrade _) as x when Trackmap.check_build_track loc ~dir player_idx v.track -> x
  | `Ok | `RateWar _ | `Bridge | `Ferry | `Tunnel _ | `HighGrade _ | `Illegal -> `Illegal

let check_build_bridge loc ~dir player_idx v =
  match check_build_track loc ~dir player_idx v with
  | `Bridge -> true
  | _ -> false

let _build_bridge ((x, y) as loc) ~dir player_idx ~kind v =
  if not @@ check_build_bridge loc ~dir player_idx v then v else
  let before = Scan.scan v.track loc player_idx in
  let track = Trackmap.build_bridge loc ~dir player_idx ~kind v.track in
  let after = Scan.scan track loc player_idx in
  let graph = G.Track.handle_build_track_simple v.graph before after in
  let blocks =
    let trains = get_trains player_idx v in
    (* TODO: this was v.track. Check if it's now ok *)
    Block_map.handle_build_track player_idx graph track trains v.blocks before after
  in
  let players = Player.update v.players player_idx (fun player ->
    player
    |> Player.update_and_pay_for_track x y ~dir ~len:2 ~climate:v.params.climate v.map
    |> Player.pay `BridgeTunnel (Bridge.price_of kind))
  in
  send_ui_msg v @@ BridgeCreated{player_idx; kind};
  [%up {v with graph; track; blocks; players}]

let check_change_double_track loc player_idx ~double v =
  match Trackmap.get loc v.track with
  | Some track when Owner.(track.player = player_idx) && Track.is_doubleable track ->
      not @@ Bool.(double = Track.is_visually_double track)
  | _ -> false

let _change_double_track loc player_idx ~double v =
  if check_change_double_track loc player_idx ~double v then (
    let track =
      let t = Trackmap.get_exn loc v.track |> Track.change_to_double ~double in
      Trackmap.set loc t v.track
    in
    let after = Scan.scan track loc player_idx in
    let blocks =
      let trains = get_trains player_idx v in
      Block_map.handle_double_change player_idx v.graph track trains v.blocks after
    in
    [%up {v with track; blocks}]
  ) else v

let _handle_rate_war_union_station player_idx loc v =
  (* Rate war declared by player. Could be a union station *)
  let city = Cities.find_close loc ~range:100 v.cities |> Option.get_exn_or "no city found" in
  match Ai.ai_of_city city v.ai with
  | None -> v
  | Some ai_idx ->
    match (if Stock_market.controls_company player_idx ~target:ai_idx v.stocks then `UnionStation else `RateWar) with
    | `RateWar ->
      let v = _build_station loc `Station player_idx v in
      let stations = Station_map.update loc Station.set_rate_war v.stations in
      let ai = Ai.set_city_rate_war city v.ai in
      let msg = Ui_msg.RateWarDeclared{player_idx; other_player_idx=ai_idx; station=loc} in
      send_ui_msg v msg;
      {v with stations; ai}

    | `UnionStation ->
      let v = _build_station loc ~union_station:true `Terminal player_idx v in
      let msg = Ui_msg.UnionStation {player_idx; station=loc} in
      send_ui_msg v msg;
      v

let _build_track ((x, y) as loc) ~dir player_idx v =
  (* Can either create a new edge or a new node (ixn) *)
  let ret = check_build_track loc ~dir player_idx v in
  if is_illegal ret then v else
  let before = Scan.scan v.track loc player_idx in
  let track = Trackmap.build_track loc ~dir player_idx v.track in
  let after = Scan.scan track loc player_idx in
  let graph = G.Track.handle_build_track x y v.graph before after in
  let blocks =
    let trains = get_trains player_idx v in
    Block_map.handle_build_track player_idx graph track trains v.blocks before after
  in
  let players = Player.update v.players player_idx @@
    Player.update_and_pay_for_track x y ~dir ~len:1 ~climate:v.params.climate v.map
  in
  let v = [%up {v with graph; track; blocks; players}] in
  let v = match ret with
    | `RateWar loc -> _handle_rate_war_union_station player_idx loc v
    | _ -> v in
  v

let is_ferry = function `Ferry -> true | _ -> false

let _build_ferry ((x, y) as loc) ~dir player_idx v =
  let ret = check_build_track loc ~dir player_idx v in
  if not @@ is_ferry ret then v else
  let track, map = v.track, v.map in
  let before = Scan.scan track loc player_idx in
  let tile1 = Tilemap.get_tile loc map in
  let loc2 = Dir.adjust dir x y in
  let tile2 = Tilemap.get_tile loc2 map in
  let kind1, kind2 = match tile1, tile2 with
    | Tile.Ocean _ , Ocean _ -> Track.Ferry `Single, Track.Ferry `Single
    | Ocean _, _ -> Ferry `Single, Track `Single
    | _, Ocean _ -> Track `Single, Ferry `Single
    | _ -> assert false
  in
  let track = Trackmap.build_track loc ~dir player_idx ~kind1 ~kind2 v.track in
  let after = Scan.scan track loc player_idx in
  let graph = G.Track.handle_build_track_simple v.graph before after in
  (* TODO: check if this needs v.track *)
  let blocks =
    let trains = get_trains player_idx v in
    Block_map.handle_build_track player_idx graph track trains v.blocks before after
  in
  let players = Player.update v.players player_idx @@
    Player.update_and_pay_for_track x y ~dir ~len:1 ~climate:v.params.climate v.map in
  [%up {v with graph; track; blocks; players}]

let check_remove_track loc ~dir player_idx v =
  Trackmap.check_remove_track loc ~dir player_idx v.track 

let _remove_station ((x, y) as loc) ?dir player_idx v =
  let track, graph, stations, blocks = v.track, v.graph, v.stations, v.blocks in
  let before = Scan.scan track loc player_idx in
  (* Have to be careful with order here or we'll mess up state *)
  let blocks = Block_map.handle_remove_station graph track blocks loc before in
  let track = match dir with | Some dir -> Trackmap.remove_track loc player_idx ~dir track | None -> track in
  let after = Scan.scan track loc player_idx in
  let graph = G.Track.handle_remove_track x y graph before after in
  let stations = Station_map.delete loc stations in
  (* TODO: Not sure this is right. Check this in build_station *)
  let players =
    Player.update v.players player_idx (fun player ->
      let player = Player.remove_station loc player in
      match dir with
      | Some dir -> Player.update_and_remove_track x y ~len:1 ~dir ~climate:v.params.climate v.map player
      | None -> player)
  in
  [%up {v with stations; blocks; track; graph; players}]

let _remove_track ((x,y) as loc) ~dir player_idx v =
  let track, graph, blocks, players = v.track, v.graph, v.blocks, v.players in
  let is_station = Trackmap.has_station loc track in
  if is_station then _remove_station loc ~dir player_idx v else
  let before = Scan.scan v.track loc player_idx in
  (* Have to be careful with order here or we'll mess up state *)
  let track = Trackmap.remove_track loc ~dir player_idx track in
  let after = Scan.scan track loc player_idx in
  let graph = G.Track.handle_remove_track x y graph before after in
  let blocks =
    let trains = get_trains player_idx v in
    Block_map.handle_remove_track player_idx graph track trains blocks before after
  in
  let players = Player.update players player_idx @@
    Player.update_and_remove_track x y ~len:1 ~dir ~climate:v.params.climate v.map
  in
  [%up {v with track; graph; blocks; players}]

let _improve_station loc player_idx ~upgrade v =
  let stations = 
    Station_map.update loc (Station.add_upgrade upgrade player_idx) v.stations
  in
  let players = Player.update v.players player_idx @@
    Player.(pay `StructuresEquipment @@ Station.price_of_upgrade upgrade)
  in
  [%up {v with stations; players}]

let _build_train loc engine cars other_station player_idx v =
  let players =
    let engine_t = Engine.t_of_make v.engines engine in
    let train =
      (* TODO: Temporary solution for getting track dir *)
      let track = Trackmap.get loc v.track |> Option.get_exn_or "trackmap" in
      let dir, _ = Dir.Set.pop track.dirs in
      Train.make loc engine_t cars other_station ~dir player_idx
    in
    Player.update v.players player_idx (fun player ->
    let trains = Trainmap.add train player.trains in
    let player = Player.pay `Train engine_t.price player in
    [%up {player with trains}])
  in
  let msg =
    let trains = get_trains player_idx v in
    UIM.TrainBuilt (Trainmap.Id.of_int (Trainmap.size trains - 1))
  in
  send_ui_msg v msg;
  [%up {v with players}]

let _remove_stop_car train ~stop ~car player_idx v =
  let players = Player.update v.players player_idx (fun player ->
    let trains = Trainmap.update train player.trains (Train.remove_stop_car stop car) in
    [%up {player with trains}])
  in
  [%up {v with players}]

let check_stop_station ~train ~stop ~station player_idx v =
  let train =
    let player = Player.get player_idx v.players in
    Trainmap.get train player.trains 
  in
  Train.check_stop_station train stop station

let modify_train train player_idx v f =
  let players = Player.update v.players player_idx (fun player ->
    let trains = Trainmap.update train player.trains f in
    [%up {player with trains}])
  in
  [%up {v with players}]

let _set_stop_station ~train ~stop ~station player_idx v =
  modify_train train player_idx v (Train.set_stop_station stop station)

let _remove_stop ~train ~stop player_idx v =
  modify_train train player_idx v (Train.remove_stop stop)

let _add_stop_car ~train ~stop ~car player_idx v =
  modify_train train player_idx v (Train.add_stop_car stop car)

let _remove_all_stop_cars ~train ~stop player_idx v =
  modify_train train player_idx v (Train.remove_all_stop_cars stop)

let get_num_trains player_idx v =
  let player = Player.get player_idx v.players in
  Trainmap.size player.trains
  
let trackmap_iter v f = Trackmap.iter v.track f

let _train_set_type ~train ~typ player_idx v =
  modify_train train player_idx v (Train.set_type typ)

let _train_replace_engine ~train ~engine player_idx v =
  let players = Player.update v.players player_idx (fun player ->
    let engine = Engine.t_of_make v.engines engine in
    let player = Player.pay `Train engine.price player in
    let trains = Trainmap.update train player.trains (Train.replace_engine engine) in
    [%up {player with trains}])
  in
  [%up {v with players}]

let _train_toggle_stop_wait ~train ~stop player_idx v =
  modify_train train player_idx v (Train.toggle_stop_wait stop)

let _train_toggle_hold player_idx train_idx v =
  modify_train train_idx player_idx v Train.toggle_hold_at_next_station

let _station_set_signal loc dir cmd v =
  (* the user can only set the override *)
  let signal = match cmd with
  | `Normal -> Station.NoOverride
  | `Hold -> OverrideHold
  | `Proceed -> OverrideProceed
  in
  let stations = Station_map.update loc (Station.set_override dir signal) v.stations in
  [%up {v with stations}]

let _build_industry ((x, y) as loc) tile player_idx v =
  if Tilemap.check_build_industry_at x y tile v.map ~region:v.params.region then (
    Tilemap.set_tile loc tile v.map;
    let cost = Tile.Info.build_cost_of_tile v.params.region tile in
    let v = update_player v player_idx (Player.build_industry cost) in
    send_ui_msg v @@ IndustryBuilt {player_idx; tile};
    v
  ) else v

let _remove_train train_idx player_idx v =
  (* Same function called by Backend *)
  let players = Player.update v.players player_idx @@ fun player ->
    let trains = Train_station.remove_train train_idx v.blocks player.trains in
    [%up {player with trains}]
  in
  [%up {v with players}]

let reset_tick v =
  v.last_tick <- 0

(* Each time period is both 2 years and a 24 hour day *)

let _month_of_time time = (time / C.month_ticks) mod 12

let get_interest_rate v player_idx =
  get_player player_idx v |> Player.get_interest_rate v.params

let player_has_bond player_idx v =
  get_player player_idx v |> Player.has_bond

let _sell_bond player_idx v =
  let player = get_player player_idx v in
  if Player.check_sell_bond v.params player then (
    let interest_rate = Player.get_interest_rate v.params player in
    (* Must be before we get new bond *)
    let v = update_player v player_idx (Player.sell_bond v.params) in
    send_ui_msg v @@ StockBroker(BondSold{player_idx; interest_rate});
    v
  ) else v

  (* TODO: evaluate ai company value *)
let _repay_bond player_idx v =
  if Player.check_repay_bond (get_player player_idx v) then (
    let v = update_player v player_idx Player.repay_bond in
    send_ui_msg v @@ StockBroker(BondRepaid{player_idx});
    v
  ) else v

let can_buy_stock player_idx ~target v =
  let player = get_player player_idx v in
  let cash = Player.get_cash player in
  Stock_market.can_buy_stock player_idx ~target ~cash v.params v.stocks

let _buy_stock v player_idx ~stock = 
  let player = get_player player_idx v in
  let cash = Player.get_cash player in
  match Stock_market.buy_stock player_idx ~target:stock ~cash v.params v.stocks with
  | `Bought cost, stocks ->
      let v = update_player v player_idx (Player.add_cash @@ M.neg cost) in
      send_ui_msg v @@ StockBroker(StockBought {player_idx; stock; cost});
      {v with stocks}
  | `Takeover money_map, stocks ->
      let players = 
        Owner.Map.fold (fun player_idx change players ->
          Player.update players player_idx @@ Player.add_cash change)
        money_map
        v.players
      in
      send_ui_msg v @@ StockBroker(Takeover {player_idx; stock});
      [%up{v with stocks; players}]
  | `None, stocks ->
      [%up {v with stocks}]

let _sell_stock player_idx ~stock v =
  let cost, stocks = Stock_market.sell_stock player_idx ~target:stock v.stocks in
  let players = Player.update v.players player_idx @@ Player.add_cash cost in
  send_ui_msg v @@ StockBroker(StockSold {player_idx; stock; cost});
  [%up {v with stocks; players}]

let check_bankruptcy v player_idx = get_player v player_idx |> Player.check_bankruptcy

let _declare_bankruptcy player_idx v =
  let player = Player.get player_idx v.players in
  if Player.check_bankruptcy player then (
    let stocks, cash_map =
      let player_iter = Iter.append (Owner.Map.keys v.players) (Ai.ai_iter v.ai) in
      Stock_market.declare_bankruptcy player_idx player_iter v.stocks
    in
    let players, ai =
      Owner.Map.fold (fun idx profit (players, ais) ->
        if Owner.is_human idx then
          let players = Player.update v.players idx @@ Player.add_cash profit in
          players, ais
        else
          players, Ai.add_cash idx profit ais)
        cash_map
        (v.players, v.ai)
    in
    let players = Player.update players player_idx @@ Player.set_bankrupt v.params in
    send_ui_msg v @@ StockBroker(BankruptcyDeclared {player_idx});
    [%up {v with players; stocks; ai}]
  ) else v

let get_date (v:Backend_d.t) = _month_of_time v.params.time, v.params.year

let get_time_of_day time =
  (* Get time of day representation *)
  let minutes = 3 * time / 8 in
  let hours = minutes / 60 in
  let time_hours = ((hours + 11) / 12) + 1 in
  let time_mins = minutes mod 60 in
  let am_pm = if hours >= 12 then "PM" else "AM" in
  Printf.sprintf "%d:%02d %s" time_hours time_mins am_pm

let companies_controlled_by player_idx v =
  Stock_market.other_companies_controlled_by player_idx v.stocks

let _operate_rr_take_money player_idx ~company ~amount v =
  let players = v.players in
  let players = Player.update players player_idx @@ Player.add_cash amount in
  let players = Player.update players company @@ Player.add_cash @@ M.neg amount in
  (* TODO: fix this *)
  (* Player.update_ai_valuation v.players player_idx; *)
  send_ui_msg v @@ StockBroker(MoneyTransferredFrom{player_idx; company; amount});
  [%up {v with players}]

let _operate_rr_give_money player_idx ~company ~amount v =
  let players = v.players in
  let players = Player.update players player_idx @@ Player.add_cash @@ M.neg amount in
  let players = Player.update players company @@ Player.add_cash amount in
  (* TODO: fix this *)
  (* Player.update_ai_valuation v.players player_idx; *)
  send_ui_msg v @@ StockBroker(MoneyTransferredTo{company; player_idx; amount});
  [%up {v with players}]

let _name_train player_idx train_idx name v =
  update_player v player_idx 
    @@ Player.update_trains @@ fun trains ->
        Trainmap.update train_idx trains @@ Train.set_name name

let _name_rr player_idx name handle v =
  update_player v player_idx
    @@ fun player -> {player with name=Some(name, handle)}

let _operate_rr_repay_bond player_idx ~company_idx v =
  let v = update_player v company_idx Player.repay_bond in
  (* TODO: fix this *)
  (* Player.update_ai_valuation v.players player_idx; *)
  send_ui_msg v @@ StockBroker(AiBondRepaid {player_idx; company=company_idx});
  v

  (* TODO: RR build track *)
let _operate_rr_build_track player_idx ai_idx src dst v =
  if Stock_market.controls_company player_idx ~target:ai_idx v.stocks then
    let ai = Ai.set_build_order ai_idx src dst v.ai in
    {v with ai}
  else v

let broker_timer_active player_idx v =
  Player.get player_idx v.players |> Player.has_broker_timer 

let _start_broker_timer player_idx v =
  (* Only activate if no broker time active *)
  if not @@ broker_timer_active player_idx v then (
    update_player v player_idx (fun player -> fst @@ Player.incr_broker_timer player)
  ) else v

let create_balance_sheet player_idx v =
  Balance_sheet.create player_idx v.players v.stocks v.stations v.params v.track v.map

let _station_rate_war_score_result station v =
  let player_idx = Station.get_player_idx station in
  let loc = Station.get_loc station in
  let city =
    Cities.find_close loc ~range:10 v.cities |> Option.get_exn_or "Nearby city not found" in
  let ai_idx = Ai.ai_of_city city v.ai |> Option.get_exn_or "AI missing for city" in
  let picked_up = Station.get_picked_up_goods_exn station
    |> Hashtbl.to_iter |> Freight.Map.of_goods_iter ~merge:(+) in
  let ai_picked_up = Station.get_lost_supply_exn station
    |> Hashtbl.to_iter |> Freight.Map.of_goods_iter ~merge:(+) in
  let pickup_scores = Array.fold (fun acc freight ->
    let amt = Freight.Map.get_or ~default:0 freight picked_up in
    let ai_amt = Freight.Map.get_or ~default:0 freight ai_picked_up in
    let sum = amt + ai_amt in
    let category = if sum >= 320 then `Huge else if sum >= 160 then `Mid else `Little in
    let score, ai_score = match category with
    | `Little when amt >= ai_amt -> 1, 0
    | `Little -> 0, 1
    | `Mid when amt > ai_amt * 2 -> 2, 0
    | `Mid when amt * 2 < ai_amt -> 0, 2
    | `Mid -> 1, 1
    | `Huge when amt > ai_amt * 2 -> 3, 0
    | `Huge when amt * 2 < ai_amt -> 0, 3
    | `Huge when amt > ai_amt -> 2, 1
    | `Huge -> 1, 2
    in
    let ai_score = if Ai.get_track_length ai_idx v.ai < 8 then 0 else ai_score in
    Freight.Map.add freight (score, ai_score) acc)
    Freight.Map.empty
    Freight.all_freight
  in
  let delivered = Station.get_goods_revenue station |> Goods.Map.to_iter |> Goods.Set.of_iter_with_test ~test:(fun x -> M.(x > zero)) in
  let ai_delivered =
    (* For ai, collect supply from all their cities *)
    let ai_supply = Loc_map.fold_loc (fun loc owner acc ->
       if Owner.(owner = ai_idx) then
         let supply = Tilemap.collect_demand_supply loc ~range:3 v.map
          |> snd |> Hashtbl.to_iter |> Goods.Set.of_iter_with_mult in
         Goods.Set.union acc supply
       else acc)
      ~init:Goods.Set.empty
      (Ai.ai_of_city_map v.ai)
    in
    let station_demand = Station.get_demand_exn station in
    Goods.Set.inter station_demand ai_supply
  in
  let delivery_scores = List.fold_left (fun acc good ->
    let have = Goods.Set.mem good delivered in
    let ai_have = Goods.Set.mem good ai_delivered in
    match have, ai_have with
    | true, true -> Goods.Map.add good (1, 1) acc
    | true, false -> Goods.Map.add good (2, 0) acc
    | false, true -> Goods.Map.add good (0, 2) acc
    | _ -> acc)
    Goods.Map.empty
    Goods.order
  in
  let delivery_scores = if B_options.complex_economy v.params.options then delivery_scores else Goods.Map.empty in
  let final_scores =
    let final_score fn =
      Freight.Map.sum (fun _ pair -> fn pair) pickup_scores + Goods.Map.sum (fun _ pair -> fn pair) delivery_scores
    in
    final_score fst, final_score snd
  in
  let winner =
    let score, ai_score = final_scores in
    if score >= ai_score * 2 then `Player
    else if ai_score >= score * 2 then `Ai
    else `None
  in
  Ui_msg.{
     ai_idx;
     player_idx;
     city;
     station=loc;
     picked_up;
     ai_picked_up;
     pickup_scores;
     delivered;
     ai_delivered;
     delivery_scores;
     final_scores;
     winner;
  }

let _rate_war_info player_idx v =
  let rate_wars =
    Station_map.fold (fun station acc ->
      if Owner.(Station.get_player_idx station = player_idx) &&
          Station.has_rate_war station then station::acc
      else acc)
      v.stations
      ~init:[]
  in
  List.map (fun station -> _station_rate_war_score_result station v) rate_wars

let _rate_war_handle_result result v =
  let module C = C.RateWar in
  let player_idx = result.Ui_msg.player_idx in
  match result.Ui_msg.winner with
    | `Player ->
        (* Double rates for the rest of the fiscal period. Ai loses route(s) *)
        let loc = result.station in
        let stations = Station_map.update loc Station.set_double_rates v.stations in
        let ai = Ai.rate_war_ai_loss result.city result.ai_idx v.map v.ai in
        {v with stations; ai}, true
    | `Ai ->
        (* Player loses station, trains, track within 3 squares *)
        (* We have to use v because these operations affect many parts of v *)
        let x, y = result.station in
        let v = _remove_station result.station player_idx v in
        let v =
          update_player v player_idx (
            Player.update_trains (fun trains ->
              let trains_to_remove = Trainmap.find_trains_in_range ~x ~y ~range:C.loss_radius trains in
              List.fold_left (fun acc train_id -> Trainmap.delete train_id acc)
                trains
                trains_to_remove))
        in
        let v =
          let tracks_to_remove = Trackmap.find_track_in_range ~x ~y ~range:C.loss_radius player_idx v.track in
          List.fold_left (fun acc (x,y,dir) -> _remove_track (x,y) ~dir player_idx acc) v tracks_to_remove
        in
        v, false
    | `None -> v, false

  (* Delayed function to modify the backend with things that can't be done
     while the UI is running, specifically dissolving companies
    *)
let _fin_end_remove_players_delay players _ v =
  List.fold_left (fun v player_idx ->
    if Owner.is_ai player_idx then 
      let ai = Ai.dissolve_ai player_idx v.ai in
      {v with ai}
    else
      v)
  v
  players

  (* Find end 1st stage in backend_low: cyan screen, income statement, balance sheet
     then we get this message from the UI to continue to the next stage *)
let _fin_end_proceed player_idx v =
  let net_worth =
    let balance_sheet = create_balance_sheet player_idx v in
    Balance_sheet.compute_profit balance_sheet in
  let player = get_player player_idx v in
  let player, total_revenue, ui_msgs1 = Player.fiscal_period_end net_worth v.stations v.params player in
  (* TODO handle fired *)
  let player, stocks, ui_msgs2 = Player.fiscal_period_end_stock_eval ~total_revenue ~net_worth v.stocks v.params player in
  let player = Player.fiscal_period_end_achievements ~revenue:total_revenue ~net_worth v.params player in
  let rate_war_results = _rate_war_info player_idx v in
  let ai, stocks, ai_msgs = Ai.fiscal_period_end_stock_eval stocks v.ai in
  let job, player = Player.update_retirement_bonus_and_job ~fired:false stocks v.params player in
  let job_msg = match job with Some job -> [Ui_msg.JobOffer job] | None -> [] in
  let rw_msgs = List.map (fun info -> Ui_msg.RateWar info) rate_war_results in
  let ui_msg = Ui_msg.FiscalPeriodEndMsgs (player_idx, job_msg @ ui_msgs1 @ ui_msgs2 @ rw_msgs @ ai_msgs) in
  send_ui_msg v ui_msg;
  let stations = Station_map.map Station.end_of_period_reset v.stations in
  let cycle = if v.params.cycle > 20000 then 0 else v.params.cycle in
  (* TODO: check 100 years, "After 20*difficulty+40 years of faithful\nservcice you must retire\n
     from the presidency of the name\n" *)
  let params = {v.params with current_period=Params.next_period v.params; time=0; cycle} in
  let player = Player.clear_periodic params player in
  let dev_state = Tile_develop.end_of_period v.params v.dev_state in
  let v = update_player v player_idx (fun _ -> player) in
  let v = {v with params; ai; stocks; stations; dev_state} in
  (* Now that we dealt with end of the year stuff, optionally deal with rate war result *)
  let v, refresh_map =
    List.fold_left (fun (v, refresh_acc) result ->
      let v, refresh_map = _rate_war_handle_result result v in
      v, refresh_map || refresh_acc)
      (v, false)
      rate_war_results in
  if refresh_map then (send_ui_msg v Ui_msg.UpdateMap);
  (* Dissolving a company happens in a delayed function so the UI can get info until then *)
  let v =
    let players_to_dissolve =
      ai_msgs |> List.filter_map (function Ui_msg.SharePriceChange{player_idx; fired=`Fired;_} -> Some player_idx | _ -> None)
    in
    if List.is_empty players_to_dissolve then v
    else
      let delayed_fn = _fin_end_remove_players_delay players_to_dissolve |> Option.some in
      {v with delayed_fn}
  in
  v

  (* Returns ui_msgs and whether we have a cycle *)
let handle_tick v cur_time =
  let delay_mult = B_options.delay_mult_of_speed v.params.options.speed in
  let tick_delta = delay_mult * C.tick_ms in
  let new_time = v.last_tick + tick_delta in
  let ui_msgs = v.ui_msgs in
  v.ui_msgs <- [];

  if cur_time >= new_time then (
    v.last_tick <- cur_time;
    let v, misc_msgs = Backend_low.handle_cycle ~delayed_fn:_fin_end_proceed v in
    v, ui_msgs @ misc_msgs, true)
  else
    v, ui_msgs, false


let _handle_cheat player_idx cheat v = match cheat with
  | Cheat_d.Add500Cash ->
    update_player v player_idx @@ Player.add_cash @@ M.of_int 500

  | CreatePriorityShipment ->
    let player = Player.get player_idx v.players in
    let stations, player, ui_msgs =
      Backend_low._try_to_create_priority_shipment ~force:true player v.stations v.params v.random
    in
    let players = Player.set C.player player v.players in
    List.iter (send_ui_msg v) ui_msgs;
    [%up {v with players; stations}]

  | CancelPriorityShipment ->
    let players, stations, ui_msgs =
      Backend_low._cancel_expired_priority_shipments ~force:true v.players v.stations v.params
    in
    List.iter (send_ui_msg v) ui_msgs;
    [%up {v with players; stations}]

  | CreateAi ->
    let player_net_worth = get_net_worth player_idx v in
    let track, map, stations, stocks, ai, ui_msgs =
      Ai.ai_track_routines ~force_create:true ~stocks:v.stocks ~params:v.params ~player_net_worth
          ~tilemap:v.map ~tracks:v.track ~cities:v.cities ~stations:v.stations v.random v.ai
    in
    List.iter (send_ui_msg v) ui_msgs;
    [%up {v with track; map; stations; stocks; ai}]

let get_priority_shipment player_idx v =
  let player = Player.get player_idx v.players in
  player.priority

let get_job_and_bonus player_idx v =
  get_player player_idx v
  |> Player.job_and_bonus ~fired:false v.stocks v.params

module Action = struct
  type stop = [`Stop of int | `Priority] [@@deriving show]

  type operate_rr =
    | RRTakeMoney of Money.t
    | RRGiveMoney of Money.t
    | RRBuildTrack of Utils.loc * Utils.loc
    | RRRepayBond
    [@@deriving show]

  type msg = {
    x: int;
    y: int;
    dir: Dir.t;
    player_idx: Owner.t;
  } [@@deriving show]

  type t =
    | NoAction
    | Pause
    | Unpause
    | BuildTrack of msg
    | BuildFerry of msg
    | BuildStation of {x: int; y: int; kind: Station.kind; player_idx: Owner.t}
    | BuildBridge of msg * Bridge.t
    | BuildTunnel of msg * int (* length: 3 or 2 * length *)
    | RemoveTrack of msg
    | DoubleTrack of {x: int; y: int; double: bool; player_idx: Owner.t}
    | ImproveStation of {x:int; y:int; player_idx: Owner.t; upgrade: Station.upgrade}
    | SetSpeed of B_options.speed
    | BuildTrain of {engine: Engine.make; cars: Goods.t list;
                     station: int * int; other_station: (int * int) option; player_idx: Owner.t} 
    | SetStopStation of {train: Trainmap.Id.t; stop: stop; station: int * int; player_idx: Owner.t}
    | RemoveStop of {train: Trainmap.Id.t; stop: stop; player_idx: Owner.t}
    | AddStopCar of {train: Trainmap.Id.t; stop: stop; car: Goods.t; player_idx: Owner.t}
    | RemoveStopCar of {train: Trainmap.Id.t; stop: stop; car: int; player_idx: Owner.t}
    | RemoveAllStopCars of {train: Trainmap.Id.t; stop: stop; player_idx: Owner.t}
    | TrainSetType of {train: Trainmap.Id.t; typ: Train.train_type; player_idx: Owner.t}
    | RemoveTrain of {idx: Trainmap.Id.t; player_idx: Owner.t}
    | TrainReplaceEngine of {train: Trainmap.Id.t; engine: Engine.make; player_idx: Owner.t}
    | TrainToggleStopWait of {train: Trainmap.Id.t; stop: int; player_idx: Owner.t}
    | TrainToggleHold of {player_idx: Owner.t; train_idx: Trainmap.Id.t}
    | BuildIndustry of {player_idx: Owner.t; x: int; y: int; tile: Tile.t}
    | CallBroker of {player_idx: Owner.t}
    | StationSetSignal of {x: int; y: int; dir: Dir.t; cmd: [`Normal| `Hold| `Proceed]}
    | SellBond of {player_idx: Owner.t}
    | RepayBond of {player_idx: Owner.t}
    | Declare_bankruptcy of {player_idx: Owner.t}
    | BuyStock of {player_idx: Owner.t; stock: Owner.t}
    | SellStock of {player_idx: Owner.t; stock: Owner.t}
    | OperateRR of {player_idx: Owner.t; company: Owner.t; action: operate_rr}
    | NameTrain of {player_idx: Owner.t; train: Train.Id.t; name: string}
    | NameRR of {player_idx: Owner.t; name: string; handle: string}
    | Cheat of Owner.t * Cheat_d.t (* player *)
    | Quit_game
    | RunDelayedFn of Owner.t (* Move past first stage of fin_end *)
    [@@deriving show]

  let has_action = function NoAction -> false | _ -> true

  let handle_msgs backend msgs =
    let run_single backend msg =
      if has_action msg then Log.debug (fun f -> f "Received msg %s" (show msg));
      match msg with
      | BuildTrack {x; y; dir; player_idx} ->
          _build_track (x,y) ~dir player_idx backend 
      | BuildFerry {x; y; dir; player_idx} ->
          _build_ferry (x,y) ~dir player_idx backend
      | BuildStation {x; y; kind; player_idx} ->
          _build_station (x,y) kind player_idx backend
      | BuildBridge({x; y; dir; player_idx}, kind) ->
          _build_bridge (x,y) ~dir ~kind player_idx backend
      | BuildTunnel({x; y; dir; player_idx}, _length) ->
          _build_tunnel (x,y) ~dir player_idx backend
      | RemoveTrack {x; y; dir; player_idx} ->
          _remove_track (x,y) ~dir player_idx backend
      | DoubleTrack {x; y; double; player_idx} ->
          _change_double_track (x,y) player_idx ~double backend
      | ImproveStation {x; y; player_idx; upgrade} ->
          _improve_station (x,y) player_idx ~upgrade backend
      | SetSpeed speed ->
          _set_speed backend speed
      | BuildTrain {engine; cars; station; other_station; player_idx} ->
          _build_train station engine cars other_station player_idx backend
      | RemoveStopCar {train; stop; car; player_idx} ->
          _remove_stop_car train ~stop ~car player_idx backend
      | SetStopStation {train; stop; station; player_idx} ->
          _set_stop_station ~train ~stop ~station player_idx backend
      | RemoveStop {train; stop; player_idx} ->
          _remove_stop ~train ~stop player_idx backend
      | RemoveAllStopCars {train; stop; player_idx} ->
          _remove_all_stop_cars ~train ~stop player_idx backend
      | AddStopCar {train; stop; car; player_idx} ->
          _add_stop_car ~train ~stop ~car player_idx backend
      | TrainSetType {train; typ; player_idx} ->
          _train_set_type ~train ~typ player_idx backend
      | RemoveTrain {idx; player_idx} ->
          _remove_train idx player_idx backend
      | TrainReplaceEngine {train; engine; player_idx} ->
          _train_replace_engine ~train ~engine player_idx backend
      | TrainToggleStopWait {train; stop; player_idx} ->
          _train_toggle_stop_wait ~train ~stop player_idx backend
      | TrainToggleHold {player_idx; train_idx} ->
          _train_toggle_hold player_idx train_idx backend
      | StationSetSignal {x; y; dir; cmd} ->
          _station_set_signal (x, y) dir cmd backend
      | BuildIndustry{player_idx; x; y; tile} ->
          _build_industry (x, y) tile player_idx backend
      | CallBroker{player_idx} ->
          _start_broker_timer player_idx backend
      | SellBond{player_idx} ->
          _sell_bond player_idx backend
      | RepayBond{player_idx}->
          _repay_bond player_idx backend
      | BuyStock{player_idx; stock} ->
          _buy_stock backend player_idx ~stock
      | SellStock{player_idx; stock} ->
          _sell_stock player_idx ~stock backend
      | Declare_bankruptcy{player_idx} ->
          _declare_bankruptcy player_idx backend 
      | OperateRR{player_idx; company; action=RRTakeMoney x} ->
          _operate_rr_take_money player_idx ~company ~amount:x backend
      | OperateRR{player_idx; company; action=RRGiveMoney x} ->
          _operate_rr_give_money player_idx ~company ~amount:x backend
      | OperateRR{player_idx; company; action=RRBuildTrack(src,dst)} ->
          _operate_rr_build_track player_idx company src dst backend
      | OperateRR{player_idx; company; action=RRRepayBond} ->
          _operate_rr_repay_bond player_idx ~company_idx:company backend
      | NameTrain {player_idx; train; name} ->
          _name_train player_idx train name backend
      | NameRR {player_idx; name; handle} ->
          _name_rr player_idx name handle backend
      | RunDelayedFn player_idx ->
          begin match backend.delayed_fn with
          | Some fn ->
              let backend = {backend with delayed_fn=None} in
              fn player_idx backend
          | None -> backend
          end
      | Pause -> {backend with pause=true}
      | Unpause -> {backend with pause=false}
      | NoAction -> backend
      | Cheat (player_idx, x) -> _handle_cheat player_idx x backend
      | Quit_game -> backend
    in
    List.fold_left run_single backend msgs
end


