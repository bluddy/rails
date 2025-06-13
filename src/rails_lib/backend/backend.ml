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

(* This is the backend. All game-modifying functions go through here *)

(* The actual game (server) state
   Observers can observe data in the backend,
   but actions can only be taken via messages (Backend.Action)
 *)

type t = Backend_d.t [@@deriving yojson]

let default region resources ~random ~seed = 
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
  let options = B_options.default in
  let stations = Station_map.empty in
  let players = Owner.Map.singleton C.player @@ Player.default C.player in
  let year = match region with
    | EastUS -> 1830
    | WestUS -> 1866
    | Britain -> 1828
    | Europe -> 1900
  in
  let graph = Track_graph.make () in
  let engines = Engine.of_region region |> Engine.randomize_year random in
  let stocks = Stock_market.default
    |> Stock_market.add_human_player C.player options.difficulty in
  let params = { Params.default with year; year_start=year; region }
  in
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

let find_close_city x y ~range v = Cities.find_close x y v.cities ~range

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

let get_track_length player_idx v =
  get_player_ai Player.track_length Ai.get_track_length player_idx v

let players_and_ai v =
  Iter.append (Owner.Map.keys v.players) (Ai.ai_iter v.ai)

let check_build_station x y player_idx station_type v =
  let loc = (x, y) in
  match Trackmap.check_build_station loc player_idx station_type v.track with
  | `Ok -> Tilemap.check_build_station loc v.map
  | x -> x

let _build_station ((x,y) as loc) station_type player_idx v =
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
      |> Player.pay `StructuresEquipment (Station.price_of station_type)
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

let _build_bridge ((x, y) as loc) ~dir player_idx ~kind v =
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

let check_build_track loc ~dir player_idx v =
  (* First check the tilemap, then the trackmap *)
  match Tilemap.check_build_track loc ~dir v.params v.map with
  | `Bridge when Trackmap.check_build_stretch loc ~dir player_idx ~length:2 v.track -> `Bridge
  | (`Ok | `Ferry | `Tunnel _ | `HighGrade _) as x when Trackmap.check_build_track loc ~dir player_idx v.track -> x
  | _ -> `Illegal

let check_build_bridge loc ~dir player_idx v =
  match check_build_track loc ~dir player_idx v with
  | `Bridge -> `Ok
  | _ -> `Illegal

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
    
let _build_track ((x, y) as loc) ~dir player_idx v =
  (* Can either create a new edge or a new node (ixn) *)
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
  [%up {v with graph; track; blocks; players}]

let _build_ferry ((x, y) as loc) ~dir player_idx v =
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

let _remove_station ((x, y) as loc) ~dir player_idx v =
  let track, graph, stations, blocks = v.track, v.graph, v.stations, v.blocks in
  let before = Scan.scan track loc player_idx in
  (* Have to be careful with order here or we'll mess up state *)
  let blocks = Block_map.handle_remove_station graph track blocks loc before in
  let track = Trackmap.remove_track loc player_idx ~dir track in
  let after = Scan.scan track loc player_idx in
  let graph = G.Track.handle_remove_track x y graph before after in
  let stations = Station_map.delete loc stations in
  (* TODO: Not sure this is right. Check this in build_station *)
  let players =
    Player.update v.players player_idx (fun player ->
      player
      |> Player.remove_station loc
      |> Player.update_and_remove_track x y ~len:1 ~dir ~climate:v.params.climate v.map)
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
    Station_map.update loc (Option.map @@ Station.add_upgrade upgrade player_idx) v.stations
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

let _station_set_signal loc dir cmd v =
  (* the user can only set the override *)
  let signal = match cmd with
  | `Normal -> Station.NoOverride
  | `Hold -> OverrideHold
  | `Proceed -> OverrideProceed
  in
  let stations = Station_map.update loc 
    (Option.map (fun station -> Station.set_override dir signal station ))
    v.stations
  in
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

  (* Returns ui_msgs and whether we have a cycle *)
let handle_tick v cur_time =
  let delay_mult = B_options.delay_mult_of_speed v.params.options.speed in
  let tick_delta = delay_mult * C.tick_ms in
  let new_time = v.last_tick + tick_delta in
  let ui_msgs = v.ui_msgs in
  v.ui_msgs <- [];

  if cur_time >= new_time then (
    v.last_tick <- cur_time;
    let v, misc_msgs = Backend_low.handle_cycle v in
    v, ui_msgs @ misc_msgs, true)
  else
    v, ui_msgs, false

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

let _operate_rr_repay_bond player_idx ~company_idx v =
  let v = update_player v company_idx Player.repay_bond in
  (* TODO: fix this *)
  (* Player.update_ai_valuation v.players player_idx; *)
  send_ui_msg v @@ StockBroker(AiBondRepaid {player_idx; company=company_idx});
  v

  (* TODO: RR build track *)
let _operate_rr_build_track player_idx ~company _src _dst v =
  let _player_idx, _company = player_idx, company in
  v

let broker_timer_active player_idx v =
  Player.get player_idx v.players |> Player.has_broker_timer 

let _start_broker_timer player_idx v =
  (* Only activate if no broker time active *)
  if not @@ broker_timer_active player_idx v then (
    update_player v player_idx (fun player -> fst @@ Player.incr_broker_timer player)
  ) else v

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
    | Cheat of Owner.t * Cheat_d.t (* player *)
    | Quit_game
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
          _operate_rr_build_track player_idx ~company src dst backend
      | OperateRR{player_idx; company; action=RRRepayBond} ->
          _operate_rr_repay_bond player_idx ~company_idx:company backend
      | NameTrain {player_idx; train; name} ->
          _name_train player_idx train name backend
      | Pause -> {backend with pause=true}
      | Unpause -> {backend with pause=false}
      | NoAction -> backend
      | Cheat (player_idx, x) -> _handle_cheat player_idx x backend
      | Quit_game -> backend
    in
    List.fold_left run_single backend msgs
end


