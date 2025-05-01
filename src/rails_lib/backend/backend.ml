open Containers
open! Utils.Infix
open Backend_d

let src = Logs.Src.create "backend" ~doc:"Backend"
module Log = (val Logs.src_log src: Logs.LOG)

module G = Track_graph
module C = Constants

(* This is the backend. All game-modifying functions go through here *)

(* The actual game (server) state
   Observers can observe data in the backend,
   but actions can only be taken via messages (Backend.Action)
 *)

type t = Backend_d.t [@@deriving yojson]
type ui_msg = [%import: Backend_d.ui_msg]

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
  let players = Array.init 1 (fun _ -> Player.default) in
  let year = match region with
    | EastUS -> 1830
    | WestUS -> 1866
    | Britain -> 1828
    | Europe -> 1900
  in
  let graph = Track_graph.make () in
  let engines = Engine.of_region region |> Engine.randomize_year random in
  let stocks = Stock_market.default |> Stock_market.add_human_player ~player:0 options.difficulty in
  let params = {
    Params.year;
    year_start=year;
    num_fiscal_periods=0;
    fiscal_period=`First;
    climate=Normal;
    west_us_route_done=false;
    region;
    options;
  }
  in
  {
    params;
    time=0;
    cycle=0;
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

let get_tile v x y = Tilemap.get_tile v.map x y

let get_track v x y = Trackmap.get v.track ~x ~y

let get_cities v = Cities.to_list v.cities

let get_station loc v = Station_map.get loc v.stations

let get_region v = v.params.region

let get_year v = v.params.year

let get_climate v = v.params.climate

let get_period v = v.params.fiscal_period

let get_map v = v.map

let get_options v = v.params.options

let get_tile_height v x y = Tilemap.get_tile_height v.map x y

let iter_cities f v = Cities.iter f v.cities

let find_close_city v x y ~range = Cities.find_close v.cities x y ~range

let send_ui_msg v msg =
  (* Mutation. Line up ui msg for when we can send it *)
  v.ui_msgs <- msg::v.ui_msgs


let get_cash v ~player = Player.get_cash v.players.(player)

let num_players v = Array.length v.players

let get_player v player = v.players.(player)

let check_build_station v ~x ~y ~player station_type =
  match Trackmap.check_build_station v.track ~x ~y ~player station_type with
  | `Ok -> Tilemap.check_build_station v.map ~x ~y
  | x -> x

let _build_station v ~x ~y station_type ~player =
  let before = Scan.scan v.track ~x ~y ~player in
  let track, build_new_track_dir = Trackmap.build_station v.track ~x ~y station_type in
  let after = Scan.scan track ~x ~y ~player in
  let graph = G.Track.handle_build_station v.graph ~x ~y before after in
  let blocks = Block_map.handle_build_station graph v.blocks track v.players.(player).trains (x,y) after in
  let station = match station_type with
  | `SignalTower ->
    Station.make_signaltower ~x ~y ~year:v.params.year ~player
  | _ ->
    let city_xy = find_close_city ~range:100 v x y |> Option.get_exn_or "error" in
    let check_for_first_city () =
      (* first one has engine shop *)
      match
        Station_map.filter 
          (fun v -> Station.has_upgrade v Station.EngineShop)
        v.stations 
        |> Iter.head
      with Some _ -> false | None -> true
    in
    let first = check_for_first_city () in
    (* Get suffix if needed *)
    let city_name, suffix =
      let (x,y) = city_xy in
      let name, offset = Cities.find_exn v.cities x y in
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
    Station.make ~x ~y
      ~year:v.params.year
      ~city_xy
      ~suffix
      ~city_name
      ~kind:station_type
      ~player
      ~first
  in
  let loc = (x, y) in
  let stations = Station_map.add loc station v.stations in
  update_player v player @@ Player.add_station loc;
  Option.iter (fun dir ->
    update_player v player @@
      Player.update_and_pay_for_track ~x ~y ~dir ~len:1 ~climate:v.params.climate ~map:v.map
  ) build_new_track_dir;
  (* Initialize supply and demand *)
  let simple_economy =
    not @@ B_options.RealityLevels.mem v.params.options.reality_levels `ComplexEconomy 
  in
  let climate = v.params.climate in
  ignore @@ Station.update_supply_demand station v.map ~climate ~simple_economy;
  update_player v player @@
    Player.pay `StructuresEquipment (Station.price_of station_type);
  [%upf v.stations <- stations];
  [%upf v.graph <- graph];
  [%upf v.track <- track];
  [%upf v.blocks <- blocks];
  v

let check_build_tunnel v ~x ~y ~dir ~player =
  let check length = Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length in
  match Tilemap.check_build_tunnel v.map ~x ~y ~dir with
  | `Tunnel(length, _, _) when not(check length) -> `HitsTrack
  | x -> x

let _build_tunnel v ~x ~y ~dir ~player =
  match check_build_tunnel v ~x ~y ~dir ~player with
  | `Tunnel(length, _, cost) ->
    let before = Scan.scan v.track ~x ~y ~player in
    let track = Trackmap.build_tunnel v.track ~x ~y ~dir ~player ~length in
    let after = Scan.scan track ~x ~y ~player in
    let graph = G.Track.handle_build_track_simple v.graph before after in
    let blocks = Block_map.handle_build_track graph v.track v.players.(player).trains v.blocks before after in
    update_player v player @@ Player.pay `BridgeTunnel cost;
    [%upf v.graph <- graph];
    [%upf v.track <- track];
    [%upf v.blocks <- blocks];
    v
  | _ -> v

let _build_bridge v ~x ~y ~dir ~player ~kind =
  let before = Scan.scan v.track ~x ~y ~player in
  let track = Trackmap.build_bridge v.track ~x ~y ~dir ~player ~kind in
  let after = Scan.scan track ~x ~y ~player in
  let graph = G.Track.handle_build_track_simple v.graph before after in
  let blocks = Block_map.handle_build_track graph v.track v.players.(player).trains v.blocks before after in
  update_player v player (fun player ->
    player
    |> Player.update_and_pay_for_track ~x ~y ~dir ~len:2 ~climate:v.params.climate ~map:v.map
    |> Player.pay `BridgeTunnel (Bridge.price_of kind));
  [%upf v.graph <- graph];
  [%upf v.track <- track];
  [%upf v.blocks <- blocks];
  v

let check_build_track v ~x ~y ~dir ~player =
  (* First check the tilemap, then the trackmap *)
  let ret = Tilemap.check_build_track v.map ~x ~y ~dir ~difficulty:v.params.options.difficulty in
  match ret with
  | `Bridge when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length:2 -> `Bridge
  | `Ok | `Ferry | `Tunnel _ | `HighGrade _ when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> ret
  | _ -> `Illegal

let check_build_bridge v ~x ~y ~dir ~player =
  match check_build_track v ~x ~y ~dir ~player with
  | `Bridge -> `Ok
  | _ -> `Illegal

let check_change_double_track v ~x ~y ~player double =
  match Trackmap.get v.track ~x ~y with
  | Some track when track.player = player && Track.is_doubleable track ->
      not @@ Bool.equal double (Track.is_visually_double track)
  | _ -> false

let _change_double_track (v:t) ~x ~y ~player double =
  if check_change_double_track v ~x ~y ~player double then (
    let t = Trackmap.get_exn v.track ~x ~y in
    let t = Track.change_to_double t double in
    let track = Trackmap.set v.track ~x ~y ~t in
    let after = Scan.scan track ~x ~y ~player in
    let blocks = Block_map.handle_double_change v.graph track v.players.(player).trains v.blocks after in
    [%upf v.track <- track];
    [%upf v.blocks <- blocks];
    v
  ) else v
    
let _build_track (v:t) ~x ~y ~dir ~player =
  (* Can either create a new edge or a new node (ixn) *)
  let before = Scan.scan v.track ~x ~y ~player in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player in
  let after = Scan.scan track ~x ~y ~player in
  let graph = G.Track.handle_build_track v.graph ~x ~y before after in
  let blocks = Block_map.handle_build_track graph v.track v.players.(player).trains v.blocks before after in
  update_player v player @@
    Player.update_and_pay_for_track ~x ~y ~dir ~len:1 ~climate:v.params.climate ~map:v.map;
  [%upf v.graph <- graph];
  [%upf v.track <- track];
  [%upf v.blocks <- blocks];
  v

let _build_ferry v ~x ~y ~dir ~player =
  let before = Scan.scan v.track ~x ~y ~player in
  let tile1 = get_tile v x y in
  let dx, dy = Dir.to_offsets dir in
  let tile2 = get_tile v (x+dx) (y+dy) in
  let kind1, kind2 = match tile1, tile2 with
    | Tile.Ocean _ , Ocean _ -> Track.Ferry `Single, Track.Ferry `Single
    | Ocean _, _ -> Ferry `Single, Track `Single
    | _, Ocean _ -> Track `Single, Ferry `Single
    | _ -> assert false
  in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player ~kind1 ~kind2 in
  let after = Scan.scan track ~x ~y ~player in
  let graph = G.Track.handle_build_track_simple v.graph before after in
  let blocks = Block_map.handle_build_track graph v.track v.players.(player).trains v.blocks before after in
  update_player v player @@
    Player.update_and_pay_for_track ~x ~y ~dir ~len:1 ~climate:v.params.climate ~map:v.map;
  [%upf v.graph <- graph];
  [%upf v.track <- track];
  [%upf v.blocks <- blocks];
  v

let check_remove_track v ~x ~y ~dir ~player=
  Trackmap.check_remove_track v.track ~x ~y ~dir ~player

let _remove_station v ~x ~y ~dir ~player =
  let loc = (x,y) in
  let before = Scan.scan v.track ~x ~y ~player in
  (* Have to be careful with order here or we'll mess up state *)
  let blocks = Block_map.handle_remove_station v.graph v.track v.blocks loc before in
  let track = Trackmap.remove_track v.track ~x ~y ~dir ~player in
  let after = Scan.scan track ~x ~y ~player in
  let graph = G.Track.handle_remove_track v.graph ~x ~y before after in
  let stations = Station_map.delete loc v.stations in
  update_player v player (Player.remove_station loc);
  (* TODO: Not sure this is right. Check this in build_station *)
  update_player v player @@
    Player.update_and_remove_track ~x ~y ~dir ~len:1 ~climate:v.params.climate ~map:v.map;
  [%upf v.stations <- stations];
  [%upf v.blocks <- blocks];
  [%upf v.track <- track];
  [%upf v.graph <- graph];
  v

let _remove_track v ~x ~y ~dir ~player =
  let loc = (x,y) in
  let is_station = Trackmap.has_station loc v.track in
  if is_station then _remove_station v ~x ~y ~dir ~player else
  let before = Scan.scan v.track ~x ~y ~player in
  (* Have to be careful with order here or we'll mess up state *)
  let track = Trackmap.remove_track v.track ~x ~y ~dir ~player in
  let after = Scan.scan track ~x ~y ~player in
  let graph = G.Track.handle_remove_track v.graph ~x ~y before after in
  let blocks = Block_map.handle_remove_track graph v.track v.players.(player).trains v.blocks before after in
  update_player v player @@
    Player.update_and_remove_track ~x ~y ~dir ~len:1 ~climate:v.params.climate ~map:v.map;
  [%upf v.blocks <- blocks];
  [%upf v.track <- track];
  [%upf v.graph <- graph];
  v

let _improve_station v ~x ~y ~player ~upgrade =
  let stations = 
    Station_map.update (x,y)
      (Option.map @@ fun station -> Station.add_upgrade station upgrade player)
      v.stations
  in
  update_player v player @@
    Player.(pay `StructuresEquipment @@ Station.price_of_upgrade upgrade);
  [%upf v.stations <- stations];
  v

let _build_train v ((x, y) as station) engine cars other_station ~player =
  let engine_t = Engine.t_of_make v.engines engine in
  (* TODO: Temporary solution for getting track dir *)
  let track = Trackmap.get v.track ~x ~y |> Option.get_exn_or "trackmap" in
  let dir, _ = Dir.Set.pop track.dirs in
  let train = Train.make station engine_t cars other_station ~dir ~player in
  let trains = Trainmap.add v.players.(player).trains train in
  update_player v player (fun player ->
    let player = Player.pay `Train engine_t.price player in
    [%up {player with trains}]
  );
  let msg = TrainBuilt (Trainmap.Id.of_int (Trainmap.size v.players.(player).trains - 1)) in
  send_ui_msg v msg;
  v

let _remove_stop_car v ~train ~stop ~car ~player =
  update_player v player (fun player ->
    let trains =
      Trainmap.update player.trains train
        (fun train -> Train.remove_stop_car train stop car)
    in
    [%up {player with trains}]
  );
  v

let check_stop_station v ~train ~stop ~station ~player =
  let train = Trainmap.get v.players.(player).trains train in
  Train.check_stop_station train stop station

let _set_stop_station v ~train ~stop ~station ~player =
  update_player v player (fun player ->
    let trains =
      Trainmap.update player.trains train
        (fun train -> Train.set_stop_station train stop station)
    in
    [%up {player with trains}]
  );
  v

let _remove_stop v ~train ~stop ~player =
  update_player v player (fun player ->
    let trains =
      Trainmap.update player.trains train
        (fun train -> Train.remove_stop train stop)
    in
    [%up {player with trains}]
  );
  v

let _add_stop_car v ~train ~stop ~car ~player =
  update_player v player (fun player ->
    let trains =
      Trainmap.update player.trains train
        (fun train -> Train.add_stop_car train stop car)
    in
    [%up {player with trains}]
  );
  v

let _remove_all_stop_cars v ~train ~stop ~player =
  update_player v player (fun player ->
    let trains =
      Trainmap.update player.trains train
        (fun train -> Train.remove_all_stop_cars train stop)
    in
    [%up {player with trains}]
  );
  v

let get_num_trains v ~player = Trainmap.size v.players.(player).trains

let get_train v idx ~player = Trainmap.get v.players.(player).trains idx
  
let trackmap_iter v f = Trackmap.iter v.track f

let _train_set_type v ~train ~typ ~player =
  update_player v player (fun player ->
    let trains =
      Trainmap.update player.trains train (fun train -> Train.set_type train typ)
    in
    [%up {player with trains}]
  );
  v

let _train_replace_engine v ~train ~engine ~player =
  let engine = Engine.t_of_make v.engines engine in
  update_player v player (fun player ->
    let player = Player.pay `Train engine.price player in
    let trains =
      Trainmap.update player.trains train
        (fun train -> Train.replace_engine train engine)
    in
    [%up {player with trains}]
  );
  v

let _train_toggle_stop_wait v ~train ~stop ~player =
  update_player v player (fun player ->
    let trains =
      Trainmap.update player.trains train
        (fun train -> Train.toggle_stop_wait train stop)
    in
    [%up {player with trains}]
  );
  v

let _station_set_signal v loc dir cmd =
  (* the user can only set the override *)
  let signal = match cmd with
  | `Normal -> Station.NoOverride
  | `Hold -> OverrideHold
  | `Proceed -> OverrideProceed
  in
  let stations = Station_map.update loc 
    (Option.map (fun station -> Station.set_override station dir signal))
    v.stations
  in
  [%up {v with stations}]

let _build_industry v ~player_idx (x, y) tile =
  if Tilemap.check_build_industry_at v.map tile ~region:v.params.region ~x ~y then (
    Tilemap.set_tile v.map x y tile;
    let cost = Tile.Info.build_cost_of_tile v.params.region tile in
    update_player v player_idx (Player.build_industry cost);
    send_ui_msg v @@ IndustryBuilt {player=player_idx; tile};
    v
  ) else
    v
  
let _remove_train v idx ~player =
  update_player v player (fun player ->
    let train = Trainmap.get player.trains idx in
    (match train.state with
      | Traveling {block; _} ->
        Block_map.block_decr_train block v.blocks
      | _ -> ());
    let trains = Trainmap.delete player.trains idx in
    [%up {player with trains}]
  );
  v

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
  let player = get_player v player_idx in
  Player.get_interest_rate player v.params.climate v.params.region 

let player_has_bond v player_idx =
  let player = get_player v player_idx in
  Player.has_bond player

let _sell_bond v player_idx =
  let player = get_player v player_idx in
  if Player.check_sell_bond player v.params.climate v.params.region then (
    let interest_rate = Player.get_interest_rate player v.params.climate v.params.region in
    (* Must be before we get new bond *)
    update_player v player_idx (fun player -> Player.sell_bond player v.params.climate v.params.region);
    send_ui_msg v @@ StockBroker(BondSold{player=player_idx; interest_rate});
    v
  ) else v

  (* TODO: evaluate ai company value *)
let _repay_bond v player_idx =
  let player = get_player v player_idx in
  if Player.check_repay_bond player then (
    update_player v player_idx Player.repay_bond;
    send_ui_msg v @@ StockBroker(BondRepaid{player=player_idx});
    v
  ) else v

let can_buy_stock v ~player_idx ~target =
  let player = get_player v player_idx in
  Stock_market.can_buy_stock ~player:player_idx ~target ~cash:(Player.get_cash player) ~difficulty:v.params.options.difficulty v.stocks

let _buy_stock v player_idx ~stock = 
  let difficulty = v.params.options.difficulty in
  let player = get_player v player_idx in
  let cash = Player.get_cash player in
  match Stock_market.buy_stock ~player:player_idx ~target:stock ~cash ~difficulty v.stocks with
  | `Bought cost, stocks ->
      update_player v player_idx (Player.add_cash @@ -cost);
      send_ui_msg v @@ StockBroker(StockBought {player=player_idx; stock; cost});
      {v with stocks}
  | `Takeover money_map, stocks ->
      Utils.IntMap.iter (fun player change -> update_player v player @@ Player.add_cash change) money_map;
      send_ui_msg v @@ StockBroker(Takeover {player=player_idx; stock});
      {v with stocks}
  | _, stocks ->
      [%up {v with stocks}]

let _sell_stock v player_idx ~stock =
  let cost, stocks = Stock_market.sell_stock player_idx ~target:stock v.stocks in
  update_player v player_idx @@ Player.add_cash cost;
  send_ui_msg v @@ StockBroker(StockSold {player=player_idx; stock; cost});
  [%up {v with stocks}]

let check_bankruptcy v player_idx =
  let player = get_player v player_idx in
  Player.check_bankruptcy player

let _declare_bankruptcy v player_idx =
  let player = Player.get_player v.players player_idx in
  if Player.check_bankruptcy player then (
    let players, stocks = Player.declare_bankruptcy v.players player_idx ~difficulty:v.params.options.difficulty v.stocks in
    send_ui_msg v @@ StockBroker(BankruptcyDeclared {player=player_idx});
    [%up {v with players; stocks}]
  ) else v

let get_date v = _month_of_time v.time, v.params.year

let get_time_of_day time =
  (* Get time of day representation *)
  let minutes = 3 * time / 8 in
  let hours = minutes / 60 in
  let time_hours = ((hours + 11) / 12) + 1 in
  let time_mins = minutes mod 60 in
  let am_pm = if hours >= 12 then "PM" else "AM" in
  Printf.sprintf "%d:%02d %s" time_hours time_mins am_pm

let get_company_name v player_idx =
  let player = get_player v player_idx in
  Player.get_name player v.stations v.cities

let companies_controlled_by v player_idx =
  Stock_market.other_companies_controlled_by player_idx v.stocks

let _operate_rr_take_money v ~player_idx ~company ~amount =
  update_player v player_idx @@ Player.add_cash amount;
  update_player v company @@ Player.add_cash @@ -amount;
  (* TODO: fix this *)
  (* Player.update_ai_valuation v.players player_idx; *)
  send_ui_msg v @@ StockBroker(MoneyTransferredFrom{player=player_idx; company; amount});
  v
  
let _operate_rr_give_money v ~player_idx ~company ~amount =
  let player = get_player v player_idx in
  if not @@ Player.in_receivership player then (
    update_player v player_idx @@ Player.add_cash @@ - amount;
    update_player v company @@ Player.add_cash amount
  );
  (* TODO: fix this *)
  (* Player.update_ai_valuation v.players player_idx; *)
  send_ui_msg v @@ StockBroker(MoneyTransferredTo{company; player=player_idx; amount});
  v

let _operate_rr_repay_bond v ~player_idx ~company_idx =
  update_player v company_idx Player.repay_bond;
  (* TODO: fix this *)
  (* Player.update_ai_valuation v.players player_idx; *)
  send_ui_msg v @@ StockBroker(AiBondRepaid {player=player_idx; company=company_idx});
  v

  (* TODO: RR build track *)
let _operate_rr_build_track v ~player_idx ~company _src _dst =
  let _player_idx, _company = player_idx, company in
  v

let broker_timer_active v player_idx =
  Player.has_broker_timer (get_player v player_idx)

let _start_broker_timer v player_idx =
  (* Only activate if no broker time active *)
  if not @@ broker_timer_active v player_idx then (
    update_player v player_idx (fun player ->
      Player.incr_broker_timer player |> fst);
    v
  ) else v

let _handle_cheat v player = function
  | Cheat_d.Add500Cash ->
    update_player v player @@ Player.add_cash 500;
    v
  | CreatePriorityShipment ->
    let player = Player.get_player v.players player in
    let stations, player, ui_msgs = Backend_low._try_to_create_priority_shipment v ~force:true player v.stations in
    [%upf v.stations <- stations];
    Player.set v.players C.player player;
    List.iter (send_ui_msg v) ui_msgs;
    v
  | CancelPriorityShipment ->
    let ui_msgs = Backend_low._try_to_cancel_priority_shipments v ~force:true in
    List.iter (send_ui_msg v) ui_msgs;
    v
    

let get_priority_shipment v player =
  let player = Player.get_player v.players player in
  player.priority

module Action = struct
  type stop = [`Stop of int | `Priority] [@@deriving show]

  type operate_rr =
    | RRTakeMoney of int
    | RRGiveMoney of int
    | RRBuildTrack of Utils.loc * Utils.loc
    | RRRepayBond
    [@@deriving show]

  type t =
    | NoAction
    | Pause
    | Unpause
    | BuildTrack of Utils.msg
    | BuildFerry of Utils.msg
    | BuildStation of {x: int; y: int; kind: Station.kind; player: int}
    | BuildBridge of Utils.msg * Bridge.t
    | BuildTunnel of Utils.msg * int (* length: 3 or 2 * length *)
    | RemoveTrack of Utils.msg
    | DoubleTrack of {x: int; y: int; double: bool; player: int}
    | ImproveStation of {x:int; y:int; player: int; upgrade: Station.upgrade}
    | SetSpeed of B_options.speed
    | BuildTrain of {engine: Engine.make; cars: Goods.t list;
                     station: int * int; other_station: (int * int) option; player: int} 
    | SetStopStation of {train: Trainmap.Id.t; stop: stop; station: int * int; player: int}
    | RemoveStop of {train: Trainmap.Id.t; stop: stop; player: int}
    | AddStopCar of {train: Trainmap.Id.t; stop: stop; car: Goods.t; player: int}
    | RemoveStopCar of {train: Trainmap.Id.t; stop: stop; car: int; player: int}
    | RemoveAllStopCars of {train: Trainmap.Id.t; stop: stop; player: int}
    | TrainSetType of {train: Trainmap.Id.t; typ: Train.train_type; player: int}
    | RemoveTrain of {idx: Trainmap.Id.t; player: int}
    | TrainReplaceEngine of {train: Trainmap.Id.t; engine: Engine.make; player: int}
    | TrainToggleStopWait of {train: Trainmap.Id.t; stop: int; player: int}
    | BuildIndustry of {player: int; x: int; y: int; tile: Tile.t}
    | CallBroker of {player: int}
    | StationSetSignal of {x: int; y: int; dir: Dir.t; cmd: [`Normal| `Hold| `Proceed]}
    | SellBond of {player: int}
    | RepayBond of {player: int}
    | Declare_bankruptcy of {player: int}
    | BuyStock of {player: int; stock: int}
    | SellStock of {player: int; stock: int}
    | OperateRR of {player: int; company: int; action: operate_rr}
    | Cheat of int * Cheat_d.t (* player *)
    | Quit_game
    [@@deriving show]

  let has_action = function NoAction -> false | _ -> true

  let handle_msgs backend msgs =
    let run_single backend msg =
      if has_action msg then Log.debug (fun f -> f "Received msg %s" (show msg));
      match msg with
      | BuildTrack {x; y; dir; player} ->
          _build_track backend ~x ~y ~dir ~player
      | BuildFerry {x; y; dir; player} ->
          _build_ferry backend ~x ~y ~dir ~player
      | BuildStation {x; y; kind; player} ->
          _build_station backend ~x ~y kind ~player
      | BuildBridge({x; y; dir; player}, kind) ->
          _build_bridge backend ~x ~y ~dir ~kind ~player
      | BuildTunnel({x; y; dir; player}, _length) ->
          _build_tunnel backend ~x ~y ~dir ~player
      | RemoveTrack {x; y; dir; player} ->
          _remove_track backend ~x ~y ~dir ~player
      | DoubleTrack {x; y; double; player} ->
          _change_double_track backend ~x ~y ~player double
      | ImproveStation {x; y; player; upgrade} ->
          _improve_station backend ~x ~y ~player ~upgrade
      | SetSpeed speed ->
          _set_speed backend speed
      | BuildTrain {engine; cars; station; other_station; player} ->
          _build_train backend station engine cars other_station ~player
      | RemoveStopCar {train; stop; car; player} ->
          _remove_stop_car backend ~train ~stop ~car ~player
      | SetStopStation {train; stop; station; player} ->
          _set_stop_station backend ~train ~stop ~station ~player
      | RemoveStop {train; stop; player} ->
          _remove_stop backend ~train ~stop ~player
      | RemoveAllStopCars {train; stop; player} ->
          _remove_all_stop_cars backend ~train ~stop ~player
      | AddStopCar {train; stop; car; player} ->
          _add_stop_car backend ~train ~stop ~car ~player
      | TrainSetType {train; typ; player} ->
          _train_set_type backend ~train ~typ ~player
      | RemoveTrain {idx; player} ->
          _remove_train backend idx ~player
      | TrainReplaceEngine {train; engine; player} ->
          _train_replace_engine backend ~train ~engine ~player
      | TrainToggleStopWait {train; stop; player} ->
          _train_toggle_stop_wait backend ~train ~stop ~player
      | StationSetSignal {x; y; dir; cmd} ->
          _station_set_signal backend (x, y) dir cmd
      | BuildIndustry{player; x; y; tile} ->
          _build_industry backend ~player_idx:player (x, y) tile
      | CallBroker{player} ->
          _start_broker_timer backend player
      | SellBond{player} ->
          _sell_bond backend player
      | RepayBond{player}->
          _repay_bond backend player
      | BuyStock{player; stock} ->
          _buy_stock backend player ~stock
      | SellStock{player; stock} ->
          _sell_stock backend player ~stock
      | Declare_bankruptcy{player} ->
          _declare_bankruptcy backend player
      | OperateRR{player; company; action=RRTakeMoney x} ->
          _operate_rr_take_money backend ~player_idx:player ~company ~amount:x
      | OperateRR{player; company; action=RRGiveMoney x} ->
          _operate_rr_give_money backend ~player_idx:player ~company ~amount:x
      | OperateRR{player; company; action=RRBuildTrack(src,dst)} ->
          _operate_rr_build_track backend ~player_idx:player ~company src dst
      | OperateRR{player; company; action=RRRepayBond} ->
          _operate_rr_repay_bond backend ~player_idx:player ~company_idx:company
      | Pause -> {backend with pause=true}
      | Unpause -> {backend with pause=false}
      | NoAction -> backend
      | Cheat (player, x) -> _handle_cheat backend player x
      | Quit_game -> backend
    in
    List.fold_left run_single backend msgs
end


