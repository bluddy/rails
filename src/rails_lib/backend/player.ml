(* A human player and his related data *)

open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module C = Constants
module List = Utils.List
open! Utils.Infix

let src = Logs.Src.create "player" ~doc:"Player"
module Log = (val Logs.src_log src: Logs.LOG)

module IntMap = Utils.IntMap
module Vector = Utils.Vector
module U = Utils
module M = Money
module Random = U.Random

type monetary = {
  cash: Money.t; (* all x1000 *)
  bonds: Money.t;
  stockholders_equity : Money.t; (* not sure how this changes *)
  owned_industry: Money.t;
  yearly_interest_payment: Money.t;
  net_worth: Money.t;
  profit: Money.t;
  in_receivership: bool; (* bankruptcy *)
  income_statement: Income_statement_d.t;
  total_income_statement: Income_statement_d.t;
  last_balance_sheet: Balance_sheet_d.t;
  num_bankruptcies: int;
  investor_anger: int; (* at 5+, you'll be fired *)
} [@@deriving yojson]

let default_monetary = {
  cash = Money.of_int 1000;
  bonds = Money.of_int 500;
  stockholders_equity = Money.of_int @@ -500;
  owned_industry = Money.zero;
  profit = Money.zero;
  yearly_interest_payment = Money.of_int 20;
  net_worth = Money.of_int 500; 
  in_receivership = false;
  income_statement = Income_statement_d.default;
  total_income_statement = Income_statement_d.default;
  last_balance_sheet = Balance_sheet_d.default;
  num_bankruptcies = 0;
  investor_anger = 0;
}

type event =
  BridgeWashout of U.loc
  [@@deriving yojson]

 (* TODO: this is ton-miles traveled vs delivered (in trains). Check *)

type periodic = {
  mutable dist_traveled: int;
  mutable time_running: int;  (* TODO: update this in train_update *)
  ton_miles: int; (* goods delievered per mile per period *)
  freight_ton_miles: int Freight.Map.t; (* per period *)
} [@@deriving yojson]

let make_periodic () = {
  dist_traveled=0;
  time_running=0;
  ton_miles=0;
  freight_ton_miles=Freight.Map.empty;
}

module Record = struct
  type t = {
    earnings: Money.t; (* Earnings is delta in net worth *)
    total_revenue: Money.t;
    avg_speed: int;
    ton_miles: int;
    train_speed: int;
    job: Jobs.t option;
  } [@@deriving yojson]

  let default = {
    earnings=Money.zero;
    total_revenue=Money.zero;
    avg_speed=0;
    ton_miles=0;
    train_speed=2; (* starting record to beat *)
    job=None;
  }
end

module History = struct
  (* All reversed *)
  type t = {
    net_worth: Money.t list;
    earnings: Money.t list; (* reversed list *)
    total_revenue: Money.t list;
    avg_speed: int list; (* reversed *)
    ton_miles: int list;
    track_pieces: int list; (* track length per period *)
  } [@@deriving yojson]

  let default = {
    net_worth=[];
    earnings=[];
    total_revenue=[];
    avg_speed=[];
    ton_miles=[];
    track_pieces=[]; (* track length per period *)
  }
end

module Achievement = struct
  (* 8 achievements. We store the year of achievement *)

  let num = 8 - 1

  let track_length_of_idx idx = 100 * idx + 100
  let revenue_of_idx idx = 250 * idx + 250 |> M.of_int
  let net_worth_of_idx idx = (1 lsl idx) * 100 |> M.of_int

  type year = int
    [@@deriving yojson]

  type t = {
    track_length: year IntMap.t;
    revenue: year IntMap.t;
    net_worth: year IntMap.t;
  } [@@deriving yojson]

  let default = {
    track_length=IntMap.empty;
    revenue=IntMap.empty;
    net_worth=IntMap.empty;
  }

  let update x f gt ~year map =
    Iter.fold (fun acc i ->
      if gt x (f i) then
        IntMap.update i (function None -> Some year | x -> x) acc
      else
        acc)
    map
    Iter.(0 -- num)

  let update_all ~year ~track_length ~revenue ~net_worth v =
    let track_length = update track_length track_length_of_idx (>) ~year v.track_length in
    let revenue = update revenue revenue_of_idx M.(>) ~year v.revenue in
    let net_worth = update net_worth net_worth_of_idx M.(>) ~year v.net_worth in
    {track_length; revenue; net_worth}
end

type t = {
  idx: Owner.t;
  name: (string * string) option; (* custom name, handle for railroad *)
  mutable trains: Trainmap.t;
  station_locs: Utils.loc list; (* Stations ordered by reverse creation order *)
  m: monetary;
  track_length: int; (* track length according to the game (not per track piece) *)
  (* TODO: problem: this is used as history + owned track (for maintenance). But we can delete history this way! *)
  track: Utils.loc Vector.vector; (* vector of track owned by player *)
  goods_delivered: Goods.Set.t;  (* goods delivered so far (for newness) *)
  goods_picked_up: Goods.Set.t;  (* goods picked up *)
  broker_timer: int option;  (* Time left to see broker, if any *)
  priority: Priority_shipment.t option;
   (* A current active station, which causes high development *)
  mutable active_station: Utils.loc option;
  event : event option;
  periodic: periodic * periodic;
  total_difficulty: int;  (* dynamic tracker of difficulty over time *)
  history: History.t;
  record: Record.t;
  achievements: Achievement.t;
} [@@deriving yojson]


let default idx = {
  idx;
  name=None;
  station_locs = [];
  m = default_monetary;
  trains = Trainmap.empty ();
  track_length = 0;
  track = Vector.create ();
  goods_delivered=Goods.Set.empty;
  goods_picked_up=Goods.Set.empty;
  broker_timer=None;
  priority=None;
  active_station=None;
  event=None;
  periodic=(make_periodic (), make_periodic ());
  total_difficulty=0;
  history = History.default;
  record = Record.default;
  achievements = Achievement.default;
}

let get_cash v = v.m.cash

let net_worth v = v.m.net_worth

let get_profit v = v.m.profit

let get_net_worth v = v.m.net_worth

let bonds v = v.m.bonds

let modify_cash f v = {v with m={v.m with cash = f v.m.cash}}

let add_cash x v = modify_cash (fun cash -> Money.(cash + x)) v

let in_receivership v = v.m.in_receivership

let get_trains v = v.trains

let update_trains f v =
  let trains2 = f v.trains in
  [%up {v with trains=trains2}]

let pay expense money (v:t) =
  let income_statement = Income_statement.deduct expense money v.m.income_statement in
  let cash = Money.(v.m.cash - money) in
  {v with m = {v.m with cash; income_statement}}

let earn revenue money (v:t) =
  let income_statement = Income_statement.add_revenue revenue money v.m.income_statement in
  let cash = Money.(v.m.cash + money) in
  {v with m = {v.m with cash; income_statement}}

let add_income_stmt income_stmt (v:t) =
  let income_statement = Income_statement.merge v.m.income_statement income_stmt in
  let cash = Money.(v.m.cash + Income_statement.total income_stmt) in
  {v with m={v.m with income_statement; cash}}

let revenue_sum v = Income_statement.total_revenue v.m.income_statement

let get_ton_miles period v = (Utils.read_pair v.periodic period).ton_miles

(* "Game" reported track length, not track pieces *)
let track_length v = v.track_length

(* Number of track pieces we've placed *)
let get_num_track_pieces v = Vector.length v.track

let fiscal_period_end net_worth stations params v =
  (* Messages, computation, housecleaning *)
  let current_period = Params.current_period params in
  let next_period = Params.last_period params in
  let trains = v.trains in
  let ui_msgs = [] in
  let ui_msgs =
    (* Trainmap is mutable *)
    Trainmap.fold_mapi_in_place (fun idx acc train ->
      let period = Train.get_period current_period train in
      let acc = if Money.(period.revenue = zero) then Ui_msg.TrainNoRevenue(idx)::acc else acc in
      let acc = if not train.had_maintenance then Ui_msg.TrainNoMaintenance(idx)::acc else acc in
      let acc = if Train.get_route_length train = 0 then Ui_msg.TrainNoSchedule(idx)::acc else acc in
      let maintenance_cost =
        let added_maint = if Train.get_engine train |> Engine.has_steam then 2 else 1 in
        let no_maint_penalty = if train.had_maintenance then 0 else 2 in
        Money.(train.maintenance_cost +~ added_maint +~ no_maint_penalty) in
      let acc = if Money.(maintenance_cost >= Train.get_engine_cost train) then Ui_msg.TrainOldEngine(idx)::acc else acc in
      let periodic = Train.update_periodic next_period train.periodic (fun _ -> Train.make_periodic ()) in
      acc, {train with had_maintenance=false; periodic})
      trains
      ~init:ui_msgs
  in
  let player_idx = v.idx in
  let ui_msgs =
    List.fold_left (fun acc loc ->
      let station = Station_map.get_exn loc stations in
      if Station.has_override_hold station then
        let loc =
          if Station.is_proper_station station then loc
          else
            Station_map.find_nearest ~player_idx ~only_proper:true loc stations 
            |> Option.get_exn_or "couldn't find station" |> fun x -> x.loc
        in
        Ui_msg.StationHasHold(loc)::acc
      else acc)
      ui_msgs
      v.station_locs
  in
  let total_revenue = revenue_sum v in
  let total_income_statement = Income_statement.merge v.m.total_income_statement v.m.income_statement in
  let ui_msgs =
    if Money.(total_revenue / 2 < v.m.yearly_interest_payment && v.m.bonds > Money.of_int 2000) then
      (Ui_msg.ConsiderBankruptcy)::ui_msgs else ui_msgs
  in
  let earnings = Money.((net_worth - v.m.net_worth) * 10) in (* aka profit *)
  let earnings_record, ui_msgs =
    if Money.(earnings > v.record.earnings) then
      earnings, Ui_msg.RecordEarnings(earnings)::ui_msgs
    else
      v.record.earnings, ui_msgs
  in
  let income_statement = Income_statement.default in
  let total_time = (10 + Pair.fold (fun p1 p2 -> p1.time_running + p2.time_running) v.periodic) / 10 in
  let total_dist = 6 * (Pair.fold (fun p1 p2 -> p1.dist_traveled + p2.dist_traveled) v.periodic) in
  let avg_speed = total_dist / total_time in
  let avg_speed_record, ui_msgs =
    if avg_speed > v.record.avg_speed then
      avg_speed, Ui_msg.AvgSpeedRecord(avg_speed)::ui_msgs
    else
      v.record.avg_speed, ui_msgs
  in
  let ton_miles = get_ton_miles current_period v in
  let ton_mile_record, ui_msgs =
    if ton_miles > v.record.ton_miles then
      ton_miles, Ui_msg.TonMileRecord(ton_miles)::ui_msgs
    else
      v.record.ton_miles, ui_msgs
  in
  let total_revenue_record, ui_msgs =
    if Money.(total_revenue > v.record.total_revenue) then
      total_revenue, Ui_msg.RevenueRecord(total_revenue)::ui_msgs
    else
      v.record.total_revenue, ui_msgs
  in
  let m = { v.m with net_worth; income_statement; total_income_statement; profit=earnings } in
  let history = History.{
    v.history with
    total_revenue=total_revenue::v.history.total_revenue;
    earnings=earnings::v.history.earnings;
    ton_miles=ton_miles::v.history.ton_miles;
    avg_speed=(avg_speed / 2)::v.history.avg_speed;
    net_worth=net_worth::v.history.net_worth;
  } in
  let record = Record.{
    v.record with
    ton_miles=ton_mile_record;
    avg_speed=avg_speed_record;
    earnings=earnings_record;
    total_revenue=total_revenue_record;
  } in
  let v = {v with m; history; record } in
  v, total_revenue, ui_msgs

let fiscal_period_end_stock_eval ~total_revenue ~net_worth stocks params v =
  let player_idx = v.idx in
  let old_share_price = Stock_market.share_price v.idx stocks in
  let total_shares = Stock_market.total_shares v.idx stocks in
  let share_price = M.div M.((total_revenue / 4 + net_worth + of_int 5)) (total_shares / 10) in
  let share_price = M.(max (of_int 1) share_price) in
  let avg_share_price = Stock_market.avg_share_price player_idx stocks in
  let avg_share_price = M.((avg_share_price * 7) / 8 + share_price) in
  let fiscal_period_div = Utils.clip params.Params.num_fiscal_periods ~min:1 ~max:4 in
  let z = M.((((share_price * 8) - avg_share_price) * 12) + (avg_share_price / 16)) in
  (* In percent *)
  let share_price_growth = M.((z /~ ((avg_share_price / 8) +~ 1))) / fiscal_period_div in
  let stocks = stocks
    |> Stock_market.set_share_price player_idx share_price
    |> Stock_market.set_avg_share_price player_idx avg_share_price
  in
  let split = M.(share_price > of_int 100) in
  let stocks = if split then Stock_market.split_stock player_idx stocks else stocks in
  let stocks = Stock_market.update_history_with_stock_value player_idx stocks in
  let investor_opinion = Stock_market.investor_opinion share_price_growth in
  let investor_anger = v.m.investor_anger + Stock_market.investor_anger_mod investor_opinion in
  let controls_company = Stock_market.controls_company player_idx ~target:player_idx stocks in
  let fired = if not controls_company then
      if investor_anger >= 5 then `Fired
      else if investor_anger >= 3 then `Warning
      else if investor_anger >= 1 then `MinorWarning
      else `Normal
    else `Normal
  in
  let msg = Ui_msg.SharePriceChange{
    player_idx; from_=old_share_price; to_=share_price; share_price_growth; split; fired}
  in
  {v with m={v.m with investor_anger}}, stocks, [msg]

let fiscal_period_end_achievements ~revenue ~net_worth params v =
  (* Handle achievements *)
  let year, track_length = params.Params.year, v.track_length in
  let achievements = Achievement.update_all ~year ~track_length ~revenue ~net_worth v.achievements in
  {v with achievements}

let update_track_history v =
  (* Update track length. Unlike OG, we do this every year *)
  let track_pieces = get_num_track_pieces v in
  let history =
    let track_pieces = track_pieces::v.history.track_pieces in
    {v.history with track_pieces }
  in
  {v with history}

let clear_periodic params v =
  (* Should be called on a new period *)
  let periodic =
    Utils.update_pair v.periodic params.Params.current_period (fun _ -> make_periodic ())
  in
  let trains = Trainmap.mapi_in_place (fun _ train -> Train.clear_periodic params train) v.trains in
  {v with periodic; trains}

let build_industry cost (v:t) =
  let v = pay `StructuresEquipment cost v in
  let owned_industry = Money.(v.m.owned_industry + cost) in
  {v with m={v.m with owned_industry}}

let _get_first_2_cities station_map cities v =
  (* Getting the name if it's not custom is... complicated *)
  List.fold_right (fun loc acc ->
    if List.length acc >= 2 then acc
    else
      let station = Station_map.get_exn loc station_map in
      match station.Station.info with 
      | Some info ->
        let x, y = info.city in
        let city, _ = Cities.find_exn x y cities in
        city::acc
      | _ -> acc)
  v.station_locs
  []

let get_name_and_handle station_map cities v = match v.name with
  | Some (name, handle) -> name, handle
  | _ -> 
    match _get_first_2_cities station_map cities v with
    | x::y::_ -> Printf.sprintf "%s & %s RR" y x, Printf.sprintf "%c&%c" x.[0] y.[0]
    | x::_ -> Printf.sprintf "%s RR" x, Printf.sprintf "%c" x.[0]
    | _ -> "RR", "RR"

let get_name station_map cities v = 
  get_name_and_handle station_map cities v |> fst

let get_handle station_map cities v =
  get_name_and_handle station_map cities v |> snd

let incr_dist_traveled ~dist period_year v =
  Utils.pair_iter v.periodic period_year (fun period ->
    period.dist_traveled <- period.dist_traveled + dist)

let incr_time_running cur_period v =
  Utils.pair_iter v.periodic cur_period (fun period ->
    period.time_running <- period.time_running + 1)

let add_station loc v =
  {v with station_locs=loc::v.station_locs}

let remove_station loc v =
  let station_locs = List.filter (fun loc2 -> not @@ Utils.equal_loc loc loc2) v.station_locs in
  [%up {v with station_locs}]

let has_bond (v:t) = Money.(v.m.bonds > Money.zero)

let get_interest_rate params v =
  let bond_val = match params.Params.region with
    | Region.WestUS -> Money.of_int 1000 (* Special treatment *)
    | _ -> Money.of_int 500
  in
  let num_bonds = Money.(v.m.bonds /~ bond_val) in
  num_bonds - (Climate.to_enum params.climate) + v.m.num_bankruptcies + 6

let check_sell_bond params v =
  let interest_rate = get_interest_rate params v in
  interest_rate < C.max_interest_rate && not v.m.in_receivership
    
let sell_bond params (v:t) =
  if check_sell_bond params v then (
    let open Money in
    let bonds = v.m.bonds + C.bond_value in
    let cash = v.m.cash + C.bond_value in
    let interest_rate = get_interest_rate params v in
    let base_payment = C.bond_value / 100 in
    let interest_increase = base_payment * interest_rate in
    let yearly_interest_payment = v.m.yearly_interest_payment + interest_increase in
    let v = {v with m = {v.m with bonds; cash; yearly_interest_payment}} in
    pay `InterestFees base_payment v
  ) else v

let check_repay_bond (v:t) =
  let open Money in
  let has_bond = v.m.bonds > zero in
  let has_cash = v.m.cash > C.bond_value in
  has_bond && (has_cash || not v.m.in_receivership)

let repay_bond (v:t) =
  if check_repay_bond v then
    let open Money in
    let num_bonds = v.m.bonds /~ C.bond_value in
    let interest_saving = v.m.yearly_interest_payment / num_bonds in
    let yearly_interest_payment = v.m.yearly_interest_payment - interest_saving in
    let bonds = v.m.bonds - C.bond_value in
    let v = pay `InterestFees C.bond_value v in
    (* Get rid of bankruptcy if needed *)
    let in_receivership = if bonds = Money.zero then false else v.m.in_receivership in
    {v with m = {v.m with bonds; yearly_interest_payment; in_receivership}}
  else v

let check_bankruptcy (v:t) =
  let open Money in
  not v.m.in_receivership &&
  v.m.bonds > C.min_bonds_for_bankruptcy &&
  v.m.cash < C.max_cash_for_bankruptcy 

let set_bankrupt (params:Params.t) v =  
  let bonds = Money.(((v.m.bonds + of_int 500) / 1000) * 500) in  (* bonds / 2 rounded up *)
  let yearly_interest_payment =
    Money.(v.m.yearly_interest_payment * (B_options.difficulty_to_enum params.options.difficulty) / 4)
  in
  let num_bankruptcies = v.m.num_bankruptcies + 1 in
  let v = {v with m =
    {v.m with bonds; yearly_interest_payment; in_receivership=true; num_bankruptcies}}
  in v

let has_broker_timer player = Option.is_some player.broker_timer

let incr_broker_timer player =
  let broker_timer, msg = match player.broker_timer with
    | None -> Some 0, false
    | Some i when i = 3 -> None, true
    | Some i -> Some (i + 1), false
  in
  {player with broker_timer}, msg

let set_priority priority player = {player with priority}

let has_priority player = Option.is_some player.priority

let get_priority player = player.priority

let check_cancel_priority_shipment params v =
  (* Priority shipments are cancelled when the bonus is < 20 *)
  Option.map_or ~default:false
    (fun pr_data -> Priority_shipment.should_be_cancelled pr_data params)
    v.priority

let check_priority_delivery stations v =
  (* Return whether a priority delivery has been fulfilled *)
  Option.map_or ~default:false
    (fun pr_data -> Priority_shipment.check_priority_delivery pr_data stations)
    v.priority

let get_total_ton_miles v =
  (Utils.read_pair v.periodic `First).ton_miles + (Utils.read_pair v.periodic `Second).ton_miles

let add_freight_ton_miles ftm cur_period v =
  let periodic =
    Utils.update_pair v.periodic cur_period (fun period ->
      let freight_ton_miles = Freight.Map.merge_add period.freight_ton_miles ftm in
      let ton_miles = period.ton_miles + Freight.Map.sum (fun _ x -> x) ftm in
      {period with freight_ton_miles; ton_miles})
  in
  {v with periodic}

let add_goods_delivered gd v =
  let goods_delivered = Goods.Set.union gd v.goods_delivered in
  [%up {v with goods_delivered}]

let add_goods_picked_up gd v =
  let goods_picked_up = Goods.Set.union gd v.goods_picked_up in
  [%up {v with goods_picked_up}]

let set_active_station active_station v =
  Log.debug (fun f -> f "Active station set to %s" @@ Utils.show_loc active_station);
  {v with active_station=Some active_station}

let _calc_base_length_track_land_expense x y ~len ~dir ~climate map =
  let base_length = if Dir.is_diagonal dir then 3 else 2 in
  (* includes climate, for one piece of track *)
  let track_expense = (base_length * 2 * ((Climate.to_enum climate) + 4)) / 4 |> Money.of_int in
  let land_expense = Tilemap.track_land_expense map ~track_expense ~x ~y ~dir ~len in
  base_length, track_expense, land_expense

let _add_track x y ~len ~dir v =
  Iter.fold (fun ((x, y) as loc) _ ->
    Vector.push v.track loc;
    Dir.adjust dir x y)
  (x, y)
  Iter.(0 -- (len - 1))
  |> ignore

let update_and_pay_for_track x y ~len ~dir ~climate map v =
  let base_length, track_expense, land_expense = _calc_base_length_track_land_expense x y ~len ~dir ~climate map in
  (* TODO: check Europe scale here *)
  let track_length = v.track_length + len * base_length in
  let () = _add_track x y ~len ~dir v in
  {v with track_length}
  |> pay `Track (Money.(track_expense * len))
  |> pay `RightOfWay land_expense

let _remove_track x y ~len ~dir v =
  (* Inefficient, but how often do we delete track? *)
  Iter.fold (fun ((x, y) as loc) _ ->
    begin match Vector.find_idx (fun loc2 -> Utils.equal_loc loc loc2) v.track with
    | Some idx -> Vector.remove_and_shift v.track idx
    | _ -> ()
    end;
    Dir.adjust dir x y)
  (x, y)
  Iter.(0 -- (len - 1))
  |> ignore

let update_and_remove_track x y ~len ~dir ~climate map v =
  (* This is the proper way to remove track. Effectively sells land *)
  let base_length, _, land_revenue = _calc_base_length_track_land_expense x y ~len ~dir ~climate map in
  let track_length = v.track_length - len * base_length in
  _remove_track x y ~len ~dir v;
  (* TODO: deal with messing up history and achievements for track *)
  {v with track_length}
  |> earn `Other land_revenue

let track_pieces v = Vector.length v.track

let get_track_loc i v = Vector.get v.track i

  (* The original code only selects random spots up to 1250.
     I guess they assumed you'll never have more than this much track? *)
let track_maintenance_random_spot trackmap random v =
  let maintain_roll = Random.int C.maintain_max_roll random in
  let len = Vector.length v.track in
  (* TODO: fix this. For > max, need more rolls *)
  (* We translate to 2 rolls, so all track counts for maintenance. But we keep the odds based on the
     1250 number.
     *)
  if maintain_roll < len then (
    let i = Random.int len random in
    let loc = Vector.get v.track i in
    let expense = match Trackmap.get loc trackmap with
      | Some track ->
         let cost = if Track.acts_like_double track then C.track_maintenance_double else C.track_maintenance_single in
         Dir.Set.fold (fun acc _ -> Money.(acc + cost)) Money.zero track.dirs
      | _ -> failwith "Trackmap incorrect behavior"
    in
    pay `TrackMaintenance expense v
  ) else v

let pay_station_maintenance station_map v =
  let expense =
    List.sum_cash (fun loc ->
      let station = Station_map.get_exn loc station_map in
      Station.maintenance_of station
    ) v.station_locs
  in
  pay `StationMaintenance expense v

let pay_train_maintenance v =
  let expense = Trainmap.total_maintenance v.trains in
  pay `TrainMaintenance expense v

let handle_bridge_washout tracks params random v =
  let age = params.Params.year - params.year_start in
  match v.event with
  | Some(BridgeWashout loc) ->
      {v with event=None}, [Ui_msg.BridgeWashout{player_idx=v.idx; loc; fixed=true}]
  | None when age < 5 || B_options.investor params.options -> v, []
  | _ ->
    let rec loop i =
      if i <= 0 then None else
      let roll = Random.int C.washout_max_roll random in
      let len = Vector.length v.track in
      if roll < len then
        (* TODO: need multiple rolls if more track than max *)
        let loc = Vector.random random v.track in
        let roll = Random.int C.bridge_extra_roll random in
        let track = Trackmap.get_exn loc tracks in
        match Track.get_kind track with
        | Bridge Wood -> Some loc
        (* Note: in the original code, some iron bridges were just cursed because they used the same roll *)
        | Bridge Iron when roll mod C.iron_bridge_fail_odds = 0 -> Some loc
        (* Note: according to the manual, strone bridges should very rarely wash out, but it's not implemented *)
        | Bridge Stone when roll mod C.stone_bridge_fail_odds = 0 -> Some loc
        | _ -> None
      else loop (i - 1)
    in
    match loop C.bridge_washout_tries with
    | None -> v, []
    | Some loc ->
      {v with event=Some(BridgeWashout loc)}, [Ui_msg.BridgeWashout{player_idx=v.idx; loc; fixed=false}]

let update v idx f =
  let p = Owner.Map.find idx v in
  let p' = f p in
  if p =!= p' then
    Owner.Map.add idx p' v
  else v

let get idx v = Owner.Map.find idx v

let set idx player v =
  Owner.Map.add idx player v

let pay_yearly_interest v =
  let open Money in
  let v = pay `InterestFees v.m.yearly_interest_payment v in
  if v.m.cash < zero then
    pay `InterestFees (neg v.m.cash / 8) v
  else v

let add_to_total_difficulty params v =
  (* We dynamically keep track of difficulty *)
  let options = params.Params.options in
  let diff = v.total_difficulty in
  let difficulty = B_options.difficulty_enum options in
  let diff = difficulty * 4 + 2 + diff in
  let increase diff = diff + difficulty / 2 + 1 in
  let diff = if B_options.dispatcher_ops options then increase diff else diff in
  let diff = if B_options.complex_economy options then increase diff else diff in
  let diff = if B_options.cutthroat options then increase diff else diff in 
  {v with total_difficulty=diff}

let get_total_difficulty v = v.total_difficulty

let get_bridge_washout v = match v.event with
  | Some(BridgeWashout loc) -> Some loc
  | _ -> None

let check_new_speed_record speed v = speed > (v.record.train_speed + 1) * 2

let check_update_speed_record speed v =
  if check_new_speed_record speed v then
    {v with record={v.record with train_speed=speed}}
  else v

let _retirement_bonus_and_job ~fired stocks params v =
  let player_idx = v.idx in
  let owned_ais = Stock_market.other_companies_controlled_by player_idx stocks |> List.length in
  let age = Params.age params |> Utils.clip ~min:1 ~max:999 in
  let net_worth = get_net_worth v in
  let difficulty_factor = (get_total_difficulty v * 10 / age) / 2 in
  let modify_by_owned_ais value =
    let mult_val = Int.shift_left 1 (owned_ais - 1) in
    M.(value + (value / C.max_num_players) * mult_val)
  in
  let job_idx, retirement_bonus =
    if M.(net_worth < of_int 100) then
      let job_idx = M.(net_worth / 20) |> Utils.clip_cash ~min:0 ~max:4 |> M.to_int in
      let retirement_bonus = modify_by_owned_ais net_worth in
      job_idx, retirement_bonus
    else
      let retirement_bonus = M.((net_worth / Int.(age + 20)) * difficulty_factor) in
      let retirement_bonus = modify_by_owned_ais retirement_bonus in
      (* fired penalty *)
      let retirement_bonus = if fired then M.(retirement_bonus * 3 / 4) else retirement_bonus in
      let rec loop value i =
        let value = (value / 4) * 3 in
        if value > 200 && i < Jobs.max then
          loop value (i + 1)
        else i
      in 
      let job_idx =
        if M.to_int retirement_bonus >= 10000 then Jobs.max
        else loop (M.to_int retirement_bonus) (4 + 1)
      in
      job_idx, retirement_bonus
  in
  let job = Jobs.of_enum params.region job_idx in
  job, retirement_bonus

let update_retirement_bonus_and_job ~fired stocks params v =
  let job, _retirement_bonus = _retirement_bonus_and_job ~fired stocks params v in
  let ret () = Some job, {v with record={v.record with job=Some job}} in
  match v.record.job with
  | Some cur_job when Jobs.to_enum job > Jobs.to_enum cur_job -> ret ()
  | None -> ret ()
  | _ -> None, v

let get_achievements v = v.achievements

let get_track_pieces_history v = v.history.track_pieces

