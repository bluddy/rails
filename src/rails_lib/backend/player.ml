(* A human player and his related data *)

open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module C = Constants
module List = Utils.List
open! Utils.Infix

let src = Logs.Src.create "player" ~doc:"Player"
module Log = (val Logs.src_log src: Logs.LOG)

module Vector = Utils.Vector
module U = Utils

type monetary = {
  cash: Money.t; (* all x1000 *)
  bonds: Money.t;
  stockholders_equity : Money.t; (* not sure how this changes *)
  owned_industry: Money.t;
  yearly_interest_payment: Money.t;
  net_worth: Money.t;
  net_worth_last_period: Money.t;
  earnings_record: Money.t; (* Earnings is delta in net worth *)
  earnings_history: Money.t list; (* reversed list *)
  in_receivership: bool; (* bankruptcy *)
  income_statement: Income_statement_d.t;
  total_income_statement: Income_statement_d.t;
  last_balance_sheet: Balance_sheet_d.t;
  num_bankruptcies: int;
  net_worth_history: Money.t list;
  total_revenue_record: Money.t;
  total_revenue_history: Money.t list;
} [@@deriving yojson]

let default_monetary = {
    cash = Money.of_int 1000;
    bonds = Money.of_int 500;
    stockholders_equity = Money.of_int @@ -500;
    owned_industry = Money.zero;
    yearly_interest_payment = Money.of_int 20;
    net_worth = Money.of_int 500; 
    net_worth_last_period = Money.of_int 500; 
    earnings_record = Money.zero;
    earnings_history = [];
    in_receivership = false;
    income_statement = Income_statement_d.default;
    total_income_statement = Income_statement_d.default;
    last_balance_sheet = Balance_sheet_d.default;
    num_bankruptcies = 0;
    net_worth_history=[];
    total_revenue_record=Money.zero;
    total_revenue_history=[];
}

type event =
  BridgeWashout of U.loc
  [@@deriving yojson]

type periodic = {
  mutable dist_traveled: int;
  mutable time_running: int;  (* TODO: update this in train_update *)
  ton_miles: int; (* goods delievered per mile per period *)
  freight_ton_miles: int Freight.Map.t; (* per period *)
} [@@deriving yojson]

type t = {
  idx: Owner.t;
  name: string option; (* custom name for railroad *)
  mutable trains: Trainmap.t;
  station_locs: Utils.loc list; (* Stations ordered by creation order *)
  m: monetary;
  track_length: int; (* track length according to the game (not per tile) *)
  track: Utils.loc Vector.vector; (* vector of track owned by player *)
  goods_delivered: Goods.Set.t;  (* goods delivered so far (for newness) *)
  broker_timer: int option;  (* Time left to see broker, if any *)
  priority: Priority_shipment.t option;
   (* A current active station, which causes high development *)
  mutable active_station: Utils.loc option;
  event : event option;
  periodic: periodic * periodic;
  avg_speed_history: int list; (* reversed *)
  avg_speed_record: int;
  ton_mile_history: int list;
  ton_mile_record: int;
  total_difficulty: int;  (* dynamic tracker of difficulty over time *)
} [@@deriving yojson]

let make_periodic () = {
  dist_traveled=0;
  time_running=0;
  ton_miles=0;
  freight_ton_miles=Freight.Map.empty;
}

let default idx =
  {
    idx;
    name=None;
    station_locs = [];
    m = default_monetary;
    trains = Trainmap.empty ();
    track_length = 0;
    track = Vector.create ();
    goods_delivered=Goods.Set.empty;
    broker_timer=None;
    priority=None;
    active_station=None;
    event=None;
    periodic=(make_periodic (), make_periodic ());
    avg_speed_history=[];
    avg_speed_record=0;
    ton_mile_history=[];
    ton_mile_record=0;
    total_difficulty=0;
  }

let get_cash v = v.m.cash

let net_worth v = v.m.net_worth

let bonds v = v.m.bonds

let modify_cash f v = {v with m={v.m with cash = f v.m.cash}}

let add_cash x v = modify_cash (fun cash -> Money.(cash + x)) v

let in_receivership v = v.m.in_receivership

let get_trains v = v.trains

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

let fiscal_period_end stations params v =
  (* Messages and housecleaning *)
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
  let total_revenue = Income_statement.total_revenue v.m.income_statement in
  let total_income_statement = Income_statement.merge v.m.total_income_statement v.m.income_statement in
  let ui_msgs =
    if Money.(total_revenue / 2 < v.m.yearly_interest_payment && v.m.bonds > Money.of_int 2000) then
      (Ui_msg.ConsiderBankruptcy)::ui_msgs else ui_msgs
  in
  let earnings = Money.((v.m.net_worth - v.m.net_worth_last_period) * 10) in
  let earnings_history = earnings::v.m.earnings_history in
  let earnings_record, ui_msgs =
    if Money.(earnings > v.m.earnings_record) then
      earnings, Ui_msg.RecordEarnings(earnings)::ui_msgs
    else
      v.m.earnings_record, ui_msgs
  in
  let income_statement = Income_statement.default in
  let total_time = (10 + Pair.fold (fun p1 p2 -> p1.time_running + p2.time_running) v.periodic) / 10 in
  let total_dist = 6 * (Pair.fold (fun p1 p2 -> p1.dist_traveled + p2.dist_traveled) v.periodic) in
  let avg_speed = total_dist / total_time in
  let avg_speed_history = (avg_speed / 2)::v.avg_speed_history in
  let avg_speed_record, ui_msgs =
    if avg_speed > v.avg_speed_record then
      avg_speed, Ui_msg.AvgSpeedRecord(avg_speed)::ui_msgs
    else
      v.avg_speed_record, ui_msgs
  in
  let ton_miles = (Utils.read_pair v.periodic current_period).ton_miles in
  let ton_mile_history = ton_miles::v.ton_mile_history in
  let ton_mile_record, ui_msgs =
    if ton_miles > v.ton_mile_record then
      ton_miles, Ui_msg.TonMileRecord(ton_miles)::ui_msgs
    else
      v.ton_mile_record, ui_msgs
  in
  let total_revenue_history = total_revenue::v.m.total_revenue_history in
  let total_revenue_record, ui_msgs =
    if Money.(total_revenue > v.m.total_revenue_record) then
      total_revenue, Ui_msg.RevenueRecord(total_revenue)::ui_msgs
    else
      v.m.total_revenue_record, ui_msgs
  in
  let net_worth_last_period = v.m.net_worth in
  let m = {
    v.m with net_worth_last_period; earnings_record; earnings_history; income_statement;
    total_income_statement; total_revenue_record; total_revenue_history } in
  let v =
    {v with m; avg_speed_record; avg_speed_history; ton_mile_record; ton_mile_history}
  in
  (* TODO: change current period in backend *)
  v, Ui_msg.FiscalPeriodEndMsgs(v.idx, ui_msgs)

let build_industry cost (v:t) =
  let v = pay `StructuresEquipment cost v in
  let owned_industry = Money.(v.m.owned_industry + cost) in
  {v with m={v.m with owned_industry}}

let get_name station_map cities v = match v.name with
  | Some name -> name
  | _ -> 
    (* Getting the name if it's not custom is... complicated *)
    let first_two_proper_station_cities =
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
    in
    match first_two_proper_station_cities with
    | x::y::_ -> Printf.sprintf "%s & %s RR" y x
    | _ -> "RR"

  (* "Game" reported track lenght, not track pieces *)
let track_length v = v.track_length

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

let add_freight_ton_miles ftm cur_period v =
  let periodic =
    Utils.update_pair v.periodic cur_period (fun period ->
      let freight_ton_miles = Freight.Map.merge_add period.freight_ton_miles ftm in
      {period with freight_ton_miles})
  in
  {v with periodic}

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
  let track_length = v.track_length + len * base_length in
  let () = _add_track x y ~len ~dir v in
  {v with track_length}
  |> pay `Track (Money.(track_expense * len))
  |> pay `RightOfWay land_expense

let _remove_track x y ~len ~dir v =
  (* Inefficient, but how often do we delete track? *)
  Iter.fold (fun ((x, y) as loc) _ ->
    begin match Vector.find_idx (fun loc2 -> Utils.equal_loc loc loc2) v.track with
    | Some idx -> Vector.remove_unordered v.track idx
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

let get_bridge_washout v = match v.event with
  | Some(BridgeWashout loc) -> Some loc
  | _ -> None

