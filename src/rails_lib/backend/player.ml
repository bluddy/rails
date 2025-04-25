open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module C = Constants
module List = Utils.List
open! Utils.Infix

let src = Logs.Src.create "player" ~doc:"Player"
module Log = (val Logs.src_log src: Logs.LOG)

module Vector = Utils.Vector

type monetary = {
  cash: int; (* all x1000 *)
  bonds: int; (* money *)
  stock: Stocks.t;
  stockholders_equity : int; (* not sure how this changes *)
  owned_industry: int;
  yearly_interest_payment: int;
  net_worth: int;
  in_receivership: bool; (* bankruptcy *)
  income_statement: Income_statement_d.t;
  total_income_statement: Income_statement_d.t;
  last_balance_sheet: Balance_sheet_d.t;
  num_bankruptcies: int;
} [@@deriving yojson]

let default_monetary ~player difficulty =
  {
    cash = 1000;
    bonds = 500;
    stockholders_equity = -500;
    stock = Stocks.default_for_player ~player difficulty;
    owned_industry = 0;
    yearly_interest_payment = 20;
    net_worth = 500;
    in_receivership = false;
    income_statement = Income_statement_d.default;
    total_income_statement = Income_statement_d.default;
    last_balance_sheet = Balance_sheet_d.default;
    num_bankruptcies = 0;
}

type ai_info = {
  opponent: Opponent.t;
  build_order: (Utils.loc * Utils.loc) option;  (* order given to subservient company *)
  yearly_income: int; (* rough estimation of 8 * yearly income *)
} [@@deriving yojson]

type t = {
  name: string option; (* custom name for railroad *)
  mutable trains: Trainmap.t;
  stations: Utils.loc list; (* Stations ordered by creation order *)
  m: monetary;
  track_length: int; (* track length according to the game (not per tile) *)
  track: Utils.loc Vector.vector; (* vector of track owned by player *)
  mutable dist_traveled: int;
  ton_miles: int * int; (* goods delievered per mile per period *)
  freight_ton_miles: int Freight.Map.t * int Freight.Map.t; (* per period *)
  goods_delivered: Goods.Set.t;  (* goods delivered so far (for newness) *)
  ai: ai_info option;  (* Used by AIs only *)
  broker_timer: int option;  (* Time left to see broker, if any *)
  priority: Priority_shipment.t option;
   (* A current active station, which causes high development *)
  mutable active_station: Utils.loc option;
} [@@deriving yojson]

let default ~player difficulty =
  let trains = Trainmap.empty () in
  {
    name=None;
    stations = [];
    m = default_monetary ~player difficulty;
    trains;
    track_length = 0;
    track=Vector.create ();
    dist_traveled=0;
    ton_miles=(0, 0);
    freight_ton_miles=(Freight.Map.empty, Freight.Map.empty);
    goods_delivered=Goods.Set.empty;
    ai=None;
    broker_timer=None;
    priority=None;
    active_station=None;
  }

let get_cash v = v.m.cash

let bonds v = v.m.bonds

let modify_cash v f = {v with m={v.m with cash = f v.m.cash}}

let get_total_shares v = Stocks.total_shares v.m.stock

let in_receivership v = v.m.in_receivership

let pay expense money (v:t) =
  let income_statement = Income_statement.deduct expense money v.m.income_statement in
  let cash = v.m.cash - money in
  {v with m = {v.m with cash; income_statement}}

let earn revenue money (v:t) =
  let income_statement = Income_statement.add_revenue revenue money v.m.income_statement in
  let cash = v.m.cash + money in
  {v with m = {v.m with cash; income_statement}}

let add_income_stmt income_stmt (v:t) =
  let income_statement = Income_statement.merge v.m.income_statement income_stmt in
  let cash = v.m.cash + Income_statement.total income_stmt in
  {v with m={v.m with income_statement; cash}}

let build_industry cost (v:t) =
  let v = pay `StructuresEquipment cost v in
  let owned_industry = v.m.owned_industry + cost in
  {v with m={v.m with owned_industry}}

let get_name v station_map cities = match v.name with
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
            let city, _ = Cities.find_exn cities x y in
            city::acc
          | _ -> acc)
      v.stations
      []
    in
    match first_two_proper_station_cities with
    | x::y::_ -> Printf.sprintf "%s & %s RR" y x
    | _ -> "RR"

  (* "Game" reported track lenght, not track pieces *)
let track_length v = v.track_length

let build_order v = match v.ai with
  | Some x -> x.build_order
  | _ -> None

let incr_dist_traveled ~dist v =
  v.dist_traveled <- v.dist_traveled + dist;
  v

let add_station loc v =
  {v with stations=loc::v.stations}

let remove_station loc v =
  let stations = List.filter (fun loc2 -> not @@ Utils.equal_loc loc loc2) v.stations in
  [%up {v with stations}]

let has_bond (v:t) = v.m.bonds > 0

let get_interest_rate v climate region =
  let bond_val = match region with
    | Region.WestUS -> 1000 (* Special treatment *)
    | _ -> 500
  in
  let num_bonds = v.m.bonds / bond_val in
  num_bonds - (Climate.to_enum climate) + v.m.num_bankruptcies + 6

let check_sell_bond v climate region =
  let interest_rate = get_interest_rate v climate region in
  interest_rate < C.max_interest_rate && not v.m.in_receivership
    

let sell_bond (v:t) climate region =
  if check_sell_bond v climate region then (
    let bonds = v.m.bonds + C.bond_value in
    let cash = v.m.cash + C.bond_value in
    let interest_rate = get_interest_rate v climate region in
    let base_payment = C.bond_value / 100 in
    let interest_increase = base_payment * interest_rate in
    let yearly_interest_payment = v.m.yearly_interest_payment + interest_increase in
    let v = {v with m = {v.m with bonds; cash; yearly_interest_payment}} in
    pay `InterestFees base_payment v
  ) else v

let check_repay_bond (v:t) =
  let has_bond = v.m.bonds > 0 in
  let has_cash = v.m.cash > C.bond_value in
  has_bond && (has_cash || not v.m.in_receivership)

let repay_bond (v:t) =
  if check_repay_bond v then
    let num_bonds = v.m.bonds / C.bond_value in
    let interest_saving = v.m.yearly_interest_payment / num_bonds in
    let yearly_interest_payment = v.m.yearly_interest_payment - interest_saving in
    let bonds = v.m.bonds - C.bond_value in
    let v = pay `InterestFees C.bond_value v in
    (* Get rid of bankruptcy if needed *)
    let in_receivership = if bonds = 0 then false else v.m.in_receivership in
    {v with m = {v.m with bonds; yearly_interest_payment; in_receivership}}
  else v

let check_bankruptcy (v:t) =
  not v.m.in_receivership &&
  v.m.bonds > C.min_bonds_for_bankruptcy &&
  v.m.cash < C.max_cash_for_bankruptcy 

let get_player players player_idx : t = players.(player_idx)
let set players player_idx player = players.(player_idx) <- player

let shares_owned_by_others players ~target_idx ~exclude_idx =
  Array.foldi (fun acc i player ->
    if i = exclude_idx then acc
    else
      acc + Stocks.owned_shares player.m.stock target_idx)
    0
    players

let compute_public_shares players ~player_idx =
  let v = get_player players player_idx in
  let total_shares = v.m.stock.total_shares in
  let owned_shares =
    Array.fold (fun acc player -> acc + Stocks.owned_shares player.m.stock player_idx)
      0 players
  in
  total_shares - owned_shares

let can_buy_stock players ~player_idx ~target_idx ~difficulty =
  let v = get_player players player_idx in
  let tgt_player = get_player players target_idx in
  (* TODO: In original code, it's < total_shares - 10. Not sure why *)
  (* Test if we have an 'anti-trust' problem *)
  let max_owned_companies = Utils.clip (B_options.difficulty_to_enum difficulty) ~min:1 ~max:3 in
  let stock = Stocks.add_shares v.m.stock ~target_idx ~num_shares:C.num_buy_shares in
  if Stocks.num_owned_companies stock > max_owned_companies then
    `Anti_trust_violation max_owned_companies
  else if player_idx <> target_idx &&
       compute_public_shares players ~player_idx = 0 && 
       Stocks.owned_shares v.m.stock target_idx < tgt_player.m.stock.total_shares / 2
    then
      let share_price = tgt_player.m.stock.share_price * 2 in
      let shares_to_buy = shares_owned_by_others players ~target_idx ~exclude_idx:player_idx in
      `Offer_takeover(share_price, shares_to_buy)
  else
    let can_buy = Stocks.owned_shares v.m.stock target_idx < get_total_shares tgt_player in
    let enough_money = v.m.cash >= tgt_player.m.stock.share_price * C.num_buy_shares in
    if enough_money && can_buy then `Ok
  else `Error

let _sell_buy_calculations v target_player ~target_idx ~add_stock ~buy =
  let share_price = target_player.m.stock.share_price in
  let non_treasury_shares = Stocks.non_treasury_shares target_player.m.stock + add_stock in
  let cost = share_price * C.num_buy_shares in
  let price_change =
    let delta = if buy then 1 else 0 in
    (cost / non_treasury_shares) + delta
  in
  let price_change = if buy then price_change else -price_change in
  Log.debug (fun f -> f "price change[%d] non-t-shares[%d] share_price=[%d]" price_change non_treasury_shares share_price);
  let share_price2 = share_price + price_change in
  let num_shares = if buy then C.num_buy_shares else -C.num_buy_shares in
  let stock = Stocks.add_shares v.m.stock ~target_idx ~num_shares in
  let cost = if buy then -cost else cost in
  let cash = v.m.cash + cost in
  cash, stock, cost, share_price2

let set_share_price (v:t) share_price =
  {v with m={v.m with stock={v.m.stock with share_price}}}

let buy_stock players ~player_idx ~target_idx ~difficulty =
  let v = get_player players player_idx in
  let tgt_player = get_player players target_idx in
  match can_buy_stock players ~target_idx ~player_idx ~difficulty with
  | `Ok when player_idx = target_idx ->
    (* add_stock: code adds 10 for ai *)
    let cash, stock, cost, share_price = _sell_buy_calculations v tgt_player ~target_idx ~add_stock:0 ~buy:true in
    let v = {v with m={v.m with cash; stock={stock with share_price}}} in
    players.(player_idx) <- v;
    `Bought(cost)

  | `Ok -> (* different companies *)
    let cash, stock, cost, share_price = _sell_buy_calculations v tgt_player ~target_idx ~add_stock:C.num_buy_shares ~buy:true in
    let v = {v with m={v.m with cash; stock}} in
    players.(player_idx) <- v;
    players.(target_idx) <- set_share_price players.(target_idx) share_price;
    `Bought(cost)

  | `Offer_takeover(share_price, num_shares) when get_cash v >= share_price * num_shares -> (* buy all *)
    (* Buy all stock for one player. Remove from all others *)
    Array.mapi_inplace (fun i player ->
      let stock, cash = if i = player_idx then
        Stocks.add_shares player.m.stock ~target_idx ~num_shares,
        (get_cash player) - (share_price * num_shares)
      else
        let num_shares = Stocks.owned_shares player.m.stock target_idx in
        Stocks.set_shares player.m.stock ~target_idx ~num_shares:0,
        player.m.cash + num_shares * share_price
      in
      {player with m={player.m with stock; cash}}
    ) players;
    `Takeover

  | _ -> `None

let can_sell_stock (v:t) target_idx =   
  Stocks.owned_shares v.m.stock target_idx > 0

let sell_stock (v:t) target_player player_idx ~target_idx =
  match can_sell_stock v target_idx with
  | true when player_idx = target_idx ->
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:0 ~buy:false in
    Some({v with m={v.m with cash; stock={stock with share_price}}}, cost, None)

  | true ->
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:C.num_buy_shares ~buy:false in
    Some({v with m={v.m with cash; stock}}, cost, Some share_price)

  | false -> None

let owns_company player ~company ~company_idx =
  Stocks.owned_shares player.m.stock company_idx > Stocks.total_shares company.m.stock / 2

let owns_company_by_idx (players:t array) ~player_idx ~company_idx =
  let player = players.(player_idx) in
  let company = players.(company_idx) in
  owns_company player ~company ~company_idx

let companies_controlled_by (players:t array) ~player_idx =
  let player = players.(player_idx) in
  Array.foldi (fun acc i company ->
    if i <> player_idx && owns_company player ~company ~company_idx:i
    then i::acc else acc)
  []
  players

let owns_some_company (players: t array) ~player_idx =
  let player = players.(player_idx) in
  Array.foldi (fun acc i company ->
    if i <> player_idx && owns_company player ~company ~company_idx:i
    then acc || true else false)
  false
  players

  (* Compute value of all stocks a player has in all companies, including itself *)
let total_owned_stock_value (players:t array) ~player_idx =
  let player = players.(player_idx) in
  Array.foldi (fun acc i company ->
    acc + ((Stocks.owned_shares player.m.stock i) / 10) * Stocks.share_price company.m.stock)
  0
  players

    (* Update valuation only for AI players *)
let update_ai_valuation players player_idx =
  let player = players.(player_idx) in
  let loans = player.m.bonds / 10 in
  let cash = player.m.cash / 10 in
  let income = match player.ai with
    | Some ai_info -> ai_info.yearly_income
    | _ -> 0
  in
  let stock_value = total_owned_stock_value players ~player_idx in
  let net_worth = cash - loans + income * 2 + stock_value in
  players.(player_idx) <- {player with m={player.m with net_worth}};
  ()

let declare_bankruptcy players player_idx ~difficulty =
  let player = get_player players player_idx in
  let share_price = Stocks.share_price player.m.stock in
  Array.mapi (fun idx v -> 
    if idx = player_idx then
      let bonds = ((v.m.bonds + 500) / 1000) * 500 in  (* bonds / 2 rounded up *)
      let yearly_interest_payment =
        v.m.yearly_interest_payment * (B_options.difficulty_to_enum difficulty) / 4
      in
      let stock = Stocks.set_total_shares v.m.stock 100
        |> Stocks.reset_owned_shares
      in
      let in_receivership = true in
      let num_bankruptcies = v.m.num_bankruptcies + 1 in
      {v with m = {
        v.m with bonds; yearly_interest_payment; stock; in_receivership; num_bankruptcies}
      }
    else
      let sold_stock = (share_price * Stocks.owned_shares v.m.stock player_idx) / 2 in
      let cash = v.m.cash + sold_stock in
      let stock = Stocks.set_shares v.m.stock ~target_idx:player_idx ~num_shares:0 in
      {v with m = {v.m with cash; stock}}
  ) players

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

let check_cancel_priority_shipment player ~cycle ~year region =
  (* Priority shipments are cancelled when the bonus is < 20 *)
  Option.map_or ~default:false
    (fun pr_data -> Priority_shipment.should_be_cancelled pr_data ~cycle ~year region)
    player.priority

let check_priority_delivery player stations =
  (* Return whether a priority delivery has been fulfilled *)
  Option.map_or ~default:false
    (fun pr_data -> Priority_shipment.check_priority_delivery pr_data stations)
    player.priority

let update players idx f =
  let p = players.(idx) in
  let p' = f p in
  if p =!= p' then
    players.(idx) <- p';
  ()

let add_freight_ton_miles ftm fiscal_period v =
  let freight_ton_miles = Utils.update_pair v.freight_ton_miles fiscal_period
    (fun cur_ftm -> Freight.Map.merge_add cur_ftm ftm)
  in
  {v with freight_ton_miles}

let set_active_station active_station v =
  Log.debug (fun f -> f "Active station set to %s" @@ Utils.show_loc active_station);
  {v with active_station=Some active_station}

let _calc_base_length_track_land_expense ~x ~y ~len ~dir ~climate ~map =
  let base_length = if Dir.is_diagonal dir then 3 else 2 in
  (* includes climate, for one piece of track *)
  let track_expense = (base_length * 2 * ((Climate.to_enum climate) + 4)) / 4 in
  let land_expense = Tilemap.track_land_expense map ~track_expense ~x ~y ~dir ~len in
  base_length, track_expense, land_expense

let _add_track x y ~len ~dir v =
  Iter.fold (fun ((x, y) as loc) _ ->
    Vector.push v.track loc;
    Dir.adjust dir x y)
  (x, y)
  Iter.(0 -- (len - 1))
  |> ignore

let update_and_pay_for_track ~x ~y ~len ~dir ~climate ~map v =
  let base_length, track_expense, land_expense = _calc_base_length_track_land_expense ~x ~y ~len ~dir ~climate ~map in
  let track_length = v.track_length + len * base_length in
  let () = _add_track x y ~len ~dir v in
  {v with track_length}
  |> pay `Track (track_expense * len)
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

let update_and_remove_track ~x ~y ~len ~dir ~climate ~map v =
  (* This is the proper way to remove track. Effectively sells land *)
  let base_length, _, land_revenue = _calc_base_length_track_land_expense ~x ~y ~len ~dir ~climate ~map in
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
  (* We translate to 2 rolls, so all track counts for maintenance. But we keep the odds based on the
     1250 number.
     *)
  if maintain_roll < len then (
    let i = Random.int len random in
    let (x, y) = Vector.get v.track i in
    let expense = match Trackmap.get trackmap ~x ~y with
      | Some track ->
         let cost = if Track.acts_like_double track then C.track_maintenance_double else C.track_maintenance_single in
         Dir.Set.fold (fun acc _ -> acc + cost) 0 track.dirs
      | _ -> failwith "Trackmap incorrect behavior"
    in
    pay `TrackMaintenance expense v
  ) else v

let pay_station_maintenance station_map v =
  let expense =
    List.sum (fun loc ->
      let station = Station_map.get_exn loc station_map in
      Station.maintenance_of station
    ) v.stations
  in
  pay `StationMaintenance expense v



