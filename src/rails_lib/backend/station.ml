open Containers
open Utils.Infix
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module C = Constants

let src = Logs.Src.create "station" ~doc:"Station"
module Log = (val Logs.src_log src: Logs.LOG)

(* minimum level to be real demand *)
let min_demand = C.car_full_demand
(* minimum level for mail on simple economy mode *)
let min_demand_mail_simple = min_demand / 2
let max_supply_with_upgrade = min_demand * 20

let moi = Money.of_int

type kind =
  [
  | `SignalTower
  | `Depot
  | `Station
  | `Terminal
  ]
  [@@deriving eq, hash, enum, yojson, show]

let show_kind = function
  | `SignalTower -> "Signal Tower"
  | `Depot -> "Depot"
  | `Station -> "Station"
  | `Terminal -> "Terminal"

let to_range = function
  | `SignalTower -> 0
  | `Depot -> 1
  | `Station -> 2
  | `Terminal -> 3

let price_of ?(union_station=false) = function
  | `SignalTower -> moi 25
  | `Depot -> moi 50
  | `Station -> moi 100
  | `Terminal when union_station -> moi 100
  | `Terminal -> moi 200

let maintenance_of_kind = function
  | `SignalTower -> moi 1
  | `Depot -> moi 2
  | `Station -> moi 3
  | `Terminal -> moi 4


let is_big_station (x:kind) = match x with
  | `SignalTower -> false
  | _ -> true

type upgrade =
  | MaintenanceShop
  | EngineShop (* higher than MaintenanceShop to hide it *)
  | SwitchingYard
  | ColdStorage
  | GoodsStorage
  | ArmsStorage (* EU *)
  | PostOffice
  | Restaurant
  | LivestockPens (* keep it higher so it's drawn last *)
  | GrapeStorage (* EU: also cold *)
  | Hotel
  [@@deriving enum, show, yojson]

let price_of_upgrade = function
  | MaintenanceShop -> moi 25
  | EngineShop -> moi 100 (* ? *)
  | SwitchingYard -> moi 50
  | ColdStorage -> moi 25
  | GoodsStorage | ArmsStorage -> moi 25
  | PostOffice -> moi 50
  | Restaurant -> moi 25
  | LivestockPens | GrapeStorage -> moi 25
  | Hotel -> moi 100

module Upgrades = Bitset.Make(struct
  type t = upgrade [@@deriving yojson]
  let to_enum = upgrade_to_enum
  let of_enum = upgrade_of_enum
  let last = Hotel
end)

type suffix =
  | Junction
  | Crossing
  | Central
  | Annex
  | Transfer
  | Valley
  | Hills
  | Woods
  [@@deriving yojson, enum]

let show_suffix = function
  | Junction -> "Junction"
  | Crossing -> "Crossing"
  | Central -> "Central"
  | Annex -> "Annex"
  | Transfer -> "Transfer"
  | Valley -> "Valley"
  | Hills -> "Hills"
  | Woods -> "Woods"

let num_suffix = (suffix_to_enum Woods) + 1

type info = {
  name: string;
  short_name: string;
  city: Utils.loc;
  suffix: suffix option;
  mutable demand: Goods.Set.t; (* sufficient demand *)
  mutable convert_demand: Goods.Set.t; (* minimum for conversion *)
  supply: (Goods.t, int) Hashtbl.t;
  lost_supply: (Goods.t, int) Hashtbl.t;
  picked_up_goods: (Goods.t, int) Hashtbl.t;
  kind: [`Depot | `Station | `Terminal];
  upgrades: Upgrades.t;
  rates: [`Normal | `Double | `Half];
  cargo_revenue: Money.t Goods.Map.t; (* revenue for each cargo type at this station *)
  holds_priority_shipment: bool;
} [@@deriving yojson]

let convert good region info =
  if Goods.Set.mem good info.convert_demand then
    Goods.convert region good
  else
    None

type override =
  | NoOverride (* no override *)
  | OverrideProceed (* pass next train and then normal *)
  | OverrideHold (* stops all trains *)
  [@@deriving yojson, eq]

type signal = 
  | Go (* safe to enter *)
  | Stop (* not safe to enter *)
  [@@deriving yojson, eq]

type id = int * int [@@deriving yojson, eq, show]

type signals = {
  lower: signal * override;
  upper: signal * override;
} [@@deriving yojson]

let default_signals = {
  lower=(Go, NoOverride);
  upper=(Go, NoOverride);
}

type t = {
  loc: Utils.loc;
  year: int;
  info: info option;
  player: Owner.t;
  signals: signals;
} [@@deriving yojson]

let with_info v default f = Option.map_or ~default f v.info

let update_with_info v f = with_info v v @@
  fun info ->
  (* None = don't update *)
    match f info with
    | Some _ as info -> {v with info}
    | None -> v

let get_age v year = year - v.year

let get_year_built v = v.year

let young v year = get_age v year <= C.young_station_age

let color_of_signal = function
  | _, OverrideProceed -> Ega.yellow
  | _, OverrideHold -> Ega.bred
  | Go, NoOverride -> Ega.bgreen
  | Stop, NoOverride -> Ega.red

let frame_color_of_signal = function
  | _, OverrideProceed
  | _, OverrideHold -> Ega.white
  | Go, NoOverride
  | Stop, NoOverride -> Ega.black

let kind_str v = with_info v "Signal Tower" (fun info -> show_kind info.kind)

let is_proper_station v = with_info v false (fun _ -> true)

let get_upgrades v = with_info v Upgrades.empty (fun info -> info.upgrades)

let has_upgrade v upgrade =
  let upgrades = get_upgrades v in
  Upgrades.mem upgrades upgrade

let total_upgrade_value v =
  Upgrades.fold (fun acc upgrade -> Money.(acc + price_of_upgrade upgrade)) Money.zero @@ get_upgrades v

let can_maintain v =
  has_upgrade v EngineShop || has_upgrade v MaintenanceShop
let can_build_train v = has_upgrade v EngineShop
let has_restaurant v = has_upgrade v Restaurant
let has_hotel v = has_upgrade v Hotel

let get_signal (v:t) dir =
  if Dir.is_lower dir then v.signals.lower else v.signals.upper

let set_signal_upper upper signal v =
  let signals = match upper with
    | `Lower ->
      {v.signals with lower=(signal, snd v.signals.lower)}
    | `Upper ->
      {v.signals with upper=(signal, snd v.signals.upper)}
  in
  {v with signals}

let set_signal dir signal v = set_signal_upper (Dir.to_upper dir) signal v

let set_override dir override (v:t) =
  let signals =
    if Dir.is_lower dir then
      {v.signals with lower=(fst v.signals.lower, override)}
    else
      {v.signals with upper=(fst v.signals.upper, override)}
  in
  {v with signals}

let cancel_override dir (v:t) = set_override dir NoOverride v 

let has_override_hold v =
  match snd v.signals.lower, snd v.signals.upper with
  | OverrideHold, _
  | _, OverrideHold -> true
  | _ -> false

  (* When an owned company builds track into our >signaltower station,
     it becomes a union station: effectively a terminal *)
let set_to_union_station v =
  update_with_info v (fun info -> Some {info with kind=`Terminal})

let can_train_go dir (v:t) =
  (* Also returns whether we need to cancel override *)
  let signal = get_signal v dir in
  match signal with
    _, OverrideProceed -> true, `Cancel_override
  | _, OverrideHold -> false, `None
  | Go, _ -> true, `None
  | Stop, _ -> false, `None

let make_signaltower x y ~year player_idx =
  { loc=(x, y); year; info=None; player=player_idx; signals=default_signals}

let make x y ~year ~city_xy ~city_name ~suffix ~kind player_idx ~first =
  let name = match suffix with
    | Some suffix -> city_name^" "^show_suffix suffix
    | None -> city_name
  in
  let short_name = match suffix with
    | Some Crossing ->
        String.take 2 city_name ^ "X"
    | Some suffix ->
        let suffix_s = show_suffix suffix in
        String.take 2 city_name ^ String.take 1 suffix_s
    | None -> String.take 3 city_name
  in
  let info = match kind with
    | `SignalTower -> None
    | `Depot | `Station | `Terminal as k ->
      {
        demand=Goods.Set.empty;
        convert_demand=Goods.Set.empty;
        supply=Hashtbl.create 10;
        picked_up_goods=Hashtbl.create 10;
        lost_supply=Hashtbl.create 10;
        kind=k;
        name;
        short_name;
        city=city_xy;
        suffix;
        upgrades=if first then Upgrades.singleton EngineShop else Upgrades.empty;
        rates=`Normal;
        cargo_revenue=Goods.Map.empty;
        holds_priority_shipment=false;
      } |> Option.some
  in
  let signals = default_signals in
  { loc=(x, y); year; info; player=player_idx; signals}

let has_suffix v = match v.info with Some {suffix=Some _; _} -> true | _ -> false

let add_upgrade upgrade player v =
  if Owner.(v.player <> player) then v else
  let info =
    match v.info with
    | Some ({upgrades;_} as info) ->
        let upgrades = Upgrades.add upgrade upgrades in
        {info with upgrades} |> Option.some
    | None -> None
  in
  {v with info}

let get_name v = with_info v "" (fun info -> info.name)

let get_short_name v = with_info v "" (fun info -> info.short_name)

let get_city v = with_info v None (fun info -> Some info.city)

let get_loc v = v.loc

let get_supply_exn v = match v.info with
  | Some info -> info.supply
  | None -> failwith "not a proper station"

let get_picked_up_goods_exn v = match v.info with
  | Some info -> info.picked_up_goods
  | None -> failwith "not a proper station"

let get_lost_supply_exn v = match v.info with
  | Some info -> info.lost_supply
  | None -> failwith "not a proper station"

let get_demand_exn v = with_info v Goods.Set.empty (fun info -> info.demand)

   (* some supplies are lost every tick in a rate war. *)
let check_rate_war_lose_supplies ~difficulty v =
  match v.info with
  | Some ({rates=`Half; _} as info) ->
      let div = match difficulty with
        | `Investor -> 1
        | `Financier -> 2
        | `Mogul -> 3
        | `Tycoon -> 4
      in
      Hashtbl.filter_map_inplace (fun good amount ->
        let amount_lost = amount / div in
        (* Save lost supply *)
        CCHashtbl.incr ~by:amount_lost info.lost_supply good;
        Some (amount - amount_lost)
      )
      info.supply
  | _ -> ()

let value_of station = match station.info with
  | None -> price_of `SignalTower
  | Some info -> price_of info.kind

let maintenance_of station = match station.info with
  | None -> maintenance_of_kind `SignalTower
  | Some info -> maintenance_of_kind info.kind

  (* Call periodically per station -- impure for performance *)
  (* TODO: see if we can make pure *)
let update_supply_demand tilemap params v =
  let mult = 4 + Climate.to_enum params.Params.climate in
  let modify_amount amount =
    amount * mult / 12
  in
  match v.info with
  | None -> []
  | Some info ->
    Log.debug (fun f -> f "Updating demand/supply");
    let temp_demand_h, temp_supply_h =
      let range = to_range info.kind in
      Tilemap.collect_demand_supply v.loc tilemap ~range
    in
    (* Add supply to station *)
    Hashtbl.iter (fun good amount ->
      CCHashtbl.incr ~by:(modify_amount amount) info.supply good
    )
    temp_supply_h;

    let simple_economy = B_options.simple_economy params.options in

    if simple_economy then (
      (* All other demand is 2x mail *)
      Hashtbl.filter_map_inplace (fun good amount ->
        match good with
        | Goods.Mail | Passengers -> Some amount
        | _ -> Some (Hashtbl.find temp_demand_h Goods.Mail * 2))
      temp_demand_h;
    );

    (* Check if we changed *)
    let demand2, msgs =
      Hashtbl.fold (fun goods amount ((demand, msgs) as old) ->
        let has_demand = match goods with
          | Goods.Mail when simple_economy -> amount >= min_demand_mail_simple
          | _ -> amount >= min_demand
        in
        let had_demand = Goods.Set.mem goods info.demand in
        if has_demand && not had_demand then
          (Goods.Set.add goods demand, (goods, true)::msgs)
        else if not has_demand && had_demand then
          (Goods.Set.remove goods demand, (goods, false)::msgs)
        else old)
      temp_demand_h
      (info.demand, [])
    in
    let msgs = if simple_economy then [] else msgs in
    let convert_demand =
      Hashtbl.fold (fun goods amount demand ->
        let f = if amount > 0 then Goods.Set.add else Goods.Set.remove in
        f goods demand)
      temp_demand_h
      info.demand
    in
    if info.demand =!= demand2 then info.demand <- demand2;
    if info.convert_demand =!= convert_demand then (
      info.convert_demand <- convert_demand;
    );
    msgs

    (** Lose supplies. Less supplies lost if we have the right upgrade *)
let lose_supplies v =
  match v.info with
  | None -> ()
  | Some info ->
      CCHashtbl.keys info.supply (fun good ->
        let amount = Hashtbl.find info.supply good in
        let amount2 = Utils.clip amount ~min:0 ~max:max_supply_with_upgrade in

        let open Goods in
        let amount2 =
          let has x = Upgrades.mem info.upgrades x in
          match good with
          | Mail when has PostOffice  -> amount2
          | Passengers when has Hotel -> amount2
          | Food when has ColdStorage -> amount2
          | Livestock when has LivestockPens -> amount2
          | Grapes when has GrapeStorage -> amount2
          | MfgGoods when has GoodsStorage -> amount2
          | Armaments when has ArmsStorage -> amount2
          | _ -> 
              (* higher freight classes are less time sensitive *)
              let freight = Freight.of_good good in
              let div = Freight.to_enum freight + 2 in
              let lost = (amount2 / div) / 2 in
              amount2 - lost
        in
        Hashtbl.replace info.supply good amount2;
        CCHashtbl.incr info.lost_supply good ~by:(amount - amount2);
      )

let total_goods_revenue v = with_info v Money.zero (fun info ->
  Goods.Map.sum_cash (fun _ cash -> cash) info.cargo_revenue)

let add_to_goods_revenue goods_rev info =
  let cargo_revenue = Goods.Map.merge_add_cash info.cargo_revenue goods_rev in
  {info with cargo_revenue}

let add_to_goods_revenue goods_rev v =
  update_with_info v (fun info -> add_to_goods_revenue goods_rev info |> Option.some)

let get_goods_revenue v = with_info v Goods.Map.empty (fun info -> info.cargo_revenue)

let color_of_rates v = match v.info with
  | Some info -> begin match info.rates with
    | `Normal -> Ega.white
    | `Half   -> Ega.bred
    | `Double -> Ega.gray
    end
  | _ -> failwith "Shouldn't get here"
     
let holds_priority_shipment v = with_info v false @@
  fun info -> info.holds_priority_shipment

let set_priority_shipment x v =
  update_with_info v
  (fun info ->
    if Bool.equal info.holds_priority_shipment x then None
    else Some {info with holds_priority_shipment = x})

let get_player_idx v = v.player

let has_rate_war v = with_info v false (fun info -> match info.rates with `Half -> true | _ -> false)

let has_double_rates v = with_info v false (fun info -> match info.rates with `Double -> true | _ -> false)

let get_rates v = with_info v `Normal (fun info -> info.rates)

let _set_rate_war x v = update_with_info v (fun info -> Some {info with rates=x})

let set_rate_war v = _set_rate_war `Half v

let set_double_rates v = _set_rate_war `Double v

let set_normal_rates v = _set_rate_war `Normal v

let total_picked_up_goods v = with_info v 0
  (fun info ->
    let goods = info.picked_up_goods in
    Hashtbl.sum (fun _ num -> num) goods)
  
let total_lost_supply v = with_info v 0
  (fun info ->
    let supply = info.lost_supply in
    Hashtbl.sum (fun _ num -> num) supply)

let remove_goods goods v = with_info v () @@
  fun info ->
    Goods.Set.iter (fun good ->
      Hashtbl.update info.supply ~k:good ~f:(fun _ -> function _ -> None)
    ) goods

let has_demand_for v good = with_info v false @@ fun info -> Goods.Set.mem good info.demand

let convert good region v = with_info v None @@ fun info -> convert good region info

let end_of_period_reset v = update_with_info v @@ fun info ->
    let rates = if has_double_rates v then `Normal else info.rates in
    let info = {info with
      picked_up_goods=Hashtbl.create 10;
      lost_supply=Hashtbl.create 10;
      cargo_revenue=Goods.Map.empty;
      rates;
    } in
    Some info

