open Containers
open Utils.Infix
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module C = Constants

(* minimum level to be real demand *)
let min_demand = C.car_full_demand
(* minimum level for mail on simple economy mode *)
let min_demand_mail_simple = min_demand / 2
let max_supply_with_upgrade = min_demand * 20

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

let price_of = function
  | `SignalTower -> 25
  | `Depot -> 50
  | `Station -> 100
  | `Terminal -> 200

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

let price_of_upgrade upgrade =
  let p = match upgrade with
  | MaintenanceShop -> 25
  | EngineShop -> 100 (* ? *)
  | SwitchingYard -> 50
  | ColdStorage -> 25
  | GoodsStorage | ArmsStorage -> 25
  | PostOffice -> 50
  | Restaurant -> 25
  | LivestockPens | GrapeStorage -> 25
  | Hotel -> 100
  in
  p * 1000

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
  city: int * int;
  suffix: suffix option;
  mutable demand: Goods.Set.t; (* sufficient demand *)
  mutable convert_demand: Goods.Set.t; (* minimum for conversion *)
  supply: (Goods.t, int) Hashtbl.t;
  lost_supply: (Goods.t, int) Hashtbl.t;
  kind: [`Depot | `Station | `Terminal];
  upgrades: Upgrades.t;
  rate_war: bool;
  rates: [`Normal | `Double | `Half];
  cargo_revenue: int Goods.Map.t; (* revenue for each cargo type at this station *)
} [@@deriving yojson]

let has_demand_for v good = Goods.Set.mem good v.demand
let convert v good region =
  if Goods.Set.mem good v.convert_demand then
    Goods.convert region good
  else
    None

type signal = 
  | Go (* safe to enter *)
  | Stop (* not safe to enter *)
  | OverrideProceed (* pass next train and then normal *)
  | OverrideHold (* stops all trains *)
  [@@deriving yojson, eq]

type id = int * int [@@deriving yojson, eq, show]

type t = {
  x: int;
  y: int;
  year: int;
  info: info option;
  player: int;
  signals: signal * signal; (* lower then upper *)
} [@@deriving yojson]

let with_info v f = match v.info with
  | Some x -> Some (f x)
  | _ -> None

let get_age v year = year - v.year

let color_of_signal = function
  | Go -> Ega.green
  | Stop -> Ega.red
  | OverrideProceed -> Ega.yellow
  | OverrideHold -> Ega.bred

let frame_color_of_signal = function
  | Go | Stop -> Ega.black
  | _ -> Ega.white

let kind_str v =
  match v.info with
  | None -> "Signal Tower"
  | Some {kind;_} -> show_kind kind

let is_proper_station v =
  match v.info with
  | Some _ -> true
  | None -> false

let get_upgrades v = match v.info with
  | Some {upgrades;_} -> upgrades
  | None -> Upgrades.empty

let has_upgrade v upgrade =
  let upgrades = get_upgrades v in
  Upgrades.mem upgrades upgrade

let can_maintain v =
  has_upgrade v EngineShop || has_upgrade v MaintenanceShop
let can_build_train v = has_upgrade v EngineShop
let has_restaurant v = has_upgrade v Restaurant
let has_hotel v = has_upgrade v Hotel

(*
let get_segment (v:t) dir = match v.segments with
  | x, _ when Dir.lower dir -> x
  | _, x -> x

let set_segment (v:t) dir seg =
  let segments = match v.segments with
    | _, x when Dir.lower dir -> seg, x
    | x, _ -> x, seg
  in
  {v with segments}
*)

let get_signal (v:t) dir = match v.signals with
  | x, _ when Dir.lower dir -> x
  | _, x -> x

let set_signal (v:t) dir signal =
  let signals = match v.signals with
    | _, x when Dir.lower dir -> signal, x
    | x, _ -> x, signal
  in
  {v with signals}

(*
let replace_segment (v:t) seg_old seg_new =
  let segments = match v.segments with
    | seg2, x when Segment.equal_id seg_old seg2 -> seg_new, x
    | x, seg2 when Segment.equal_id seg_old seg2 -> x, seg_new
    | _ -> v.segments
  in
  [%up {v with segments}]

let make_segments_and_signals segments =
  let segments = match segments with
    | [d1, seg1; _, seg2] when Dir.lower d1 -> (seg1, seg2)
    | [_, seg1; _, seg2] -> (seg2, seg1)
    | _ -> failwith "Incorrect number of segments"
  in
  let signals = Auto, Auto in
  segments, signals
  *)

let make_signaltower ~x ~y ~year ~player =
  let signals = Go, Go in
  { x; y; year; info=None; player; signals}

let make ~x ~y ~year ~city_xy ~city_name ~suffix ~kind ~player ~first =
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
        lost_supply=Hashtbl.create 10;
        kind=k;
        name;
        short_name;
        city=city_xy;
        suffix;
        upgrades=if first then Upgrades.singleton EngineShop else Upgrades.empty;
        rate_war=false;
        rates=`Normal;
        cargo_revenue=Goods.Map.empty;
      } |> Option.some
  in
  let signals = Go, Go in
  { x; y; year; info; player; signals}

let add_upgrade v upgrade player =
  if v.player <> player then v else
  let info =
    match v.info with
    | Some ({upgrades;_} as info) ->
        let upgrades = Upgrades.add upgrades upgrade in
        {info with upgrades} |> Option.some
    | None -> None
  in
  {v with info}

let get_name v = match v.info with
  | Some info -> info.name
  | _ -> ""

let get_short_name v = match v.info with
  | Some info -> info.short_name
  | _ -> ""

let get_city v = match v.info with
  | Some info -> Some(info.city)
  | _ -> None

let get_supply_exn v = match v.info with
  | Some info -> info.supply
  | None -> failwith "not a proper station"

let get_demand_exn v = match v.info with
  | Some info -> info.demand
  | None -> failwith "not a proper station"

   (* some supplies are lost every tick in a rate war. *)
let check_rate_war_lose_supplies v ~difficulty =
  match v.info with
  | Some info when info.rate_war ->
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

  (* Call periodically per station -- impure for performance *)
  (* TODO: better to memoize demand/supply calculation and recompute when things change *)
let update_supply_demand v tilemap ~climate ~simple_economy =
  let mult = 4 + Climate.to_enum climate in
  let modify_amount amount =
    amount * mult / 12
  in
  match v.info with
  | None -> []
  | Some info ->
    Printf.printf "Updating demand/supply\n";
    let temp_demand_h, temp_supply_h =
      let range = to_range info.kind in
      Tilemap.collect_demand_supply tilemap ~x:v.x ~y:v.y ~range
    in
    (* Add supply to station *)
    Hashtbl.iter (fun good amount ->
      CCHashtbl.incr ~by:(modify_amount amount) info.supply good
    )
    temp_supply_h;

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
              let freight = Goods.freight_of_goods good in
              let div = Goods.freight_to_enum freight + 2 in
              let lost = (amount2 / div) / 2 in
              amount2 - lost
        in
        Hashtbl.replace info.supply good amount2;
        CCHashtbl.incr info.lost_supply good ~by:(amount - amount2);
      )

  let total_goods_revenue v =
    match v.info with
    | Some info ->
      Goods.Map.fold (fun _ i sum -> sum + i) info.cargo_revenue 0
    | _ -> 0

  let color_of_rates v = match v.info with
    | Some info -> begin match info.rates with
      | `Normal -> Ega.white
      | `Half   -> Ega.bred
      | `Double -> Ega.gray
      end
    | _ -> failwith "Shouldn't get here"
       

