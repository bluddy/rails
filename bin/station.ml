open Containers

(* minimum level to be real demand *)
let min_demand = Goods.full_car
(* minimum level for mail on simple economy mode *)
let min_demand_mail_simple = Goods.full_car / 2
let max_supply_with_upgrade = Goods.full_car * 20

type kind =
  [
  | `SignalTower
  | `Depot
  | `Station
  | `Terminal
  ]
  [@@deriving eq, hash, enum, yojson]

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

let get_price upgrade =
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
  type t = upgrade
  let to_enum = upgrade_to_enum
  let of_enum = upgrade_of_enum
  let last = Hotel
end)

type info = {
  mutable demand: Goods.Set.t; (* Goods with sufficient demand *)
  mutable min_demand: Goods.Set.t; (* Minimally accepted goods *)
  supply: (Goods.t, int) Utils.Hashtbl.t;
  lost_supply: (Goods.t, int) Utils.Hashtbl.t;
  kind: [`Depot | `Station | `Terminal];
  upgrades: Upgrades.t;
  rate_war: bool;
} [@@deriving yojson]

type t = {
  x: int;
  y: int;
  year: int;
  name: string;
  info: info option;
  player: int;
} [@@deriving yojson]

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

let has_upgrade v ~upgrade =
  let upgrades = get_upgrades v in
  Upgrades.mem upgrades upgrade

let make ~x ~y ~year ~name ~kind ~player ~first =
  let info =
    match kind with
    | `SignalTower -> None
    | `Depot | `Station | `Terminal as k ->
      {
        demand=Goods.Set.empty;
        min_demand=Goods.Set.empty;
        supply=Hashtbl.create 10;
        lost_supply=Hashtbl.create 10;
        kind=k;
        upgrades=if first then Upgrades.singleton EngineShop else Upgrades.empty;
        rate_war=false;
      } |> Option.some
  in
  { x; y; year; name; info; player}

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

let suffixes = [
  "Junction";
  "Crossing";
  "Central";
  "Annex";
  "Transfer";
  "Valley";
  "Hills";
  "Woods";
]

   (* some supplies are lost every tick in a rate war. *)
let check_rate_war_lose_supplies v ~difficulty =
  match v.info with
  | Some info when info.rate_war ->
      let div = match difficulty with
        | `Diff100 -> 1
        | `Diff75 -> 2
        | `Diff50 -> 3
        | `Diff25 -> 4
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
    let min_demand2 =
      Hashtbl.fold (fun goods amount demand ->
        let f = if amount > 0 then Goods.Set.add else Goods.Set.remove in
        f goods demand)
      temp_demand_h
      info.demand
    in
    if not @@ CCEqual.physical info.demand demand2 then info.demand <- demand2;
    if not @@ CCEqual.physical info.min_demand min_demand2 then info.min_demand <- min_demand2;
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
          match good with
          | Mail when Upgrades.mem info.upgrades PostOffice  -> amount2
          | Passengers when Upgrades.mem info.upgrades Hotel -> amount2
          | Food when Upgrades.mem info.upgrades ColdStorage -> amount2
          | Livestock when Upgrades.mem info.upgrades LivestockPens -> amount2
          | Grapes when Upgrades.mem info.upgrades GrapeStorage -> amount2
          | MfgGoods when Upgrades.mem info.upgrades GoodsStorage -> amount2
          | Armaments when Upgrades.mem info.upgrades ArmsStorage -> amount2
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

