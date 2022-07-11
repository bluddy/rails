open Containers
open Sexplib.Std

(* minimum level to be real demand *)
let min_demand = 64
(* minimum level for mail on simple economy mode *)
let min_demand_mail_simple = 32

type kind =
  [
  | `SignalTower
  | `Depot
  | `Station
  | `Terminal
  ]
  [@@deriving eq, hash, enum, sexp]

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
  | PostOffice
  | Restaurant
  | LivestockPens (* keep it higher so it's drawn last *)
  | Hotel
  [@@deriving enum, show, sexp]

let get_price upgrade =
  let p = match upgrade with
  | MaintenanceShop -> 25
  | EngineShop -> 100 (* ? *)
  | SwitchingYard -> 50
  | ColdStorage -> 25
  | GoodsStorage -> 25
  | PostOffice -> 50
  | Restaurant -> 25
  | LivestockPens -> 25
  | Hotel -> 100
  in
  p * 1000

module Upgrades = Bitset.Make(struct
  type t = upgrade
  let to_enum = upgrade_to_enum
  let of_enum = upgrade_of_enum
  let last = Hotel
end)

module GoodsSet = struct
  include Utils.Set.Make(struct
    type t = Goods.t 
    let compare = Goods.compare
    let sexp_of_t = Goods.sexp_of_t
    let t_of_sexp = Goods.t_of_sexp
  end) [@@deriving_sexp]
end

type info = {
  demand: GoodsSet.t; (* Goods with sufficient demand *)
  min_demand: GoodsSet.t; (* Minimally accepted goods *)
  supply: (Goods.t, int) Hashtbl.t;
  lost_supply: (Goods.t, int) Hashtbl.t;
  kind: [`Depot | `Station | `Terminal];
  upgrades: Upgrades.t;
  rate_war: bool;
} [@@deriving sexp]

type t = {
  x: int;
  y: int;
  year: int;
  name: string;
  info: info option;
  player: int;
} [@@deriving sexp]

let kind_str v =
  match v.info with
  | None -> "Signal Tower"
  | Some {kind;_} -> show_kind kind

let get_upgrades v = match v.info with
  | Some {upgrades;_} -> upgrades
  | None -> Upgrades.empty

let make ~x ~y ~year ~name ~kind ~player =
  let info =
    match kind with
    | `SignalTower -> None
    | `Depot | `Station | `Terminal as k ->
      {
        demand=GoodsSet.empty;
        min_demand=GoodsSet.empty;
        supply=Hashtbl.create 10;
        lost_supply=Hashtbl.create 10;
        kind=k;
        upgrades=Upgrades.empty;
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
let rate_war_lose_supplies info ~difficulty =
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

  (* Call periodically per station *)
let update_supply_demand v tilemap ~difficulty ~climate ~complex_economy =
  let mult = 4 + Climate.to_enum climate in
  let modify_amount amount =
    amount * mult / 12
  in
  match v.info with
  | None -> ()
  | Some info ->
    (* Check for rate war on this station *)
    if info.rate_war then
      rate_war_lose_supplies info ~difficulty;

    let temp_demand_h, temp_supply_h =
      let range = to_range info.kind in
      Tilemap.collect_demand_supply tilemap ~x:v.x ~y:v.y ~range
    in
    (* Add supply to station *)
    Hashtbl.iter (fun good amount ->
      CCHashtbl.incr ~by:(modify_amount amount) info.supply good
    )
    temp_supply_h;

    if not complex_economy then (
      (* All other demand is 2x mail *)
      Hashtbl.filter_map_inplace (fun good amount ->
        match good with
        | Goods.Mail | Passengers -> Some amount
        | _ -> Some (Hashtbl.find temp_demand_h Goods.Mail * 2))
      temp_demand_h;
    );





  
