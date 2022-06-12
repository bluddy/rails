open Containers

type kind =
  [
  | `SignalTower
  | `Depot
  | `Station
  | `Terminal
  ]
  [@@deriving eq, hash, enum]

let show_kind = function
  | `SignalTower -> "Signal Tower"
  | `Depot -> "Depot"
  | `Station -> "Station"
  | `Terminal -> "Terminal"

let range_of = function
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
  [@@deriving enum, show]

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

type info = {
  demand: (Goods.t, int) Hashtbl.t;
  supply: (Goods.t, int) Hashtbl.t;
  kind: [`Depot | `Station | `Terminal];
  upgrades: Upgrades.t;
}

type t = {
  x: int;
  y: int;
  year: int;
  name: string;
  info: info option;
  player: int;
}

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
        demand=Hashtbl.create 10;
        supply=Hashtbl.create 10;
        kind=k;
        upgrades=Upgrades.empty;
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

module Map = struct
  type nonrec t = {
    map: (int, t) Hashtbl.t;
    width: int;
  }

  let create width = {
    map= Hashtbl.create 10;
    width;
  }

  let iter f v =
    Hashtbl.iter (fun offset station ->
      let x, y = Utils.x_y_of_offset v.width offset in
      f x y station)
    v.map

  let get v x y = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

  let add v x y station =
    Hashtbl.replace v.map (Utils.calc_offset v.width x y) station;
    v

  let delete v x y =
    Hashtbl.remove v.map (Utils.calc_offset v.width x y);
    v

end

