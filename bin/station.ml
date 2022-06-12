open Containers

type kind =
  [
  | `SignalTower
  | `Depot
  | `Station
  | `Terminal
  ]
  [@@deriving eq, hash, enum]

let range_of v = match v with
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
  city: string;
  info: info option;
  player: int;
}

let get_upgrades v = match v.info with
  | Some {upgrades;_} -> upgrades
  | None -> Upgrades.empty

let make ~x ~y ~year ~city ~kind ~player =
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
  { x; y; year; city; info; player}

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

