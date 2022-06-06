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
  | EngineShop
  | SwitchingYard
  | MaintenanceShop
  | ColdStorage
  | LivestockPens
  | GoodsStorage
  | PostOffice
  | Restaurant
  | Hotel
  [@@deriving enum]

module Upgrades = Bitset.Make(struct
  type t = upgrade
  let to_enum = upgrade_to_enum
  let of_enum = upgrade_of_enum
  let last = Hotel
end)

type major = {
  demand: Goods.t list;
  supply: Goods.t list;
  kind: [`Depot | `Station | `Terminal];
  upgrades: Upgrades.t;
}

type t = {
  x: int;
  y: int;
  year: int;
  city: string;
  major: major option;
  player: int;
}

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

end

