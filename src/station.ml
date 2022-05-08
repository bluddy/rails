open Containers

type t =
  | SignalTower
  | Depot
  | Station
  | Terminal
  [@@deriving eq, hash, enum]

let range_of (v:t) = match v with
  | SignalTower -> 0
  | Depot -> 1
  | Station -> 2
  | Terminal -> 3
