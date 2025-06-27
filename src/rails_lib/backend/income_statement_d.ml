open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl

type expense = [
  | `RightOfWay
  | `Track
  | `BridgeTunnel
  | `StructuresEquipment
  | `Train
  | `InterestFees
  | `TrainMaintenance
  | `TrackMaintenance
  | `StationMaintenance
  ] [@@deriving enum, ord, yojson, eq]

let show_expense = function
  | `RightOfWay -> "Right-of-Way"
  | `Track -> "Track"
  | `BridgeTunnel -> "Bridges/Tunnels"
  | `StructuresEquipment -> "Structures/Equipment"
  | `Train -> "Trains"
  | `InterestFees -> "Interest/Fees"
  | `TrainMaintenance -> "Train Maintenance"
  | `TrackMaintenance -> "Track Maintenance"
  | `StationMaintenance -> "Station Maintenance"

module ExpenseMap = Utils.Map.Make(struct
    type t = expense [@@deriving yojson]
    let compare = compare_expense
  end)

type revenue = [
  | `Mail
  | `Passenger
  | `Fast
  | `Slow
  | `Bulk
  | `Other
  ] [@@deriving enum, ord, yojson]

let show_revenue = function
  | `Mail -> "Mail"
  | `Passenger -> "Passengers"
  | `Fast -> "Fast Freight"
  | `Slow -> "Slow Freight"
  | `Bulk -> "Bulk Freight"
  | `Other -> "Other Income"

module RevenueMap = struct
  include Utils.Map.Make(struct
    type t = revenue [@@deriving yojson]
    let compare = compare_revenue
  end)

  let of_goods ~merge goods =
    Goods.Map.to_iter goods
    |> Iter.map (fun (good, x) -> Freight.of_good good, x)
    |> of_iter_merge ~merge
end


type t = {
  expenses: Money.t ExpenseMap.t;
  revenues: Money.t RevenueMap.t;
} [@@deriving yojson]

let default = {
  expenses=ExpenseMap.empty;
  revenues=RevenueMap.empty;
}

