open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl

type expense =
  | RightOfWay
  | Track
  | BridgeTunnel
  | StructuresEquipment
  | Trains
  | InterestFees
  | TrainMaintenance
  | TrackMaintenance
  | StationMaintenance
  [@@deriving enum, ord, yojson, eq]

let show_expense = function
  | RightOfWay -> "Right-of-Way"
  | Track -> "Track"
  | BridgeTunnel -> "Bridges/Tunnels"
  | StructuresEquipment -> "Structures/Equipment"
  | Trains -> "Trains"
  | InterestFees -> "Interest/Fees"
  | TrainMaintenance -> "Train Maintenance"
  | TrackMaintenance -> "Track Maintenance"
  | StationMaintenance -> "Station Maintenance"

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

module RevenueMap = Utils.Map.Make(struct
  type t = revenue [@@deriving yojson]
  let compare = compare_revenue
end)

type t = {
  expenses: int ExpenseMap.t;
  revenues: int RevenueMap.t;
} [@@deriving yojson]

let default = {
  expenses=ExpenseMap.empty;
  revenues=RevenueMap.empty;
}

