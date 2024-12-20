open! Containers

  type t =
  | Add500Cash
  | CreatePriorityShipment
  | CancelPriorityShipment
  [@@deriving show, yojson]
