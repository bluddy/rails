open! Containers
module C = Constants

type t = [
  | `Mail
  | `Passenger
  | `Fast
  | `Slow
  | `Bulk
  ] [@@deriving enum, ord, yojson, show]

let all_freight : t array = [|`Mail; `Passenger; `Fast; `Slow; `Bulk|]

let to_color freight ~full = match freight, full with
  | `Mail, true -> Ega.white
  | `Mail, false -> Ega.gray
  | `Passenger, true -> Ega.bcyan
  | `Passenger, false -> Ega.cyan
  | `Fast, true -> Ega.yellow
  | `Fast, false -> Ega.bgreen
  | `Slow, true -> Ega.bred
  | `Slow, false -> Ega.red
  | `Bulk, true -> Ega.black
  | `Bulk, false -> Ega.dgray

let num = (to_enum `Bulk) + 1

let show = function
  | `Mail -> "Mail"
  | `Passenger -> "Passenger"
  | `Fast -> "Fast"
  | `Slow -> "Slow"
  | `Bulk -> "Bulk"

let of_good = function
  | Goods.Mail -> `Mail
  | Passengers -> `Passenger
  | Food
  | Livestock
  | Beer
  | Wine
  | Grapes
  | Armaments
  | MfgGoods -> `Fast
  | Fertilizer
  | Textiles
  | Steel
  | Grain
  | Paper
  | Hops -> `Slow
  | Cotton
  | Coal
  | Chemicals
  | Petroleum
  | Wood
  | Nitrates
  | Wool -> `Bulk

(* Index of good inside its freight class.
   Used for drawing shipping reports *)
let idx_of_good = function
  | Goods.Mail
  | Passengers
  | Food
  | Beer
  | Fertilizer
  | Grain
  | Hops
  | Chemicals
  | Petroleum
  | Nitrates
  | Wine -> 0
  | Grapes
  | Textiles
  | Paper
  | Cotton
  | Wood
  | Wool
  | Livestock -> 1
  | MfgGoods
  | Steel
  | Coal
  | Armaments -> 2

module Set = Bitset.Make(struct
  type nonrec t = t
  let t_of_yojson = t_of_yojson
  let yojson_of_t = yojson_of_t
  let of_enum = of_enum
  let to_enum = to_enum
  let last = `Bulk
end)

module Map = Utils.Map.Make(struct
  type nonrec t = t
  let t_of_yojson = t_of_yojson
  let yojson_of_t = yojson_of_t
  let compare = compare
end)

type complex =
  | MailFreight
  | PassengerFreight
  | MixedExpress
  | FastFreight
  | PassengerMixedFreight
  | MixedFreight
  | SlowFreight
  | BulkFreight
  [@@deriving yojson]

let show_complex = function
  | MailFreight -> "Mail"
  | PassengerFreight -> "Passenger"
  | MixedExpress ->  "Mixed Express"
  | FastFreight -> "Fast Freight"
  | PassengerMixedFreight -> "Passenger/Freight"
  | MixedFreight -> "Mixed Freight"
  | SlowFreight -> "Slow Freight"
  | BulkFreight -> "Bulk Freight"

let complex_of_set set = 
  assert (Set.cardinal set <> 0);
  if Set.mem set `Mail && Set.cardinal set = 1 then MailFreight
  else
    (* Mail won't matter from now on *)
    let set = Set.remove `Mail set in
    if Set.cardinal set = 1 then
      if Set.mem set `Passenger then PassengerFreight
      else if Set.mem set `Fast then FastFreight
      else if Set.mem set `Slow then SlowFreight
      else BulkFreight
    else if Set.mem set `Passenger then PassengerMixedFreight
    else MixedFreight




