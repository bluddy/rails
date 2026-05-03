
module Level = struct
  type t =
    | Average
    | Good
    | Excellent
    | Awesome
    [@@deriving enum, show]

  let next = function
    | Average -> Good
    | Good -> Excellent
    | Excellent -> Awesome
    | Awesome -> Awesome

  let prev = function
    | Average -> Average
    | Good -> Average
    | Excellent -> Good
    | Awesome -> Excellent
end

type field =
  | Combat
  | Driving
  | Crypto
  | Electronics
  [@@deriving ord, yojson, eq]

module Map = struct
  include Utils.Map.Make(struct
  type t = field [@@deriving yojson]
  let compare = compare_field
end)

  let incr field v = update field (Option.map (fun field -> Level.next field)) v

  let decr field v = update field (Option.map (fun field -> Level.prev field)) v

end


