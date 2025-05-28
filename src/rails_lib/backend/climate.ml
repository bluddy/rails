
type t =
  | Panic
  | Recession
  | Normal
  | Prosperity
  | Boom
  [@@deriving enum, yojson, show{with_path=false}]

let default = Normal

let weak_or_normal = function
  | Panic
  | Recession
  | Normal -> true
  | _ -> false

let strong = function
  | Prosperity
  | Boom -> true
  | _ -> false

let plus_4 climate = to_enum climate + 4

let modify random v =
  let roll_max = match v with Panic -> 2 | _ -> 3 in
  let roll = Random.int roll_max random in
  if roll = 0 then
  else v

