
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

