
type t =
  | Panic
  | Recession
  | Normal
  | Prosperity
  | Boom
  [@@deriving enum, yojson, show{with_path=false}]

let default = Normal
