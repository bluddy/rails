
type t =
  | Panic
  | Recession
  | Moderation
  | Prosperity
  | Boom
  [@@deriving enum, yojson]

let default = Moderation
