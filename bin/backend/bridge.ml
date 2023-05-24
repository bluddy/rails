
type t =
  | Wood
  | Iron
  | Stone
  [@@deriving eq, hash, yojson, show]

let price_of = function
  | Wood -> 50
  | Stone -> 400
  | Iron -> 200
