module M = Money

type t =
  | Wood
  | Iron
  | Stone
  [@@deriving eq, hash, yojson, show]

let price_of = function
  | Wood -> M.of_int 50
  | Stone -> M.of_int 400
  | Iron -> M.of_int 200
