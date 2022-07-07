
type t =
  | Panic
  | Recession
  | Moderation
  | Prosperity
  | Boom
  [@@deriving enum, sexp]

