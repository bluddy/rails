open Containers
open Sexplib.Std

type t = {
  money: int;
}
[@@deriving sexp]

let default = {
  money=1000
}

let get_money v = v.money
