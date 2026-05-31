open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Generate unique ids that are integers inside *)

module type S = sig
  type t
    [@@deriving yojson, show, eq, ord]
  val to_int: t -> int
  val of_int: int -> t
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val show: t -> string
end

module Make() : S = struct
  type t = int
    [@@ deriving yojson, show, eq, ord]
  let to_int x : int = x
  let of_int x : t = x
  let (=) = equal
  let (<>) x y = not @@ equal x y
end

