open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Generate unique ids that are integers inside *)

module type S = sig
  type t
    [@@deriving yojson, show, eq]
  val to_int: t -> int
  val of_int: int -> t
end

module Make(M: sig end) : S = struct
  type t = int
    [@@ deriving yojson, show, eq]
  let to_int x : int = x
  let of_int x : t = x
end

