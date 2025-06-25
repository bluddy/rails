open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

module type S = sig
  type t
    [@@deriving yojson, show, eq]
  val to_int: t -> int
  val of_int: int -> t
end

module M : S = struct
  type t = int
    [@@ deriving yojson, show, eq]
  let to_int x : int = x
  let of_int x : t = x
end

include M

let print ?(ks=true) ?(show_neg=true) ?(spaces=0) ?(decimal=false) ?region (cash:t) =
  (* show_neg: have a negative symbol
     spaces: spaces to use for the upper thousands
     region: determines money symbol to use
  *)
  let b = Buffer.create 20 in
  Option.iter (fun region -> Buffer.add_char b @@ Region.money_symbol region) region;
  let cash = to_int cash in
  let cash = if not show_neg then abs cash else cash in
  let money_s = Printf.sprintf "%#d" cash |> String.map (function '_' -> ',' | x -> x) in
  let len = String.length money_s in
  for _j=0 to (spaces - 1 - len) do
    Buffer.add_char b ' ';
  done;
  Buffer.add_string b money_s;
  if ks then (
    Buffer.add_string b ",000"
  );
  if decimal then (
    Buffer.add_string b ".00"
  );
  let s = Buffer.contents b in
  if show_neg then s else "|"^s^"|"

let (+~) x y = to_int x + y |> of_int
let (+) x y = to_int x + to_int y |> of_int
let add = (+)
let neg x = ~- (to_int x) |> of_int
let (-~) x y = to_int x - y |> of_int
let (-) x y = to_int x - to_int y |> of_int
let sub = (-)
let (/~) (x:t) (y:t) : int = (to_int x) / (to_int y)
let (/) (x:t) (y:int) : t = (to_int x) / y |> of_int
let div = (/)
let ( *~ ) (x:t) (y:t) : t = (to_int x) * (to_int y) |> of_int
let ( * ) (x:t) (y:int) : t = (to_int x) * y |> of_int
let mult = ( * )
let (>) x y = to_int x > to_int y
let (<) x y = to_int x < to_int y
let (<=) x y = to_int x <= to_int y
let (>=) x y = to_int x >= to_int y
let (=) x y = to_int x = to_int y

let zero = of_int 0

let clip v ~min ~max =
  let min = of_int min in
  let max = of_int max in
  if v >= min then 
    if v <= max then v else max
  else
    min

let max v1 v2 = if v1 > v2 then v1 else v2


