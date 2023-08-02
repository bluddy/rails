open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
open Utils

(* A common pattern: a hashtbl from location to a thing *)

type 'a t = 'a IntIntMap.t [@@deriving yojson]

let empty = IntIntMap.empty

let length v = IntIntMap.cardinal v

let iter f v = IntIntMap.iter (fun _ station -> f station) v

let fold f v ~init =
  IntIntMap.fold (fun _ station acc -> f station acc) v init

let find f v =
  let exception Found of loc in
  try
    IntIntMap.iter (fun k station ->
      if f station then raise_notrace @@ Found k)
    v;
    None
  with
    Found k -> Some k

let get loc v = IntIntMap.find_opt loc v

let update loc f v =
  IntIntMap.update
    loc
    (fun v -> f v)
    v

let mem loc v = IntIntMap.mem loc v

let get_exn loc v = IntIntMap.find loc v

let add loc value v = IntIntMap.add loc value v

let delete loc v = IntIntMap.remove loc v

let filter f v =
  IntIntMap.to_iter v
  |> Iter.map snd |> Iter.filter (fun value -> f value)

