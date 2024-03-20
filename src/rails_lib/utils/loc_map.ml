open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
open Utils

(* A common pattern: a hashtbl from location to a thing *)

type 'a t = 'a LocMap.t [@@deriving yojson]

let empty = LocMap.empty

let length v = LocMap.cardinal v

let iter f v = LocMap.iter (fun _ station -> f station) v

let fold f v ~init =
  LocMap.fold (fun _ station acc -> f station acc) v init

let find f v =
  let exception Found of loc in
  try
    LocMap.iter (fun k station ->
      if f station then raise_notrace @@ Found k)
    v;
    None
  with
    Found k -> Some k

let get loc v = LocMap.find_opt loc v

let update loc f v =
  LocMap.update
    loc
    (fun v -> f v)
    v

let mem loc v = LocMap.mem loc v

let get_exn loc v = LocMap.find loc v

let add loc value v = LocMap.add loc value v

let delete loc v = LocMap.remove loc v

let filter f v =
  LocMap.to_iter v
  |> Iter.map snd |> Iter.filter (fun value -> f value)

