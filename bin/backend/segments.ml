open Containers

type t = {
  mutable last: int;
  map: (int, int) Utils.Hashtbl.t;
}[@@deriving yojson]

let make () = { last=0; map=Hashtbl.create 10; }

let get v =
  Hashtbl.replace v.map v.last 0;
  let ret = v.last in
  v.last <- succ v.last;
  ret

