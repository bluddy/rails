open Containers

type id = int
[@@deriving yojson]

module Map = struct

type t = {
  mutable last: int;
  map: (id, int) Utils.Hashtbl.t; (* segment id to count *)
}[@@deriving yojson]

let make () = { last=0; map=Hashtbl.create 10; }

let get_id v =
  Hashtbl.replace v.map v.last 0;
  let ret = v.last in
  v.last <- succ v.last;
  ret

let incr_train v idx = Hashtbl.incr v.map idx
let decr_train v idx = Hashtbl.decr v.map idx

end
