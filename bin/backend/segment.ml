open Containers

type id = int
[@@deriving yojson, eq]

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
  let reset v idx = Hashtbl.replace v.map idx 0

  (* Merge segments so seg2 joins seg1 *)
  let merge v seg1 seg2 =
    let v1 = Hashtbl.find v.map seg1 in
    let v2 = Hashtbl.find v.map seg2 in
    Hashtbl.replace v.map seg1 (v1 + v2);
    Hashtbl.remove v.map seg2

end
