open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils

let src = Logs.Src.create "segments" ~doc:"Segments"
module Log = (val Logs.src_log src: Logs.LOG)

  (* Module to handle the connections between stations (segments). We use these
     to make sure the 'semaphore' for the track has the right number of trains on it.
   *)

type id = int
[@@deriving yojson, eq, show]

module Map = struct

  type upper = bool

  type t = {
    mutable last: int;
    counts: (id, int) Hashtbl.t;
    stations: (int * int * upper, id) Hashtbl.t; (* x,y,upper to id *)
  }[@@deriving yojson]

  let make () = { last=0; map=Hashtbl.create 10; }

  let new_id v =
    Hashtbl.replace v.map v.last 0;
    let ret = v.last in
    v.last <- succ v.last;
    Log.debug (fun f -> f "Segment: Get new id %d" ret);
    ret

  let incr_train v idx = Hashtbl.incr v.map idx
  let decr_train v idx = Hashtbl.decr v.map idx
  let reset v idx = Hashtbl.replace v.map idx 0

  (* Merge segments so seg2 joins seg1 *)
  let merge v seg1 seg2 =
    Log.debug (fun f -> f "Segment: Merge ids %d, %d" seg1 seg2);
    let v1 = Hashtbl.find v.map seg1 in
    let v2 = Hashtbl.find v.map seg2 in
    Hashtbl.replace v.map seg1 (v1 + v2);
    Hashtbl.remove v.map seg2

end


