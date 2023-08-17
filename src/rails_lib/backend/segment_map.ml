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

type upper = bool

type t = {
  mutable last: int;
  counts: (id, int) Hashtbl.t;
  stations: (int * int * upper, id) Hashtbl.t; (* x,y,upper to id *)
}[@@deriving yojson]

let make () = {
  last=0;
  counts=Hashtbl.create 10;
  stations=Hashtbl.create 10;
}

let new_id v =
  Hashtbl.replace v.counts v.last 0;
  let ret = v.last in
  v.last <- succ v.last;
  Log.debug (fun f -> f "Segment: Get new id %d" ret);
  ret

let incr_train v idx = Hashtbl.incr v.counts idx
let decr_train v idx = Hashtbl.decr v.counts idx
let reset v idx = Hashtbl.replace v.counts idx 0

(* Merge segments so seg2 joins seg1 *)
let merge v seg1 seg2 =
  Log.debug (fun f -> f "Segment: Merge ids %s, %s" (show seg1) (show seg2));
  let v2 = Hashtbl.find v.counts seg2 in
  Hashtbl.incr v.counts seg1 ~by:v2;
  Hashtbl.remove v.counts seg2;
  v



