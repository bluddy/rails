open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

type t = {
  freight: Freight.t;
  src_loc: Utils.loc;
  dst_loc: Utils.loc;
  deadline: int; (* in cycles. Used in a complicated way *)
} [@@deriving yojson]

(* The original game uses a strange function to decide if a priority delivery can spawn.
   1. We check every 8 time cycles
   2. We roll 1/96 for the source station, and 1/96 for the dest station based on the max number
   of stations.
   This will change throughout the game, as towards the end of the game we'll have up to 32
   full stations (1/3 * 1/3) while at the very beginning we'll have (1/96 * 1/96)
   There's also a range check to make sure that we're not too close (6) or too far (64).
   But the function ranges from 1/10000 in the early game to 1/9 in the late game.
  *)
let _create random stations cycle =
  let num_stations = Station_map.length stations in
  let src_i = Random.int num_stations random in
  let dst_i = Random.int num_stations random in
  if src_i = dst_i then None
  else
    let src_loc = Station_map.nth src_i stations in
    let dst_loc = Station_map.nth dst_i stations in
    let src_station = Station_map.get_exn src_loc stations in
    let dest_station = Station_map.get_exn dst_loc stations in
    if not @@ Station.is_proper_station src_station ||
       not @@ Station.is_proper_station dest_station then None
    else
      (* Check distance *)
      let dist = Utils.classic_dist src_loc dst_loc in
      if dist <= C.priority_min_dist || dist >= C.priority_max_dist then None
      else
        let freight = Random.pick_array Freight.all_freight random in
        let deadline = cycle - 1000 in
        let shipment = {src_loc; dst_loc; freight; deadline} in
        Some shipment

let try_to_create random stations cycle =
  (* The delay seems to be a result of just random tries *)
  let num_proper_stations = Station_map.get_num_proper_stations stations in
  (* This factor emulates the original function, which is dependent on the number of stations *)
  let factor = if num_proper_stations > 32 then (1. /. 9.)
    else
      let f = (float_of_int num_proper_stations /. 96.) in f *. f
  in
  let draw = Random.float 1. random in
  (* Return if we don't pass the test *)
  if draw >. factor then None
  else
    _create random stations cycle

let compute_bonus pr_data ~cycle ~year region =
  let dist = Utils.classic_dist pr_data.src_loc pr_data.dst_loc in
  let time_factor = cycle - pr_data.deadline
    |> Utils.clip ~min:(32 * dist) ~max:31999
  in
  let bonus_var = dist * 16 / (time_factor / 64 + 1) in
  let age = (year - C.reference_year) / 4 in
  let result = (dist + 32) * bonus_var * 8 / age in
  let bonus = Utils.clip ~min:0 ~max:999 result in
  let bonus = if Region.is_europe region then bonus + bonus / 2  else bonus in
  bonus

let should_be_cancelled pr_data ~cycle ~year region =
  compute_bonus pr_data ~cycle ~year region < C.priority_min_bonus

let create_text shipment (region:Region.t) station_map =
  let type_s = match shipment.freight, region with
    | `Mail, _ -> "Rare Vaccine Required."
    | `Passenger, _ -> "Congressional junket."
    | `Fast, Britain -> "Championship soccer game."
    | `Fast, _ -> "New strawberry crop."
    | `Slow, _ -> "Mine rescue equipment."
    | `Bulk, _ -> "Heating fuel emergency."
  in
  let get_name loc = Station_map.get_exn loc station_map |> Station.get_name in
  let msg = Printf.sprintf
    "Priority_shipment! \n\
     %s\n\
     \n from %s\
     \n to %s\
     \n"
    type_s (get_name shipment.src_loc) (get_name shipment.dst_loc)
  in
  msg

let cancel_text = "Priority Shipment\nCANCELLED.\n" 

