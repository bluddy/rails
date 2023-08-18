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

type upper = [`Upper | `Lower]
  [@@deriving yojson]

type t = {
  mutable last: int;
  counts: (id, int) Hashtbl.t;
  stations: (loc * upper, id) Hashtbl.t;
} [@@deriving yojson]

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

let add (loc, d) id v =
  let upper = Dir.catalog d in
  Hashtbl.replace v.stations (loc, upper) id

let incr_train v idx = Hashtbl.incr v.counts idx
let decr_train v idx = Hashtbl.decr v.counts idx
let reset idx v = Hashtbl.replace v.counts idx 0
let get_id (loc,d) v = Hashtbl.find v.stations (loc, Dir.catalog d)

(* Merge segments so seg2 joins seg1 *)
let merge seg1 ~remove_seg v =
  Log.debug (fun f -> f "Segment: Merge ids %s, %s" (show_id seg1) (show_id remove_seg));
  let v2 = Hashtbl.find v.counts remove_seg in
  Hashtbl.incr v.counts seg1 ~by:v2;
  Hashtbl.remove v.counts remove_seg;
  ()

module TS = Trackmap.Search

(* When we build a station, we create new station segments on both ends of the station *)
(* TODO: for upgrade, need to compare before and after *)
let build_station_get_segments graph v trackmap loc after =
  (* Connected ixns *)
  let ixns = match after with
    | TS.Station ixns -> ixns  (* 0/1/2 *)
    | _ -> assert false
  in
  let dir_segs =
    List.filter_map (fun ixn ->
      let station =
        let exclude_dir = Dir.opposite ixn.TS.dir in
        (* We only need one connected station *)
        Track_graph.connected_stations_dirs ~exclude_dir graph trackmap loc
        |> Iter.head
      in
      match station with
      (* Found a station facing us *)
      | Some ((x,y) as loc, facing_us_dir) ->
          Log.debug (fun f -> f "Segments: found existing station at (%d,%d)" x y);
          (* Get its id *)
          let id = get_id (loc, facing_us_dir) v in
          Some (ixn.search_dir, id)
      | _ -> None)
    ixns
  in
  (* We get at most 2: one per direction *)
  (* Fill in with new segments as needed *)
  match dir_segs with
  | [] -> (* No connected id found: add new ids to both ends *)
      let id = new_id v in
      let id2 = new_id v in
      add (loc, Dir.Up) id v;
      add (loc, Dir.Down) id2 v;
      v

    (* Found only one id. Add one new one and add to both ends *)
  | [dir, id] -> 
      add (loc, dir) id v;
      let id2 = new_id v in
      add (loc, Dir.opposite dir) id2 v;
      v

    (* Found both dirs *)
  | [dir, id; dir2, id2] ->
      assert Dir.(equal (opposite dir) dir2);
      add (loc, dir) id v;
      add (loc, dir2) id2 v;
      v

  | _ -> failwith "Found too many directions"


  (* We only care about connecting to a new piece of track that could lead
  to a station. ixns and stations are the same for this
  *)
  let build_track_join_segments graph trackmap segments before after =
    let join_ixns = match before, after with
      (* Add an attached ixn: make the two have the same segment *)
      | TS.Track [_], TS.Track [ixn2; ixn3] -> Some (ixn2, ixn3)

      (* Add an ixn to a 2-ixn. Make them all have same segment *)
      | Track (ixn1::_ as l1), Ixn l2 ->
          (* Find an ixn they don't have in common *)
          Utils.find_mismatch ~eq:TS.equal_ixn ~left:l2 ~right:l1
          |> Option.map (fun ixn2 -> (ixn1, ixn2))

      | _ -> None
    in
    match join_ixns with
    | None -> segments
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        let locd1 =
          Track_graph.connected_stations_dirs graph trackmap ixn1 ~exclude_ixns:[ixn2]
          |> Iter.head
        in
        let tgt_stations =
          Track_graph.connected_stations_dirs graph trackmap ixn2 ~exclude_ixns:[ixn1]
          |> Iter.to_list
        in
        begin match locd1, tgt_stations with
        | Some locd1, locd2::_ ->
          let seg = get_id locd1 segments in
          let seg2 = get_id locd2 segments in
          (* Convert and merge *)
          List.iter (fun locd -> add locd seg segments) tgt_stations;
          merge seg ~remove_seg:seg2 segments;
          segments
        | _ ->
           (* Do nothing if there's no stations to merge *)
            segments
        end

    (* Removing a piece of track can split a segment. Unfortunately we can't
       keep track of the segment's semaphore value unless we scan the whole segment for
       trains.
       We're too lazy to do that so we'll just set all segment values to 0.
       NOTE: This can cause train crashes. Implement with mapping to trains.
     *)
  let remove_track_split_segment graph trackmap segments (before:TS.scan) (after:TS.scan) =
    let split_ixns = match before, after with
      (* Disconnecting a track leading to 2 ixns *)
      | Track [ixn1; ixn2], Track [_] -> Some(ixn1, ixn2)

      (* Disconnecting an ixn *)
      | Ixn l1, Track ((ixn2::_) as l2) ->
        Utils.find_mismatch ~eq:TS.equal_ixn ~left:l1 ~right:l2
        |> Option.map (fun ixn1 -> ixn1, ixn2)

      (* Removing a station with connections on both sides *)
      | Station [ixn1; ixn2], (Track _ | NoResult) ->
        Some (ixn1, ixn2)
      | _ -> None
    in
    match split_ixns with
    | None -> segments
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        (* We need all stations on one side *)
        let locd =
          Track_graph.connected_stations_dirs graph trackmap ixn1
          |> Iter.head
        in
        let tgt_stations =
          Track_graph.connected_stations_dirs graph trackmap ixn2
          |> Iter.to_list
        in
        match locd, tgt_stations with
        | Some locd, _::_ ->
          let seg = get_id locd segments in
          (* We don't know how mnay trains, so set value of segment to 0 *)
          reset seg segments;
          (* Create a new segment for the split segment *)
          let seg2 = new_id segments in
          (* Assign seg2 to all found stations *)
          List.iter (fun locd -> add locd seg2 segments) tgt_stations;
          segments
        | _ -> segments

