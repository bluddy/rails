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
  stations: (int * int * upper, id) Hashtbl.t; (* x,y,upper to id *)
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

let add locu id v =
  Hashtbl.replace v.stations locu id

let incr_train v idx = Hashtbl.incr v.counts idx
let decr_train v idx = Hashtbl.decr v.counts idx
let reset v idx = Hashtbl.replace v.counts idx 0
let get_id (x,y,d) v = Hashtbl.find v.stations (x, y, Dir.catalog d)

(* Merge segments so seg2 joins seg1 *)
let merge v seg1 seg2 =
  Log.debug (fun f -> f "Segment: Merge ids %s, %s" (show_id seg1) (show_id seg2));
  let v2 = Hashtbl.find v.counts seg2 in
  Hashtbl.incr v.counts seg1 ~by:v2;
  Hashtbl.remove v.counts seg2;
  ()

module TS = Trackmap.Search

(* When we build a station, we create new station segments on both ends of the station *)
(* TODO: for upgrade, need to compare before and after *)
let build_station_get_segments graph v trackmap ((x,y) as loc) after =
  (* Connected ixns *)
  let ixns = match after with
    | TS.Station ixns -> ixns
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
      | Some ((x,y), facing_us_dir) ->
          Log.debug (fun f -> f "Segments: found existing station at (%d,%d)" x y);
          (* Get its id *)
          let id = get_id (x, y, facing_us_dir) v in
          Some (ixn.search_dir, id)
      | _ -> None)
    ixns
  in
  (* Only care about having one result from each station direction *)
  let dir_segs = List.sort_uniq ~cmp:(fun (d1,_) (d2,_) -> Dir.compare d1 d2) dir_segs in
  (* Fill in with new segments as needed *)
  match dir_segs with
  | [] -> (* No connected id found: add new ids to both ends *)
      let id = new_id v in
      let id2 = new_id v in
      add (x,y,`Upper) id v;
      add (x,y,`Lower) id v;
      v

    (* Found only one id. Add one new one and add to both ends *)
  | [dir, id] -> 
      add (x, y, Dir.catalog dir) id v;
      let id2 = new_id v in
      add (x, y, Dir.catalog @@ Dir.opposite dir) id2 v;
      v

    (* Found both dirs *)
  | [dir, id; dir2, id2] ->
      assert Dir.(equal (opposite dir) dir2);
      add (x, y, Dir.catalog dir) id v;
      add (x, y, Dir.catalog dir2) id2 v;
      v

  | _ -> failwith "Found too many directions"


  (* We only care about connecting to a new piece of track that could lead
  to a station. ixns and stations are the same for this
  *)
  let build_track_join_segments graph station_map segments before after =
    let join_ixns = match before, after with
      (* Add an attached ixn: make the two have the same segment *)
      | TS.Track [_], TS.Track [ixn2; ixn3] -> Some (ixn2, ixn3)
      (* Add an ixn to a 2-ixn. Make them all have same segment *)
      | Track (ixn1::_ as l1), Ixn l2 ->
          begin match Utils.find_mismatch ~eq:TS.equal_ixn ~left:l2 ~right:l1 with
          | Some ixn2 -> Some (ixn1, ixn2)
          | None -> None
          end
      | _ -> None
    in
    match join_ixns with
    | None -> station_map
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        let loc, dir1 =
          Track_graph.connected_stations_dirs graph station_map ixn1 ~exclude_ixns:[ixn2]
          |> Iter.head_exn
        in
        let station1 = Station_map.get_exn loc station_map in
        let seg1 = Station.get_segment station1 dir1 in
        let stations =
          Track_graph.connected_stations_dirs graph station_map ixn2 ~exclude_ixns:[ixn1]
          |> Iter.to_list
        in
        let loc, dir2 = List.hd stations in
        let station2 = Station_map.get_exn loc station_map in
        let seg2 = Station.get_segment station2 dir2 in
        (* Assign seg1 to all connected stations that had seg2 *)
        let station_map =
          List.fold_left (fun station_map (loc, _) ->
              Station_map.update loc 
                (Option.map (fun station ->
                    Station.replace_segment station seg2 seg1))
                station_map)
            station_map
            stations
        in
        (* Update segment map *)
        Segment.Map.merge segments seg1 seg2;
        station_map

    (* Removing a piece of track can split a segment. Unfortunately we can't
       keep track of the segment's semaphore value unless we scan the whole segment for
       trains.
       We're too lazy to do that so we'll just set all segment values to 0.
       NOTE: This can cause train crashes. Implement with mapping to trains.
     *)
  let remove_track_split_segment graph station_map segments (before:TS.scan) (after:TS.scan) =
    let separate_pair = match before, after with
      (* Disconnecting a track leading to 2 ixns *)
      | Track [ixn1; ixn2], Track [_] -> Some(ixn1, ixn2)
      (* Disconnecting an ixn *)
      | Ixn l1, Track ((ixn2::_) as l2) ->
          begin match Utils.find_mismatch ~eq:TS.equal_ixn ~left:l1 ~right:l2 with
          | Some ixn1 -> Some (ixn1, ixn2)
          | None -> assert false
          end
      (* Removing a station *)
      | Station [ixn1; ixn2], (Track _ | NoResult) -> Some (ixn1, ixn2)
      | _ -> None
    in
    match separate_pair with
    | None -> station_map
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        let loc, dir1 =
          Track_graph.connected_stations_dirs graph station_map ixn1 |> Iter.head_exn
        in
        let station1 = Station_map.get_exn loc station_map in
        let seg1 = Station.get_segment station1 dir1 in
        (* Set value of segment to 0 *)
        Segment.Map.reset segments seg1;
        (* Create a new segment for the split segment *)
        let seg2 = Segment.Map.new_id segments in
        let stations = Track_graph.connected_stations_dirs graph station_map ixn2 in
        (* Assign seg2 to these stations *)
        let station_map =
          Iter.fold (fun station_map (loc, _) ->
              Station_map.update loc
                (Option.map @@
                  fun station -> Station.replace_segment station seg1 seg2)
                station_map)
            station_map
            stations
        in
        station_map

