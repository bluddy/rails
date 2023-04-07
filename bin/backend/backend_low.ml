
open Containers

module TS = Trackmap.Search
module G = Track_graph
module C = Constants

(* Low-level backend module. Deals with multiple modules at a time *)

(* When we build a station, we create new station segments on both ends of the station *)
let build_station_get_segments graph stations segments track x y =
  let tile = Trackmap.get_exn track x y in
  Dir.Set.fold (fun acc dir ->
    match Track_graph.connected_stations_dirs ~search_dir:dir graph stations (x,y) |> Iter.head with
    | Some ((x,y), station_dir) ->
        let station = Loc_map.get_exn stations x y in
        let seg = Station.get_segment station station_dir in
        (dir, seg)::acc
    | None ->
        let seg = Segment.Map.get_id segments in
        (dir, seg)::acc)
   []
   tile.dirs

(* We only care about connecting to a new piece of track that could lead
to a station. ixns and stations are the same for this
TODO: use this function
*)
let build_track_join_segments graph station_map segments track x y (scan1:TS.scan) (scan2:TS.scan) =
  let join_ixns = match scan1, scan2 with
    (* Add an attached ixn: make the two have the same segment *)
    | Track [ixn1], Track [ixn2; ixn3] -> Some (ixn2, ixn3)
    (* Add an ixn to a 2-ixn. Make them all have same segment *)
    | Track (ixn1::_ as l1), Ixn l2 ->
        begin match Utils.find_mismatch ~eq:TS.eq ~left:l2 ~right:l1 with
        | Some ixn2 -> Some (ixn1, ixn2)
        | None -> None
        end
    | _ -> None
  in
  match join_ixns with
  | None -> ()
  | Some (ixn1, ixn2) ->
      let ixn1 = (ixn1.x, ixn1.y) in
      let ixn2 = (ixn2.x, ixn2.y) in
      let (x1, y1), dir1 =
        Track_graph.connected_stations_dirs graph station_map ixn1 ~exclude_ixns:[ixn2]
        |> Iter.head_exn
      in
      let station1 = Loc_map.get_exn station_map x1 y1 in
      let seg1 = Station.get_segment station1 dir1 in
      let stations =
        Track_graph.connected_stations_dirs graph station_map ixn2 ~exclude_ixns:[ixn1]
        |> Iter.to_list
      in
      let (x2, y2), dir2 = List.hd stations in
      let station2 = Loc_map.get_exn station_map x2 y2 in
      let seg2 = Station.get_segment station2 dir2 in
      (* Assign seg1 to all connected stations that had seg2 *)
      List.iter (fun ((x, y), _) ->
          Loc_map.update station_map x y @@
            (Option.map (fun station -> Station.modify_segment station seg2 seg1)))
        stations;
      (* Update segment map *)
      Segment.Map.merge segments seg1 seg2;
      ()

  (* Removing a piece of track can split a segment. Unfortunately we can't
     keep track of the segment's semaphore value unless we scan the whole segment for
     trains.
     We're too lazy to do that so we'll just set all segment values to 0.
     NOTE: This can cause train crashes.
     TODO: use this function
   *)
let remove_track_split_segment graph station_map segments track x y (before:TS.scan) (after:TS.scan) =
  let separate_pair = match before, after with
    (* Disconnecting a track leading to 2 ixns *)
    | Track [ixn1; ixn2], Track [_] -> Some(ixn1, ixn2)
    (* Disconnecting an ixn *)
    | Ixn l1, Track ((ixn2::_) as l2) ->
        begin match Utils.find_mismatch ~eq:TS.eq ~left:l1 ~right:l2 with
        | Some ixn1 -> Some (ixn1, ixn2)
        | None -> assert false
        end
    (* Removing a station *)
    | Station [ixn1; ixn2], _ -> Some (ixn1, ixn2)
    | _ -> None
  in
  match separate_pair with
  | None -> ()
  | Some (ixn1, ixn2) ->
      let ixn1 = (ixn1.x, ixn1.y) in
      let ixn2 = (ixn2.x, ixn2.y) in
      let (x1, y1), dir1 =
        Track_graph.connected_stations_dirs graph station_map ixn1 |> Iter.head_exn
      in
      let station1 = Loc_map.get_exn station_map x1 y1 in
      let seg1 = Station.get_segment station1 dir1 in
      (* Set value of segment to 0 *)
      Segment.Map.reset segments seg1;
      (* Create a new segment for the split segment *)
      let seg2 = Segment.Map.get_id segments in

      let stations = Track_graph.connected_stations_dirs graph station_map ixn2 in
      (* Assign seg2 to these stations *)
      Iter.iter (fun ((x, y), _) ->
          Loc_map.update station_map x y @@
            (Option.map (fun station -> Station.modify_segment station seg1 seg2)))
        stations;
      ()

