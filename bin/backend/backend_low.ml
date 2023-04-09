
open Containers

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

module TS = Trackmap.Search
module G = Track_graph
module C = Constants

(* Low-level backend module. Deals with multiple modules at a time *)

module Segments = struct

  (* When we build a station, we create new station segments on both ends of the station *)
  let build_station_get_segments graph stations segments trackmap x y after =
    let ixns = match after with
      | TS.Station ixns -> ixns
      | _ -> assert false
    in
    let dir_segs =
      List.filter_map (fun ixn ->
        let station =
          let exclude_dir = Dir.opposite ixn.TS.dir in
          Track_graph.connected_stations_dirs ~exclude_dir graph stations (x,y)
          |> Iter.head
        in
        match station with
        | Some ((x,y), station_dir) ->
            Log.debug (fun f -> f "Segments: found existing station at (%d,%d)" x y);
            let station = Loc_map.get_exn stations x y in
            let seg = Station.get_segment station station_dir in
            Some (ixn.search_dir, seg)
        | _ -> None)
      ixns
    in
    (* Fill in with new segments as needed *)
    match dir_segs with
    | [] ->
        let track = Trackmap.get_exn trackmap x y in
        (* Assert only 2 dirs *)
        Dir.Set.to_list track.dirs 
        |> List.map (fun dir -> dir, Segment.Map.new_id segments)
    | [(dir, seg)] as x -> 
        (Dir.opposite dir, Segment.Map.new_id segments)::x
    | _ -> dir_segs

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
        let seg2 = Segment.Map.new_id segments in

        let stations = Track_graph.connected_stations_dirs graph station_map ixn2 in
        (* Assign seg2 to these stations *)
        Iter.iter (fun ((x, y), _) ->
            Loc_map.update station_map x y @@
              (Option.map (fun station -> Station.modify_segment station seg1 seg2)))
          stations;
        ()

end

module Graph = struct
  open TS
  (* Routines to handle building/tearing down of track graph *)

  let handle_build_station graph ~x ~y scan1 scan2 =
    (* We just don't add stations until they've been hooked up *)
    let add_to_edge ixn1 _ ixn3 ixn4 =
      graph
      |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
      |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                        ~x2:x ~y2:y ~dir2:ixn3.search_dir
                        ~dist:ixn3.dist
      |> G.add_segment ~x1:ixn4.x ~y1:ixn4.y ~dir1:ixn4.dir
                        ~x2:x ~y2:y ~dir2:ixn4.search_dir
                        ~dist:ixn4.dist
    in
    match scan1, scan2 with
      (* Unfinished edge. Connect a station here.
          x---       ->    x---s *)
    | Track [ixn1], Station [ixn2] when TS.(eq ixn1 ixn2) ->
        G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                      ~x2:x ~y2:y ~dir2:ixn2.search_dir ~dist:ixn2.dist
                      graph

    (* Edge. Add a station.
      x-------x  ->    x---s---x
      Remove the edge and rebuild it to the new station.
    *)
    | Track [ixn1; ixn2], Station [ixn3; ixn4]
        when TS.(eq ixn1 ixn3 && eq ixn2 ixn4) ->
        add_to_edge ixn1 ixn2 ixn3 ixn4
    | Track [ixn1; ixn2], Station [ixn4; ixn3]
        when TS.(eq ixn1 ixn3 && eq ixn2 ixn4) ->
        add_to_edge ixn1 ixn2 ixn3 ixn4
    | _, _ -> graph

  (* Handle simple building of track graph-wise *)
  let handle_build_track graph scan1 scan2 =
    match scan1, scan2 with
      | Track [ixn1], Track [ixn2; ixn3]
          when TS.(eq ixn1 ixn2 || eq ixn1 ixn3) ->
          (* Only case: unfinished edge. Connect an intersection.
              x---       ->    x---x *)
          G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                        ~x2:ixn3.x ~y2:ixn3.y ~dir2:ixn3.dir
                        ~dist:(ixn2.dist+ixn3.dist)
                        graph
      | _ -> graph

  (* Handle graph management for building track.
      Complicated because we can have ixns everywhere. 
      TODO: check this for Station *)
    
  let handle_build_track_complex graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Unfinished edge. Connect an intersection.
          x---       ->    x---x *)
      | Track [ixn1], Track [ixn2; ixn3]
          when TS.(eq ixn1 ixn2 || TS.eq ixn1 ixn3) ->
            G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                          ~x2:ixn3.x ~y2:ixn3.y ~dir2:ixn3.dir
                          ~dist:(ixn2.dist+ixn3.dist)
                          graph
        (* Unfinished edge. Create an intersection.
          x---       ->    x--+ *)
      | Track [ixn1], Ixn [ixn2] when TS.eq ixn1 ixn2 ->
          G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                        ~x2:x ~y2:y ~dir2:ixn2.search_dir
                        ~dist:ixn2.dist
                        graph
        (* Regular edge. We add an intersection in the middle.
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4]
        when TS.(eq ixn1 ixn3 && eq ixn2 ixn4 || eq ixn1 ixn4 && eq ixn2 ixn3) ->
          graph
          |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
          |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                          ~x2:x ~y2:y ~dir2:ixn3.search_dir
                          ~dist:ixn3.dist
          |> G.add_segment ~x1:ixn4.x ~y1:ixn4.y ~dir1:ixn4.dir
                          ~x2:x ~y2:y ~dir2:ixn4.search_dir
                          ~dist:ixn4.dist
                          

        (* Regular edge. We add an intersection in the middle that connects to another
          intersection:
            x                x
            |                |
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4; ixn5]
        when TS.((eq ixn1 ixn3 && (eq ixn2 ixn4 || eq ixn2 ixn5))
          || (eq ixn1 ixn4 && (eq ixn2 ixn3 || eq ixn2 ixn5))
          || (eq ixn1 ixn5 && (eq ixn2 ixn3 || eq ixn2 ixn4))) ->
          graph
          |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
          |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                          ~x2:x ~y2:y ~dir2:ixn3.search_dir
                          ~dist:ixn3.dist
          |> G.add_segment ~x1:ixn4.x ~y1:ixn4.y ~dir1:ixn4.dir
                          ~x2:x ~y2:y ~dir2:ixn4.search_dir
                          ~dist:ixn4.dist
          |> G.add_segment ~x1:ixn5.x ~y1:ixn5.y ~dir1:ixn5.dir
                          ~x2:x ~y2:y ~dir2:ixn5.search_dir
                          ~dist:ixn5.dist
      | _ -> graph
        (* All other cases require no graph changes *)

  let handle_remove_track graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Was edge. Now disconnected
          x---x       ->    x- -x *)
      | Track [ixn1; ixn2], Track [ixn3]
          when TS.(eq ixn2 ixn3 || TS.eq ixn1 ixn3) ->
            G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir graph

        (* Was station. Now station gone.
          x---S       ->    x--- *)
      | Station [_], (Track [_] | NoResult) ->
            G.remove_ixn ~x ~y graph

        (* Was ixn. Now deleted.
          x---+       ->    x--- *)
      | Ixn [_], (Track [_] | NoResult) ->
            G.remove_ixn ~x ~y graph

        (* Was connecting station. Now disconnected
          x---S---x   ->    x--- ---x *)
      | Station [_; _], Track[_] ->
            G.remove_ixn ~x ~y graph

        (* Was 2 ixn. Now edge
          x---+---x   ->    x-------x *)

        (* Was 3 ixn. Now edge + disconnected
              x                 x
              |                 |
          x---+---x   ->    x-------x *)
      | Ixn [_; _], Track [ixn3; ixn4]
      | Ixn [_; _; _], Track [ixn3; ixn4] ->
          graph
          |> G.remove_ixn ~x ~y
          |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                          ~x2:ixn4.x ~y2:ixn4.y ~dir2:ixn4.dir
                          ~dist:(ixn3.dist + ixn4.dist)
      | _ -> graph
        (* All other cases require no graph changes *)

end
