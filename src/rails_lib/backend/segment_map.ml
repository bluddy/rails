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

let remove_id id v =
  Hashtbl.remove v.counts id

let add (loc, d) id v =
  Hashtbl.replace v.stations (loc, Dir.catalog d) id

let remove (loc, d) v =
  Hashtbl.remove v.stations (loc, Dir.catalog d)

let reset idx v =
  Hashtbl.replace v.counts idx 0

let get_id (loc,d) v =
  Hashtbl.find v.stations (loc, Dir.catalog d)

let incr_train locd v =
  let id = get_id locd v in
  Hashtbl.incr v.counts id

let decr_train locd v =
  let id = get_id locd v in
  if Hashtbl.find v.counts id > 0 then
    Hashtbl.decr v.counts id

(* Merge segments so seg2 joins seg1 *)
let merge seg1 ~remove_seg v =
  Log.debug (fun f -> f "Segment: Merge ids %s, %s" (show_id seg1) (show_id remove_seg));
  let v2 = Hashtbl.find v.counts remove_seg in
  (* Because of the logic of incr, we need to not do this if we add 0 *)
  if v2 > 0 then
    Hashtbl.incr v.counts seg1 ~by:v2;
  Hashtbl.remove v.counts remove_seg;
  ()

module TS = Trackmap.Search

(* When we build a station, we create new station segments on both ends of the station *)
let build_station graph v trackmap loc after =
  (* Connected ixns *)
  let ixns = match after with
    | TS.Station ixns -> ixns  (* 0/1/2 *)
    | _ -> assert false
  in
  (* Get list of (dir, stations iter on both sides) *)
  let dir_stations =
    List.filter_map (fun ixn ->
      let exclude_dir = Dir.opposite ixn.TS.dir in
      let stations =
        Track_graph.connected_stations_dirs ~exclude_dir graph trackmap loc
        |> Iter.to_list
      in
      match stations with
      | [] -> None
      | _  -> Some(ixn.search_dir, stations))
    ixns
  in
  (* let dir_segs = *)
  (*     match station with *)
  (*     (* Found a station facing us *) *)
  (*     | Some ((x,y) as loc, facing_us_dir) -> *)
  (*         Log.debug (fun f -> f "Segments: found existing station at (%d,%d)" x y); *)
  (*         (* Get its id *) *)
  (*         let id = get_id (loc, facing_us_dir) v in *)
  (*         Some (ixn.search_dir, id) *)
  (*     | _ -> None) *)
  (*   ixns *)
  (* in *)
  (* We get at most 2: one per direction *)
  (* Fill in with new segments as needed *)
  match dir_stations with
  | [] -> (* No connected stations found: add new ids to both ends *)
      let id = new_id v in
      let id2 = new_id v in
      add (loc, Dir.Up) id v;
      add (loc, Dir.Down) id2 v;
      v
    (* Found only one id. Add one new one and add to both ends *)
  | [dir, loc_dir::_] -> 
      let id = get_id loc_dir v in
      add (loc, dir) id v;
      let id2 = new_id v in
      add (loc, Dir.opposite dir) id2 v;
      v
    (* Found both dirs.
       TODO: Need to split: new id on one end, assign to all stations on that end *)
  | [dir, loc_dirs; dir2, loc_dir2::_] ->
      assert Dir.(equal (opposite dir) dir2);
      (* On one end, add id to our station *)
      let id2 = get_id loc_dir2 v in
      add (loc, dir2) id2 v;
      (* On the other end, we need to create a new id and apply it to all stations *)
      let id = new_id v in
      add (loc, dir) id v;
      List.iter(fun loc_dir ->
        add loc_dir id v
      ) loc_dirs;
      v
  | _ -> failwith "Found too many directions or ill-formed data"


  (* We only care about connecting to a new piece of track that could lead
  to a station. ixns and stations are the same for this
  *)
  let build_track graph trackmap segments before after =
    let join_ixns = match before, after with
      (* Add an attached ixn: make the two have the same segment *)
      | TS.Track [_], TS.Track [ixn2; ixn3] -> Some (ixn2, ixn3)

      (* Add an ixn to a 2-ixn. Make them all have same segment *)
      (* TODO: what if these are stations? *)
      | Track l1, Ixn l2 ->
          (* Find an ixn they don't have in common and one they do *)
          Utils.diff_inter1 ~eq:TS.equal_ixn l1 l2

      | _ -> None
    in
    match join_ixns with
    | None -> segments
    | Some (ixn1d, ixn2d) ->
        let ixn1 = (ixn1d.x, ixn1d.y) in
        let ixn2 = (ixn2d.x, ixn2d.y) in
        let locd1 =
          if Trackmap.has_station ixn1 trackmap then
            (* Handle case where ixn is station *)
            Some (ixn1, ixn1d.dir)
          else
            Track_graph.connected_stations_dirs graph trackmap ixn1 ~exclude_ixns:[ixn2]
            |> Iter.head
        in
        let tgt_stations =
          if Trackmap.has_station ixn2 trackmap then
            (* Handle case where ixn is station *)
            [ixn2, ixn2d.dir]
          else
            Track_graph.connected_stations_dirs graph trackmap ixn2 ~exclude_ixns:[ixn1]
            |> Iter.to_list
        in
        begin match locd1, tgt_stations with
        | Some locd1, locd2::_ ->
          let seg1 = get_id locd1 segments in
          let seg2 = get_id locd2 segments in
          if seg1 = seg2 then
            segments
          else (
            (* Convert all stations and merge *)
            List.iter (fun locd -> add locd seg1 segments) tgt_stations;
            merge seg1 ~remove_seg:seg2 segments;
            segments
          )
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
    (* TODO: deleting stations with no connections should actually delete the segments *)
  let remove_track graph trackmap segments (before:TS.scan) (after:TS.scan) =
    let split_ixns = match before, after with
      (* Disconnecting a track leading to 2 ixns: if all paths are disconnected, create new segment *)
      | Track [ixn1; ixn2], Track [_] -> Some(ixn1, ixn2)

      (* Disconnecting an ixn: also check disconnections on the disconnected sides *)
      | Ixn l1, Track l2 -> Utils.diff_inter1 ~eq:TS.equal_ixn l1 l2

      | _ -> None
    in
    (* Find the sets of stations/dirs from each ixn. *)
    match split_ixns with
    | None -> segments
    | Some (ixn1s, ixn2s) ->
        let get_station_sets (ixns:TS.ixn) =
          let ixn = (ixns.x, ixns.y) in
          (* We need to find the set differences. If it's a station, use that *)
          if Trackmap.has_station ixn trackmap then
            LocdSet.singleton (ixn, ixns.dir)
          else
            Track_graph.connected_stations_dirs graph trackmap ixn
            |> LocdSet.of_iter
        in
        let set1 = get_station_sets ixn1s in
        let set2 = get_station_sets ixn2s in
        (* Possible situations:
           No intersection: make sure they're separate
           Common sets: don't separate
        *)
        (* Nothing to do if we have any empty station sets or if they're the same *)
        if LocdSet.equal set1 set2 then
          segments
        else
          (* Get one member of set1 *)
          let mem_set1 = LocdSet.choose set1 in
          let seg1 = get_id mem_set1 segments in
          let mem_set2 = LocdSet.choose set2 in
          let seg2 = get_id mem_set2 segments in
          if not @@ equal_id seg1 seg2 then
            segments
          else begin
            (* seg1 == seg2
              We don't know how mnay trains, so set value of segment to 0 *)
            (* TODO: find how many trains per new set *)
            reset seg1 segments;
            (* Create a new segment for the split segment *)
            let seg2 = new_id segments in
            (* Assign seg2 to all set2 stations *)
            LocdSet.iter (fun locd -> add locd seg2 segments) set2;
            segments
          end



    (* Removing a station. *)
    (* Cases:
       - No connection: just delete both stations
       - 1+ connection: for each side, see if you're the only station. If so, delete the segment
         - For non-station side, delete segment
    *)
  let remove_station graph trackmap v station_loc (before:TS.scan) =
    let x, y = station_loc in
    let tile = Trackmap.get_exn trackmap ~x ~y in
    let dirs = tile.Track.dirs |> Dir.Set.to_list in
    let ixns, empty_dirs = match before with
      | Station [ixn1; ixn2] ->
        [(ixn1.x, ixn1.y), ixn1.search_dir; (ixn2.x, ixn2.y), ixn2.search_dir], []

      | Station [ixn] ->
        [(ixn.x, ixn.y), ixn.search_dir], [Dir.opposite ixn.search_dir]

      | Station [] ->
        (* Both sides need to be deleted *)
        [], dirs
      
      | _ -> assert false
    in
    (* delete segment for station no matter what *)
    List.iter (fun dir ->
      remove (station_loc, dir) v;
    ) dirs;
    (* Check if we have more empty dirs, i.e. no stations *)
    let empty_dirs =
      List.fold_left (fun acc (loc, search_dir) ->
        if Trackmap.has_station loc trackmap then acc
        else
          let station_connected =
            Track_graph.connected_stations_dirs graph trackmap loc ~exclude_ixns:[station_loc]
            |> Iter.head
          in
          match station_connected with
          | None -> search_dir::acc
          | _ -> acc)
      empty_dirs
      ixns
    in
    (* GC: delete segments with no other stations *)
    List.iter (fun dir ->
      let seg1 = get_id (station_loc, dir) v in
      remove_id seg1 v;
    )
    empty_dirs;
    v
