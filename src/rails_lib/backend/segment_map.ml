open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

let src = Logs.Src.create "segments" ~doc:"Segments"
module Log = (val Logs.src_log src: Logs.LOG)

  (* Module to handle the connections between stations (segments). We use these
     to make sure the 'semaphore' for the track has the right number of trains on it.
     Unlike the graph, we don't concern ourselves with ixns
   *)

type id = int
  [@@deriving yojson, eq, show]

type info = {
  mutable count: int;
  double: Track.double;
} [@@deriving yojson]

type t = {
  info: (id, info) Utils.Hashtbl.t;
  stations: (Utils.loc * Dir.upper, id) Utils.Hashtbl.t;
} [@@deriving yojson]

let make () = {
  info=Hashtbl.create 10;
  stations=Hashtbl.create 10;
}

let new_id ?(double=`Double) v =
  (* Find a missing id to use *)
  let id =
    let rec loop i =
      if Hashtbl.mem v.info i then loop (i + 1)
      else i
    in
    loop 0
  in
  Hashtbl.replace v.info id {count = 0; double};
  Log.debug (fun f -> f "Segment: Get new id %d" id);
  id

let remove_id id v =
  Hashtbl.remove v.info id

let add (loc, d) id v =
  (* add a station and direction + matching id *)
  Hashtbl.replace v.stations (loc, d) id

let remove (loc, d) v =
  (* remove a station and direction *)
  Hashtbl.remove v.stations (loc, d)

let reset idx v =
  (* reset the count for a segment id *)
  let info = Hashtbl.find v.info idx in
  info.count <- 0

let get_id locd v =
  (* get id for station/dir *)
  Hashtbl.find v.stations locd

let get_double id v =
  let info = Hashtbl.find v.info id in
  info.double

let set_double id double v =
  (* Update with new double state *)
  let info = Hashtbl.find v.info id in
  if not @@ Track.equal_double info.double double then 
    Hashtbl.replace v.info id {info with double=double}
  else ()

let incr_train locd v =
  let id = get_id locd v in
  Log.debug (fun f -> f "Segment: incr_train for id %s" (show_id id));
  let info = Hashtbl.find v.info id in
  info.count <- info.count + 1

let decr_train locd v =
  let id = get_id locd v in
  Log.debug (fun f -> f "Segment: decr_train for id %s" (show_id id));
  let info = Hashtbl.find v.info id in
  if info.count > 0 then
    info.count <- info.count - 1

let remap_station_ids ~from_id to_id v =
  let stations_to_change = Hashtbl.fold
    (fun locd id acc -> if equal_id id from_id then locd::acc else acc)
    v.stations []
  in
  List.iter (fun locd -> Hashtbl.replace v.stations locd to_id) stations_to_change

(* Merge segments so seg2 joins seg. All stations must be changed *)
let merge_ids ~from_id to_id v =
  Log.debug (fun f -> f "Segment: Merge seg_ids %s, %s" (show_id to_id) (show_id from_id));
  (* combine counts *)
  let info = Hashtbl.find v.info to_id in
  let from_info = Hashtbl.find v.info from_id in
  info.count <- info.count + from_info.count;
  remap_station_ids ~from_id to_id v;
  remove_id from_id v

let get_id_station_count id v =
  (* O(num_ids) *)
  Hashtbl.fold (fun _ id2 num -> if equal_id id id2 then num + 1 else num) v.stations 0

module TS = Trackmap.Search

let all_double info =
  List.fold_left (fun acc (_, dbl) -> Track.combine_double dbl acc) `Double info

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
      let stations = Track_graph.connected_stations_dirs_exclude_dir ~exclude_dir graph trackmap loc |> Iter.to_list in
      if List.is_empty stations then None else Some(ixn.search_dir, stations))
    ixns
  in
  match dir_stations with
  | [] -> (* No connected stations found: add new ids to both ends *)
      let id = new_id v in
      let id2 = new_id v in
      add (loc, `Upper) id v;
      add (loc, `Lower) id2 v;
      v
    (* Found only one id. Add one new one and add to both ends *)
  | [dir, (((loc_dir, _)::_) as info)] -> 
      (* Add to existing id, update double info *)
      let id = get_id loc_dir v in
      add (loc, Dir.catalog dir) id v;
      (* Only double if all connections are double *)
      set_double id (all_double info) v;

      (* New segment for missing end *)
      let id2 = new_id v in
      add (loc, Dir.catalog @@ Dir.opposite dir) id2 v;
      v

    (* Found stations on both dirs. *)
  | [dir1, ((loc_dir1, _)::_ as info1); dir2, ((loc_dir2, _)::_ as info2)] ->
      assert Dir.(equal (opposite dir1) dir2);

      (* On first end, add existing id to our station and update double *)
      let id = get_id loc_dir1 v in
      add (loc, Dir.catalog dir1) id v;
      let old_double = get_double id v in
      let double = Track.combine_double old_double @@ all_double info1 in
      set_double id double v;

      (* On second end, we need to get the old double, create a new id and apply it to all stations *)
      (* TODO: handle train counts after splitting *)
      let old_id = get_id loc_dir2 v in
      let double = all_double info2 in
      let id = new_id ~double v in
      add (loc, Dir.catalog dir2) id v;
      List.iter (fun (loc_dir, _) -> add loc_dir id v) info2;
      (* GC old id if needed *)
      if get_id_station_count old_id v = 0 then remove_id old_id v;
      v

  | _ -> failwith "Found too many directions or ill-formed data"


  (* We only care about connecting to a new piece of track that could lead
    to a station. ixns and stations are the same for this
  *)
  let build_track graph trackmap v before after =
    let join_ixns = match before, after with
      (* Add an attached ixn: make the two have the same segment *)
      | TS.Track [_], TS.Track [ixn2; ixn3] -> Some (ixn2, ixn3)

      (* Add an ixn to a 2-ixn. Make them all have same segment *)
      | Track l1, Ixn l2 ->
          (* Find an ixn they don't have in common and one they do *)
          Utils.diff_inter1 ~eq:TS.equal_ixn l1 l2

      | _ -> None
    in
    join_ixns |>
    Option.map_or ~default:v
    (fun (ixn1_res, ixn2_res) ->
      let get_stations (ixn:TS.ixn) (exclude_ixn:TS.ixn) = 
        let loc = (ixn.x, ixn.y) in
        if Trackmap.has_station loc trackmap then
          (* Handle case where ixn is station *)
          [(loc, Dir.catalog ixn.dir), `Double]
        else
          (* Gets us potentially only part of the station segment *)
          Track_graph.connected_stations_dirs graph trackmap [loc] ~exclude_ixns:[(exclude_ixn.x, exclude_ixn.y)] |> Iter.to_list
      in
      let stations1 = get_stations ixn1_res ixn2_res in
      let stations2 = get_stations ixn2_res ixn1_res in
      let double = Track.combine_double (all_double stations1) (all_double stations2) in
      match stations1, stations2 with
      | (locd1, _)::_, (locd2, _)::_ ->
        (* Update double values: they could come from unseen areas *)
        let id1 = get_id locd1 v in
        let double1 = get_double id1 v in
        let id2 = get_id locd2 v in
        let double2 = get_double id2 v in
        set_double id1 (Track.combine_double double @@ Track.combine_double double1 double2) v;
        if equal_id id1 id2 then v
        else (
          merge_ids id1 ~from_id:id2 v;
          v
        )
      | (locd, _)::_, _
      | _, (locd, _)::_ ->
        (* Just update double *)
        let id1 = get_id locd v in
        let double1 = get_double id1 v in
        set_double id1 (Track.combine_double double double1) v;
        v
      | _ -> v
         (* Do nothing if there's no stations *)
      )

    (* Removing a piece of track can split a segment. Unfortunately we can't
       keep track of the segment's semaphore value unless we scan the whole segment for
       trains.
       We're too lazy to do that so we'll just set all segment values to 0.
       NOTE: This can cause train crashes. Implement with mapping to trains.
     *)
  let remove_track graph trackmap segments (before:TS.scan) (after:TS.scan) =
    let split_ixns = match before, after with
      (* Disconnecting a track leading to 2 ixns: if all paths are disconnected, create new segment *)
      | Track [ixn1; ixn2], _ -> Some(ixn1, ixn2)

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
            Utils.LocdSet.singleton (ixn, ixns.dir)
          else
          Track_graph.connected_stations_dirs graph trackmap [ixn]
            |> Utils.LocdSet.of_iter
        in
        let set1 = get_station_sets ixn1s in
        let set2 = get_station_sets ixn2s in
        (* Possible situations:
           No intersection: make sure they're separate
           Common sets: don't separate
        *)
        (* Nothing to do if we have any empty station sets or if they're the same *)
        if Utils.LocdSet.equal set1 set2 || Utils.LocdSet.is_empty set1 || Utils.LocdSet.is_empty set2 then
          segments
        else
          (* Get one member of set1 *)
          let mem_set1 = Utils.LocdSet.choose set1 in
          let seg1 = get_id mem_set1 segments in
          let mem_set2 = Utils.LocdSet.choose set2 in
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
            Utils.LocdSet.iter (fun locd -> add locd seg2 segments) set2;
            segments
          end

    (* Removing a station. *)
    (* Cases:
       - No connection: just delete both segments
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
    (* Check if we have more empty dirs, i.e. no stations *)
    let empty_dirs =
      List.fold_left (fun acc (loc, search_dir) ->
        (* Get either station here or connected stations *)
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
    (* Finally, delete entries for station no matter what *)
    List.iter (fun dir ->
      remove (station_loc, dir) v;
    ) dirs;
    v

