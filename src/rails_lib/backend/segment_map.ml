open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module LocuSet = Utils.LocuSet

let src = Logs.Src.create "segments" ~doc:"Segments"
module Log = (val Logs.src_log src: Logs.LOG)

  (* Module to handle the connections between stations (segments). We use these
     to make sure the 'semaphore' for the track has the right number of trains on it.
     Unlike the graph, we don't concern ourselves with ixns
   *)

module Id = Int_id.Make(struct end)

type info = {
  mutable count: int;
  double: Track.double;
} [@@deriving yojson]

type t = {
  info: (Id.t, info) Utils.Hashtbl.t;
  stations: (Utils.loc * Dir.upper, Id.t) Utils.Hashtbl.t;
} [@@deriving yojson]

let make () = {
  info=Hashtbl.create 10;
  stations=Hashtbl.create 10;
}

let new_seg ?(double=`Double) v =
  (* Find a missing id to use *)
  let id =
    let rec loop i =
      if Hashtbl.mem v.info (Id.of_int i) then loop (i + 1)
      else i
    in
    loop 0
  in
  Hashtbl.replace v.info (Id.of_int id) {count = 0; double};
  Log.debug (fun f -> f "Segment: new id %d" id);
  Id.of_int id

let remove_seg id v =
  Hashtbl.remove v.info id

let add_station (loc, d) id v =
  (* add a station and direction + matching id *)
  Hashtbl.replace v.stations (loc, d) id

let remove_station (loc, d) v =
  (* remove a station and direction *)
  Hashtbl.remove v.stations (loc, d)

let set_seg_train_count idx v ~count =
  let info = Hashtbl.find v.info idx in
  info.count <- count

let get_station_seg locd v =
  (* get id for station/dir *)
  Hashtbl.find v.stations locd

let get_seg_double id v =
  let info = Hashtbl.find v.info id in
  info.double

let set_seg_double id double v =
  (* Update with new double state *)
  let info = Hashtbl.find v.info id in
  if not @@ Track.equal_double info.double double then 
    Hashtbl.replace v.info id {info with double=double}
  else ()

let seg_incr_train locd v =
  let id = get_seg_id locd v in
  Log.debug (fun f -> f "Segment: incr_train for id %s" (Id.show id));
  let info = Hashtbl.find v.info id in
  info.count <- info.count + 1

let seg_decr_train locd v =
  let id = get_seg_id locd v in
  Log.debug (fun f -> f "Segment: decr_train for id %s" (Id.show id));
  let info = Hashtbl.find v.info id in
  if info.count > 0 then
    info.count <- info.count - 1

let remap_station_seg_ids ~from_id to_id v =
  (* Remap all stations of a certain id to another one *)
  let stations_to_change = Hashtbl.fold
    (fun locd id acc -> if Id.equal id from_id then locd::acc else acc)
    v.stations []
  in
  List.iter (fun locd -> Hashtbl.replace v.stations locd to_id) stations_to_change

(* Merge segments so seg2 joins seg. All stations must be changed *)
let merge_segs ~from_id to_id v =
  Log.debug (fun f -> f "Segment: Merge seg_ids %s, %s" (Id.show to_id) (Id.show from_id));
  (* combine counts *)
  let info = Hashtbl.find v.info to_id in
  let from_info = Hashtbl.find v.info from_id in
  info.count <- info.count + from_info.count;
  (* double is harder to deal with and requires handling *)
  remap_station_seg_ids ~from_id to_id v;
  remove_seg from_id v

let get_seg_station_count id v =
  (* O(num_ids) *)
  Hashtbl.fold (fun _ id2 num ->
    if Id.equal id id2 then num + 1 else num)
    v.stations
    0

(* --- Handling different usecases --- *)

(* When we build a station, we create new station segments on both ends of the station *)
let handle_build_station graph v trackmap trains loc after =
  let x, y = loc in
  (* Connected ixns *)
  let ixns = match after with
    | Scan.Station ixns -> ixns  (* 0/1/2 *)
    | _ -> assert false
  in
  let dir_stations_on_both_sides =
    List.filter_map (fun ixn ->
      let exclude_dir = Dir.opposite ixn.Scan.dir in
      let stations = Track_graph.connected_stations_dirs_exclude_dir ~exclude_dir graph trackmap loc in
      if LocuSet.cardinal stations = 0 then None else Some(ixn.search_dir, stations))
    ixns
  in
  match dir_stations_on_both_sides with
  | [] -> (* No connected stations found: add new ids to both ends *)
      let id = new_seg v in
      let id2 = new_seg v in
      add_station (loc, `Upper) id v;
      add_station (loc, `Lower) id2 v;
      v
    (* Found only one id. Add one new one and add to both ends *)
  | [dir, loc_dirs] -> 
      (* Add to existing id, update double info *)
      let loc_dir = LocuSet.to_iter loc_dirs |> Iter.head_exn in
      let seg_id = get_seg_id loc_dir v in
      add_station (loc, Dir.catalog dir) seg_id v;
      let _, double = Scan.scan_station_segment trackmap trains ~x ~y dir ~player:0 in
      set_seg_double seg_id double v;
      
      (* New segment for missing end *)
      let seg_id = new_seg v in
      add_station (loc, Dir.catalog @@ Dir.opposite dir) seg_id v;
      v

    (* Found stations on both dirs. *)
  | [dir1, loc_dirs1; dir2, loc_dirs2 ] ->
      assert Dir.(equal (opposite dir1) dir2);

      let loc_dir1 = LocuSet.to_iter loc_dirs1 |> Iter.head_exn in
      let seg_id1 = get_seg_id loc_dir1 v in
      let loc_dir2 = LocuSet.to_iter loc_dirs2 |> Iter.head_exn in
      let seg_id2 = get_seg_id loc_dir2 v in
      (* Check if it's the same segment. They should have nothing in common *)
      let intersect = LocuSet.inter loc_dirs1 loc_dirs2 in
      if LocuSet.cardinal intersect > 0 then (
        (* Same segment on both sides *)
        add_station (loc, Dir.catalog dir1) seg_id1 v;
        add_station (loc, Dir.catalog dir2) seg_id1 v;
        (* Double status and count stays the same *)
        v
      ) else (
        (* Split segments with new station. On one end, connect *)
        add_station (loc, Dir.catalog dir1) seg_id1 v;
        let count, double = Scan.scan_station_segment trackmap trains ~x ~y dir1 ~player:0 in
        set_seg_double seg_id1 double v;
        set_seg_train_count seg_id1 ~count v;

        (* On second end, create a new id and apply it to all stations *)
        let seg_id = new_seg v in
        add_station (loc, Dir.catalog dir2) seg_id v;
        LocuSet.iter (fun loc_dir -> add_station loc_dir seg_id v) loc_dirs2;
        let count, double = Scan.scan_station_segment trackmap trains ~x ~y dir2 ~player:0 in
        set_seg_double seg_id double v;
        set_seg_train_count seg_id ~count v;

        (* GC old id if needed *)
        if get_seg_station_count seg_id2 v = 0 then remove_seg seg_id2 v;
        v
      )

  | _ -> failwith "Found too many directions or ill-formed data"


  (* We only care about connecting to a new piece of track that could lead
    to a station. ixns and stations are the same for this
  *)
  let build_track graph trackmap v before after =
    let join_ixns = match before, after with
      (* Add an attached ixn: make the two have the same segment *)
      | Scan.Track [_], Scan.Track [ixn2; ixn3] -> Some (ixn2, ixn3)

      (* Add an ixn to a 2-ixn. Make them all have same segment *)
      | Track l1, Ixn l2 ->
          (* Find an ixn they don't have in common and one they do *)
          Utils.diff_inter1 ~eq:Scan.equal_ixn l1 l2

      | _ -> None
    in
    join_ixns |>
    Option.map_or ~default:v
    (fun (ixn1_res, ixn2_res) ->
      let get_stations (ixn:Scan.ixn) (exclude_ixn:Scan.ixn) = 
        let loc = (ixn.x, ixn.y) in
        if Trackmap.has_station loc trackmap then
          (* Handle case where ixn is station *)
          LocuSet.singleton (loc, Dir.catalog ixn.dir)
        else
          Track_graph.connected_stations_dirs graph trackmap [loc] ~exclude_ixns:[(exclude_ixn.x, exclude_ixn.y)]
      in
      let stations1 = get_stations ixn1_res ixn2_res in
      let stations2 = get_stations ixn2_res ixn1_res in
      if LocuSet.cardinal stations1 = 0 || LocuSet.cardinal stations2 = 0 then
        (* If either set is empty, do nothing: we're not connecting to any station *)
        v
      else if LocuSet.inter stations1 stations2 |> LocuSet.cardinal > 0 then
        (* If it's the same segment on both sides, only update train count and double *)
        let train_cnt, double =
          Scan.scan_station_segment trackmap trains ~x:(ixn1_res.x) ~y:(ixn1_res.y) ixn1_res.dir ~player:0
        in
        let station = stations1.to_iter |> Iter.get_exn in
        let id = get_station_seg station in
        set_seg_double
      match stations1, stations2 with
      | (locd1, _)::_, (locd2, _)::_ ->
        (* Update double values: they could come from unseen areas *)
        let id1 = get_id locd1 v in
        let double1 = get_double id1 v in
        let id2 = get_id locd2 v in
        let double2 = get_double id2 v in
        set_double id1 (Track.combine_double double @@ Track.combine_double double1 double2) v;
        if Id.equal id1 id2 then v
        else (
          merge_ids id1 ~from_id:id2 v;
          v
        )
      | (locd, _)::_, _
      | _, (locd, _)::_ ->
        (* Only one connection - just update double *)
        let id1 = get_id locd v in
        let double1 = get_double id1 v in
        set_double id1 (Track.combine_double double double1) v;
        v
      | _ -> v
         (* Do nothing if there's no stations *)
      )
    *)

    (* Removing a piece of track can split a segment. Unfortunately we can't
       keep track of the segment's semaphore value unless we scan the whole segment for
       trains.
       We're too lazy to do that so we'll just set all segment values to 0.
       NOTE: This can cause train crashes. Implement with mapping to trains.
     *)
  let remove_track graph trackmap segments (before:Scan.t) (after:Scan.t) =
    let split_ixns = match before, after with
      (* Disconnecting a track leading to 2 ixns: if all paths are disconnected, create new segment *)
      | Track [ixn1; ixn2], _ -> Some(ixn1, ixn2)

      (* Disconnecting an ixn: also check disconnections on the disconnected sides *)
      | Ixn l1, Track l2 -> Utils.diff_inter1 ~eq:Scan.equal_ixn l1 l2

      | _ -> None
    in
    (* Find the sets of stations/dirs from each ixn. *)
    match split_ixns with
    | None -> segments
    | Some (ixn1s, ixn2s) ->
        let get_station_sets (ixns:Scan.ixn) =
          let ixn = (ixns.x, ixns.y) in
          (* We need to find the set differences. If it's a station, use that *)
          if Trackmap.has_station ixn trackmap then
            Utils.LocuSet.singleton (ixn, Dir.catalog ixns.dir)
          else
          Track_graph.connected_stations_dirs graph trackmap [ixn]
            |> Utils.LocuSet.of_iter
        in
        let set1 = get_station_sets ixn1s in
        let set2 = get_station_sets ixn2s in
        (* Possible situations:
           No intersection: make sure they're separate
           Common sets: don't separate
        *)
        (* Nothing to do if we have any empty station sets or if they're the same *)
        if Utils.LocuSet.equal set1 set2 || Utils.LocuSet.is_empty set1 || Utils.LocuSet.is_empty set2 then
          segments
        else
          (* Get one member of set1 *)
          let mem_set1 = Utils.LocuSet.choose set1 in
          let seg1 = get_id mem_set1 segments in
          let mem_set2 = Utils.LocuSet.choose set2 in
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
            Utils.LocuSet.iter (fun locd -> add locd seg2 segments) set2;
            segments
          end

    (* Removing a station. *)
    (* Cases:
       - No connection: just delete both segments
       - 1+ connection: for each side, see if you're the only station. If so, delete the segment
         - For non-station side, delete segment
    *)
  let remove_station graph trackmap v station_loc (before:Scan.t) =
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
      remove_seg seg1 v;
    )
    empty_dirs;
    (* Finally, delete entries for station no matter what *)
    List.iter (fun dir ->
      remove (station_loc, dir) v;
    ) dirs;
    v

  (* TODO: change_double track *)

