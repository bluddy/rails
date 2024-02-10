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
  let id = Id.of_int id in
  Hashtbl.replace v.info id {count = 0; double};
  Log.debug (fun f -> f "Segment_map: new segment %s" @@ Id.show id);
  id

let remove_seg id v =
  Log.debug (fun f -> f "Segment_map: remove segment %s" @@ Id.show id);
  Hashtbl.remove v.info id

let add_station locu id v =
  (* add a station and direction + matching id *)
  Log.debug (fun f -> f "Segment_map: add station (%s) to segment %s" (Utils.show_locu locu) @@ Id.show id);
  Hashtbl.replace v.stations locu id

let remove_station locu v =
  Log.debug (fun f -> f "Segment_map: remove station (%s)" @@ Utils.show_locu locu);
  (* remove a station and direction *)
  Hashtbl.remove v.stations locu

let set_seg_train_count idx v ~count =
  Log.debug (fun f -> f "Segment_map: set seg %s count to %d" (Id.show idx) count);
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
  Log.debug (fun f -> f "Segment_map: set seg %s double to %s" (Id.show id) @@ Track.show_double double);
  let info = Hashtbl.find v.info id in
  if not @@ Track.equal_double info.double double then 
    Hashtbl.replace v.info id {info with double=double}
  else ()

let seg_incr_train locd v =
  let id = get_station_seg locd v in
  Log.debug (fun f -> f "Segment: incr_train for id %s" (Id.show id));
  let info = Hashtbl.find v.info id in
  info.count <- info.count + 1

let seg_decr_train locd v =
  let id = get_station_seg locd v in
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

let get_stations_with_ixn_scan ixns graph trackmap =
  let loc = (ixns.Scan.x, ixns.y) in
  if Trackmap.has_station loc trackmap then
    (* Handle case where ixn is station *)
    LocuSet.singleton (loc, Dir.to_upper ixns.dir)
  else
    Track_graph.connected_stations_dirs_exclude_dir ~exclude_dir:ixns.dir graph trackmap loc

(* When we build a station, we create new station segments on both ends of the station *)
let handle_build_station graph v trackmap trains loc after =
  let x, y = loc in
  (* Connected ixns *)
  let ixns = match after with
    | Scan.Station ixns -> ixns  (* 0/1/2 *)
    | _ -> assert false
  in
  let dir_stations_on_both_sides =
    List.filter_map (fun ixns ->
      let stations = get_stations_with_ixn_scan ixns graph trackmap in
      if LocuSet.cardinal stations = 0 then None else Some(ixns.search_dir, stations))
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
      let loc_dir = LocuSet.choose_exn loc_dirs in
      let seg_id = get_station_seg loc_dir v in
      add_station (loc, Dir.to_upper dir) seg_id v;
      let _, double = Scan.scan_station_segment trackmap trains ~x ~y dir ~player:0 in
      set_seg_double seg_id double v;
      
      (* New segment for missing end *)
      let seg_id = new_seg v in
      add_station (loc, Dir.to_upper @@ Dir.opposite dir) seg_id v;
      v

    (* Found stations on both dirs. *)
  | [dir1, loc_dirs1; dir2, loc_dirs2 ] ->
      assert Dir.(equal (opposite dir1) dir2);

      let loc_dir1 = LocuSet.choose_exn loc_dirs1 in
      let seg_id1 = get_station_seg loc_dir1 v in
      let loc_dir2 = LocuSet.choose_exn loc_dirs2 in
      let seg_id2 = get_station_seg loc_dir2 v in
      (* Check if it's the same segment. They should have nothing in common *)
      let intersect = LocuSet.inter loc_dirs1 loc_dirs2 in
      if LocuSet.cardinal intersect > 0 then (
        (* Same segment on both sides *)
        add_station (loc, Dir.to_upper dir1) seg_id1 v;
        add_station (loc, Dir.to_upper dir2) seg_id1 v;
        (* Double status and count stays the same *)
        v
      ) else (
        (* Split segments with new station. On one end, connect *)
        add_station (loc, Dir.to_upper dir1) seg_id1 v;
        let count, double = Scan.scan_station_segment trackmap trains ~x ~y dir1 ~player:0 in
        set_seg_double seg_id1 double v;
        set_seg_train_count seg_id1 ~count v;

        (* On second end, create a new id and apply it to all stations *)
        let seg_id = new_seg v in
        add_station (loc, Dir.to_upper dir2) seg_id v;
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
  let handle_build_track graph trackmap trains v before after =
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
      let stations1 = get_stations_with_ixn_scan ixn1_res graph trackmap in
      let stations2 = get_stations_with_ixn_scan ixn2_res graph trackmap in
      if LocuSet.cardinal stations1 = 0 || LocuSet.cardinal stations2 = 0 then
        (* If either set is empty, do nothing: we're not connecting to any station *)
        v
      else (
        let station1 = LocuSet.choose_exn stations1 in
        let station2 = LocuSet.choose_exn stations2 in
        let id1 = get_station_seg station1 v in
        let id2 = get_station_seg station2 v in
        if not @@ Id.equal id1 id2 then (
          (* We're joining 2 segments. Combine and update count & double *)
          merge_segs ~from_id:id2 id1 v
        );
        (* update train count and double *)
        let count, double =
          Scan.scan_station_segment trackmap trains ~x:(ixn1_res.x) ~y:(ixn1_res.y) ixn1_res.dir ~player:0
        in
        set_seg_double id1 double v;
        set_seg_train_count id1 ~count v;
        v
      )
    )

    (* Removing a piece of track can split a segment.
       Check if it's truly split. If so, update counts and double status.
       *)
  let handle_remove_track graph trackmap trains v (before:Scan.t) (after:Scan.t) =
    let split_ixns = match before, after with
      (* Disconnecting a track leading to 2 ixns: if all paths are disconnected, create new segment *)
      | Track [ixn1; ixn2], _ -> Some(ixn1, ixn2)

      (* Disconnecting an ixn: also check disconnections on the disconnected sides *)
      | Ixn l1, Track l2 -> Utils.diff_inter1 ~eq:Scan.equal_ixn l1 l2

      | _ -> None
    in
    (* Find the sets of stations/dirs from each ixn. *)
    match split_ixns with
    | None -> v
    | Some (ixn1s, ixn2s) ->
        let set1 = get_stations_with_ixn_scan ixn1s graph trackmap in
        let set2 = get_stations_with_ixn_scan ixn2s graph trackmap in
        (* Nothing to do if we have any empty station sets or if they're the same segment still *)
        if LocuSet.equal set1 set2 || LocuSet.is_empty set1 || LocuSet.is_empty set2 then
          v
        else
          (* Separate segments *)
          let mem_set1 = LocuSet.choose_exn set1 in
          let seg1 = get_station_seg mem_set1 v in
          let mem_set2 = LocuSet.choose_exn set2 in
          let seg2 = get_station_seg mem_set2 v in
          if not @@ Id.equal seg1 seg2 then
            (* They're already different segments *)
            v
          else begin
            (* Same segment. Need to split it *)
            let count, double =
              Scan.scan_station_segment trackmap trains ~x:(ixn1s.x) ~y:(ixn1s.y) ixn1s.dir ~player:0
            in
            set_seg_double seg1 double v;
            set_seg_train_count seg1 ~count v;

            (* Create a new segment for the split segment *)
            let seg2 = new_seg v in
            (* Assign seg2 to all set2 stations *)
            LocuSet.iter (fun locd -> add_station locd seg2 v) set2;
            let count, double =
              Scan.scan_station_segment trackmap trains ~x:(ixn2s.x) ~y:(ixn2s.y) ixn2s.dir ~player:0
            in
            set_seg_double seg1 double v;
            set_seg_train_count seg1 ~count v;
            v
          end

    (* Removing a station. *)
    (* NOTE: assumes we complete remove the track too *)
    (* Cases:
       - No connection: just delete both segments
       - 1+ connection: for each side, see if you're the only station.
         - For non-station side, delete segment
       - Train count and double: cannot be affected since we're an edge
    *)
  let handle_remove_station graph trackmap v station_loc (before:Scan.t) =
    let x, y = station_loc in
    let tile = Trackmap.get_exn trackmap ~x ~y in
    let dirs = tile.Track.dirs |> Dir.Set.to_list in
    let ixns, empty_dirs = match before with
      | Station [ixn1; ixn2] ->
        [(ixn1.x, ixn1.y), ixn1.search_dir; (ixn2.x, ixn2.y), ixn2.search_dir], []

      | Station [ixn] ->
        (* Delete just one side *)
        [(ixn.x, ixn.y), ixn.search_dir], [Dir.opposite ixn.search_dir]

      | Station [] ->
        (* Both sides need to be deleted *)
        [], dirs
      
      | _ -> assert false
    in
    (* Check if we have more empty dirs, i.e. no stations but just ixns *)
    let empty_dirs =
      List.fold_left (fun acc (loc, search_dir) ->
        (* Get either the station here or connected stations *)
        if Trackmap.has_station loc trackmap then acc
        else
          let station_connected =
            Track_graph.connected_stations_dirs graph trackmap [loc] ~exclude_ixns:[station_loc]
          in
          if LocuSet.is_empty station_connected then
            search_dir::acc
          else acc)
      empty_dirs
      ixns
    in
    (* GC: delete empty segments *)
    List.iter (fun dir ->
      let seg1 = get_station_seg (station_loc, Dir.to_upper dir) v in
      remove_seg seg1 v)
    empty_dirs;
    (* Finally, delete entries for station no matter what *)
    List.iter (fun dir -> remove_station (station_loc, Dir.to_upper dir) v) dirs;
    v

  (* Handle double track change: just rescan the segment and update *)
let handle_double_change graph trackmap trains v (after:Scan.t) =
  match after with
    | Scan.Track (ixn::_)
    | Scan.Ixn (ixn::_) -> 
      (* We may have a segment *)
      let seg =
        let loc = (ixn.x, ixn.y) in
        let (let*) = Option.bind in
        let* station_locu =
          if Trackmap.has_station loc trackmap then
            (* Handle case where ixn is station *)
            Some (loc, Dir.to_upper ixn.dir)
          else
            Track_graph.connected_stations_dirs graph trackmap [loc] |> LocuSet.choose
        in
        get_station_seg station_locu v |> Option.return
      in
      Option.map_or ~default:v
      (fun seg ->
        let _, double =
          Scan.scan_station_segment trackmap trains ~x:(ixn.x) ~y:(ixn.y) ixn.dir ~player:0
        in
        set_seg_double seg double v;
        v)
      seg

  | _ -> v



