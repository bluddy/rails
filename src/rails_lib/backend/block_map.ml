open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module LocuHSet = Utils.LocuHSet
open Block_map_d

let src = Logs.Src.create "blocks" ~doc:"Blocks"
module Log = (val Logs.src_log src: Logs.LOG)

  (* Module to handle the connections between stations (blocks). We use these
     to make sure the 'semaphore' for the track has the right number of trains on it.
     This is entirely game business logic.
     Unlike the graph, we don't concern ourselves with ixns
   *)

type t = Block_map_d.t [@@deriving yojson, show]

let make () = {
  info=Hashtbl.create 10;
  stations=Hashtbl.create 10;
  id_stations=Hashtbl.create 10;
}

let _new_block ?(double=`Double) v =
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
  Log.debug (fun f -> f "Block_map: new block %s" @@ Id.show id);
  id

let _remove_block id v =
  Log.debug (fun f -> f "Block_map: remove block %s" @@ Id.show id);
  Hashtbl.remove v.info id

let _add_station locu id v =
  (* add a station and direction + matching id *)
  Log.debug (fun f -> f "Block_map: add station (%s) to block %s" (Utils.show_locu locu) @@ Id.show id);
  Hashtbl.replace v.stations locu id;
  Hashtbl.update v.id_stations ~f:(fun _ v -> match v with
    | None -> Some [locu]
    | Some l -> Some (locu::l)
  ) ~k:id

let _remove_station locu v =
  Log.debug (fun f -> f "Block_map: remove station (%s)" @@ Utils.show_locu locu);
  (* remove a station and direction *)
  let id = Hashtbl.find v.stations locu in
  Hashtbl.remove v.stations locu;
  Hashtbl.update v.id_stations ~f:(fun _ -> function
    | Some l ->
        let l = List.filter (fun locu' -> not @@ Utils.equal_locu locu locu') l in
        if List.is_empty l then None else Some l
    | None -> None) ~k:id

let _set_block_train_count idx v ~count =
  Log.debug (fun f -> f "Block_map: set block %s count to %d" (Id.show idx) count);
  let info = Hashtbl.find v.info idx in
  info.count <- count

let get_station_block locu v =
  (* get id for station/dir *)
  try
    Hashtbl.find v.stations locu
  with Not_found ->
    failwith @@ Printf.sprintf "Locu %s not found" @@ Utils.show_locu locu

let stations_of_block id v =
  try
    Hashtbl.find v.id_stations id
  with Not_found -> []

let get_block_double id v =
  let info = Hashtbl.find v.info id in
  info.double

let _set_block_double id double v =
  (* Update with new double state *)
  Log.debug (fun f -> f "Block_map: set block %s double to %s" (Id.show id) @@ Track.show_double double);
  let info = Hashtbl.find v.info id in
  if not @@ Track.equal_double info.double double then
    Hashtbl.replace v.info id {info with double=double}
  else ()

let incr_train_stations_to_update locu v =
  let id = get_station_block locu v in
  Log.debug (fun f -> f "Block: incr_train for id %s" (Id.show id));
  let info = Hashtbl.find v.info id in
  info.count <- info.count + 1;
  let notify_stations = match info.count, info.double with
  | 1, `Single -> stations_of_block id v
  | 2, `Double -> stations_of_block id v
  | _ -> []
  in
  id, notify_stations, Station.Stop

let decr_train_stations_to_update id v =
  Log.debug (fun f -> f "Block: decr_train for id %s" (Id.show id));
  let info = Hashtbl.find v.info id in
  if info.count > 0 then (
    info.count <- info.count - 1
  );
  let notify_stations = match info.count, info.double with
  | 0, `Single -> stations_of_block id v
  | 1, `Double -> stations_of_block id v
  | _ -> []
  in
  notify_stations, Station.Go

let _remap_station_block_ids ~from_id to_id v =
  (* Remap all stations of a certain id to another one *)
  let stations_to_change = Hashtbl.find v.id_stations from_id in
  List.iter (fun locu -> Hashtbl.replace v.stations locu to_id) stations_to_change;
  Hashtbl.remove v.id_stations from_id;
  Hashtbl.replace v.id_stations to_id stations_to_change

(* Merge blocks so block2 joins block. All stations must be changed *)
let _merge_blocks ~from_id to_id v =
  Log.debug (fun f -> f "Block: Merge block_ids %s, %s" (Id.show to_id) (Id.show from_id));
  (* combine counts *)
  let info = Hashtbl.find v.info to_id in
  let from_info = Hashtbl.find v.info from_id in
  let count = info.count + from_info.count in
  let double = Track.combine_double info.double from_info.double in
  let info = {count; double} in
  Hashtbl.replace v.info to_id info;
  _remap_station_block_ids ~from_id to_id v;
  _remove_block from_id v

let _get_block_station_count id v = stations_of_block id v |> List.length

(* --- Handling different usecases --- *)

let _get_stations_with_ixn_scan ?exclude_ixns ixns graph trackmap =
  let loc = (ixns.Scan.x, ixns.y) in
  if Trackmap.has_station loc trackmap then
    (* Handle case where ixn is station *)
    LocuHSet.singleton (loc, Dir.to_upper ixns.dir)
  else
    match exclude_ixns with
    | Some exclude_ixns ->
      Track_graph.connected_stations_dirs ~exclude_ixns graph trackmap [loc]
    | _ ->
      Track_graph.connected_stations_dirs_exclude_dir ~exclude_dir:ixns.dir graph trackmap loc

(* When we build a station, we create new station blocks on both ends of the station *)
let handle_build_station player_idx graph v trackmap trains loc after =
  (* Connected ixns *)
  let ixns = match after with
    | Scan.Station ixns -> ixns  (* 0/1/2 *)
    | _ -> assert false
  in
  let dir_stations_on_both_sides =
    List.filter_map (fun ixns ->
      let stations = _get_stations_with_ixn_scan ixns graph trackmap in
      (* Remove current loc_dir if it somehow found its way in: it doesn't count *)
      LocuHSet.remove stations (loc, Dir.to_upper ixns.search_dir);
      match LocuHSet.cardinal stations with
      | 0 -> None
      | _ -> Some(ixns.search_dir, stations))
    ixns
  in
  match dir_stations_on_both_sides with
  | [] -> (* No connected stations found: add new ids to both ends *)
      let id = _new_block v in
      let id2 = _new_block v in
      _add_station (loc, `Upper) id v;
      _add_station (loc, `Lower) id2 v;
      v
    (* Found only one id. Add one new one and add to both ends *)
  | [dir, loc_dirs] -> 
      (* Add to existing id, update double info *)
      let loc_dir = LocuHSet.choose_exn loc_dirs in
      let block_id = get_station_block loc_dir v in
      _add_station (loc, Dir.to_upper dir) block_id v;
      let _, double = Scan.scan_station_block trackmap trains loc dir player_idx in
      _set_block_double block_id double v;

      (* New block for missing end *)
      let block_id = _new_block v in
      _add_station (loc, Dir.to_upper @@ Dir.opposite dir) block_id v;
      v

    (* Found stations on both dirs. *)
  | [dir1, loc_dirs1; dir2, loc_dirs2 ] ->
      assert Dir.(equal (opposite dir1) dir2);

      (* Deal with the case of both sides connected to each other only.
         In this case, we haven't even put them in the block map yet *)
      if LocuHSet.cardinal loc_dirs1 = 1 && LocuHSet.cardinal loc_dirs2 = 1 &&
          LocuHSet.mem loc_dirs1 (loc, Dir.to_upper dir2) &&
          LocuHSet.mem loc_dirs2 (loc, Dir.to_upper dir1) then begin
        (* Add them both in under a single block *)
        let block_id = _new_block v in
        _add_station (loc, Dir.to_upper dir2) block_id v;
        _add_station (loc, Dir.to_upper dir1) block_id v;
        let count, double = Scan.scan_station_block trackmap trains loc dir1 player_idx in
        _set_block_double block_id double v;
        _set_block_train_count block_id ~count v;
        v
      end else begin
        (* Normal case *)
        (* Remove these since they're not helpful *)
        LocuHSet.remove loc_dirs1 (loc, Dir.to_upper dir2);
        LocuHSet.remove loc_dirs2 (loc, Dir.to_upper dir1);
        let loc_dir1 = LocuHSet.choose_exn loc_dirs1 in
        let loc_dir2 = LocuHSet.choose_exn loc_dirs2 in
        let block_id1 = get_station_block loc_dir1 v in
        let block_id2 = get_station_block loc_dir2 v in
        (* Check if it's the same block. They should have nothing in common *)
        let intersect = LocuHSet.inter loc_dirs1 loc_dirs2 in
        if LocuHSet.cardinal intersect > 0 then (
          (* Same block on both sides *)
          _add_station (loc, Dir.to_upper dir1) block_id1 v;
          _add_station (loc, Dir.to_upper dir2) block_id1 v;
          (* Double status and count stays the same *)
          v
        ) else (
          (* Different blocks. Split blocks with new station. On one end, connect *)
          _add_station (loc, Dir.to_upper dir1) block_id1 v;
          let count, double = Scan.scan_station_block trackmap trains loc dir1 player_idx in
          _set_block_double block_id1 double v;
          _set_block_train_count block_id1 ~count v;

          (* On second end, create a new id and apply it to all stations *)
          let block_id = _new_block v in
          _add_station (loc, Dir.to_upper dir2) block_id v;
          LocuHSet.iter (fun loc_dir -> _add_station loc_dir block_id v) loc_dirs2;
          let count, double = Scan.scan_station_block trackmap trains loc dir2 player_idx in
          _set_block_double block_id double v;
          _set_block_train_count block_id ~count v;

          (* GC old id if needed *)
          if _get_block_station_count block_id2 v = 0 then _remove_block block_id2 v;
          v
        )
      end

  | _ -> failwith "Found too many directions or ill-formed data"


  (* We only care about connecting to a new piece of track that could lead
    to a station. ixns and stations are the same for this
  *)
  let handle_build_track player_idx graph trackmap trains v before after =
    let join_ixns = match before, after with
      (* Add an attached ixn: make the two have the same block *)
      | Scan.Track [_], Scan.Track [ixn2; ixn3] -> Some (ixn2, ixn3)

      (* Add an ixn to a 2-ixn. Make them all have same block *)
      | Track l1, Ixn l2 ->
          (* Find an ixn they don't have in common and one they do *)
          Utils.diff_inter1 ~eq:Scan.equal_ixn l1 l2

      | _ -> None
    in
    join_ixns |>
    Option.map_or ~default:v
    (fun (ixn1_res, ixn2_res) ->
      let stations1 = _get_stations_with_ixn_scan ixn1_res graph trackmap in
      let stations2 = _get_stations_with_ixn_scan ixn2_res graph trackmap in
      if LocuHSet.cardinal stations1 = 0 || LocuHSet.cardinal stations2 = 0 then
        (* If either set is empty, do nothing: we're not connecting to any station *)
        v
      else (
        let station1 = LocuHSet.choose_exn stations1 in
        let station2 = LocuHSet.choose_exn stations2 in
        let id1 = get_station_block station1 v in
        let id2 = get_station_block station2 v in
        if not @@ Id.equal id1 id2 then (
          (* We're joining 2 blocks. Combine and update count & double *)
          _merge_blocks ~from_id:id2 id1 v
        );
        (* update train count and double *)
        let count, double =
          Scan.scan_station_block trackmap trains (ixn1_res.x, ixn1_res.y) ixn1_res.dir player_idx
        in
        _set_block_double id1 double v;
        _set_block_train_count id1 ~count v;
        v
      )
    )

    (* Removing a piece of track can split a block.
       Check if it's truly split. If so, update counts and double status.
       *)
  let handle_remove_track player_idx graph trackmap trains v (before:Scan.t) (after:Scan.t) =
    let split_ixns = match before, after with
      (* Disconnecting a track leading to 2 ixns: if all paths are disconnected, create new block *)
      | Track [ixn1; ixn2], _ -> Some(ixn1, ixn2)

      (* Disconnecting an ixn: also check disconnections on the disconnected sides *)
      | Ixn l1, Track l2 -> Utils.diff_inter1 ~eq:Scan.equal_ixn l1 l2

      | _ -> None
    in
    (* Find the sets of stations/dirs from each ixn. *)
    match split_ixns with
    | None -> v
    | Some (ixn1s, ixn2s) ->
        let set1 = _get_stations_with_ixn_scan ixn1s graph trackmap in
        let set2 = _get_stations_with_ixn_scan ixn2s graph trackmap in
        (* Nothing to do if we have any empty station sets or if they're the same block still *)
        if LocuHSet.equal set1 set2 || LocuHSet.is_empty set1 || LocuHSet.is_empty set2 then
          v
        else
          (* Separate blocks *)
          let mem_set1 = LocuHSet.choose_exn set1 in
          let block1 = get_station_block mem_set1 v in
          let mem_set2 = LocuHSet.choose_exn set2 in
          let block2 = get_station_block mem_set2 v in
          if not @@ Id.equal block1 block2 then
            (* They're already different blocks *)
            v
          else begin
            (* Same block. Need to split it *)
            let count, double =
              Scan.scan_station_block trackmap trains (ixn1s.x, ixn1s.y) ixn1s.dir player_idx
            in
            _set_block_double block1 double v;
            _set_block_train_count block1 ~count v;

            (* Create a new block for the split block *)
            let block2 = _new_block v in
            (* Assign block2 to all set2 stations *)
            LocuHSet.iter (fun locd -> _add_station locd block2 v) set2;
            let count, double =
              Scan.scan_station_block trackmap trains (ixn2s.x, ixn2s.y) ixn2s.dir player_idx
            in
            _set_block_double block1 double v;
            _set_block_train_count block1 ~count v;
            v
          end

    (* Removing a station. *)
    (* NOTE: assumes we complete remove the track too *)
    (* Cases:
       - No connection: just delete both blocks
       - 1+ connection: for each side, see if you're the only station.
         - For non-station side, delete block
       - Train count and double: cannot be affected since we're an edge
    *)
  let handle_remove_station graph trackmap v loc (before:Scan.t) =
    let tile = Trackmap.get_exn loc trackmap in
    let dirs = tile.Track.dirs |> Dir.Set.to_list in
    let ixns, empty_dirs = match before with
      | Station ([_; _] as l) ->
        l,[]

      | Station ([ixn] as l) ->
        (* Delete just one side *)
        l, [Dir.opposite ixn.search_dir]

      | Station [] ->
        (* Both sides need to be deleted *)
        [], dirs

      | _ -> assert false
    in
    (* Check if we have more empty dirs, i.e. no stations but just ixns *)
    let empty_dirs =
      empty_dirs @
      List.filter_map (fun ixns ->
        let stations = _get_stations_with_ixn_scan ixns graph trackmap in
        if LocuHSet.is_empty stations then
            Some ixns.search_dir
        else None)
      ixns
    in
    (* GC: delete empty blocks *)
    List.iter (fun dir ->
      let block1 = get_station_block (loc, Dir.to_upper dir) v in
      _remove_block block1 v)
    empty_dirs;
    (* Finally, delete entries for station no matter what *)
    List.iter (fun dir -> _remove_station (loc, Dir.to_upper dir) v) dirs;
    v

  (* Handle double track change: just rescan the block and update *)
let handle_double_change player_idx graph trackmap trains v (after:Scan.t) =
  match after with
    | Scan.Track (ixn::_)
    | Scan.Ixn (ixn::_) -> 
      (* We may have a block *)
      let block =
        let loc = (ixn.x, ixn.y) in
        let (let*) = Option.bind in
        let* station_locu =
          if Trackmap.has_station loc trackmap then
            (* Handle case where ixn is station *)
            Some (loc, Dir.to_upper ixn.dir)
          else
            Track_graph.connected_stations_dirs graph trackmap [loc] |> LocuHSet.choose
        in
        get_station_block station_locu v |> Option.return
      in
      Option.map_or ~default:v
      (fun block ->
        let _, double =
          Scan.scan_station_block trackmap trains (ixn.x, ixn.y) ixn.dir player_idx
        in
        _set_block_double block double v;
        v)
      block

  | _ -> v

