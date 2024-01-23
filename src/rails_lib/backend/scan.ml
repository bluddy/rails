open! Containers
open! Utils
module C = Constants

(* Module for searching the trackmap for stations and ixns, and the trainmap for trains
   For updating the graph and the station segment connectivity
 *)
(* This is our return type representing an ixn. Will be used to introspect *)
type ixn = {
  x: int;
  y: int;
  dist: int;
  dir: Dir.t; (* out dir from station/ixn *)
  search_dir: Dir.t; (* search dir to get here *)
  station: bool; (* This ixn is a station *)
  double: bool; (* Fully double track to this ixn/station *)
} [@@deriving show]

let equal_ixn res1 res2 = res1.x = res2.x && res1.y = res2.y
let nequal_ixn res1 res2 = not (res1 = res2)

let _make_ixn x y dist dir search_dir ~station ~double =
  { x; y; dist; dir; search_dir; station; double }

  (* Check more closely if the train is within the segment
     dir: direction we should focus on
       for ixns/stations, we want the entry dir
       for the first tile, we want the exit dir
     *)
let _train_count_in_ixn trains train_idxs dir =
  let _is_train_in_ixn trains train_idx dir =
    let train = Trainmap.get trains train_idx in
    let x, y = train.x mod C.tile_w, train.y mod C.tile_h in
    let div2 = C.tile_w / 2 in
    match dir with
    | Dir.Up when x = div2 && y < div2 -> true
    | UpRight when x > div2 && y < div2 -> true
    | Right when x > div2 && y = div2 -> true
    | DownRight when x > div2 && y > div2 -> true
    | Down when x = div2 && y > div2 -> true
    | DownLeft when x < div2 && y > div2 -> true
    | Left when x < div2 && y = div2 -> true
    | UpLeft when x < div2 && y < div2 -> true
    | _ -> false
  in
  List.fold_left (fun cnt train_idx ->
    if _is_train_in_ixn trains train_idx dir then cnt + 1 else cnt)
    0
    train_idxs

  (* Scan for a new segment ending in a station or ixn *)
  (* x, y: before movement *)
let _scan_for_ixn tracks ~x ~y ~dir ~double ~player =
  let search_dir = dir in
  let player2 = player in
  let rec loop_to_node x y dir double_acc ~dist =
    let oppo_dir = Dir.opposite dir in
    match Trackmap.get tracks ~x ~y with
    | Some ({ixn = true; player; _} as track) when player = player2 ->
        (* Found ixn *)
        let double = double_acc && Track.acts_like_double track in
        Some (_make_ixn x y dist oppo_dir search_dir ~station:false ~double) 
    | Some {kind = Station _; player; _} when player = player2 ->
        (* Found station *)
        Some (_make_ixn x y dist oppo_dir search_dir ~station:true ~double:double_acc)
    | Some track when track.player = player2 ->
        (* Find other dir and follow it *)
        let (let*) = Option.bind in
        let* other_dir, _ = Dir.Set.remove track.dirs oppo_dir |> Dir.Set.pop_opt in
        let* x2, y2 = Trackmap.move_dir_bounds tracks ~x ~y ~dir:other_dir in
        let double = double_acc && Track.acts_like_double track in
        loop_to_node x2 y2 other_dir double ~dist:(dist + 1)
    | _ -> None
  in
  let x2, y2 = Dir.adjust dir x y in
  loop_to_node x2 y2 dir double ~dist:1

  (* Ixn/Station/Track: what we're pointing at.
     List: what we're connected to.
     Scan results center on the current track and give results on both directions
     up to the next ixns found, if any.
   *)
type t =
  | NoResult
  | Ixn of ixn list (* 0/1/2/3 ixns *)
  | Station of ixn list  (* 0/1/2 ixns *)
  | Track of ixn list (* 0/1/2 ixns *)
  [@@deriving eq,show]
        
(* Return a query about the segment from a particular tile
   Get back a list of scan results
 *)
let scan tracks ~x ~y ~player =
  let player2 = player in
  match Trackmap.get tracks ~x ~y with
  | None -> NoResult
  | Some {player; _} when player <> player2 -> NoResult
  | Some track ->
      let scan =
        let double = Track.acts_like_double track in
        Dir.Set.fold (fun acc dir ->
          match _scan_for_ixn tracks ~x ~y ~player ~dir ~double with
          | None -> acc
          | Some res -> res::acc)
        [] track.dirs
      in
      if Track.is_ixn track then Ixn scan
      else if Track.is_station track then Station scan
      else Track scan

  (* Scan for number of trains in the station segment. Assumes we're not pointing to a station! *)
let scan_station_segment_for_trains tracks trains ~x ~y dir ~player =
  let (let*) = Option.bind in
  let seen_ixns = Hashtbl.create 10 in
  let rec loop x y dir =
    let loc = (x, y) in
    let oppo_dir = Dir.opposite dir in
    let not_been_here () = not @@ Hashtbl.mem seen_ixns loc in
    let train_idxs = Trainmap.get_at_loc loc trains in
    match Trackmap.get tracks ~x ~y with
    | Some ({kind = Station _; _} as track) when not_been_here () && track.player = player ->
        Hashtbl.replace seen_ixns loc ();
        (* Found station at edge: count just on incoming track *)
        _train_count_in_ixn trains train_idxs oppo_dir

    | Some ({ixn = true; _} as track) when not_been_here () && track.player = player ->
        Hashtbl.replace seen_ixns loc ();
        (* Found ixn: iterate over remaining dirs *)
        let other_dirs = Dir.Set.remove track.dirs oppo_dir |> Dir.Set.to_list in
        List.fold_left (fun acc dir ->
          match Trackmap.move_dir_bounds tracks ~x ~y ~dir with
          | Some (x2, y2) -> acc + loop x2 y2 dir
          | _ -> acc)
          (List.length train_idxs)
          other_dirs

    | Some track when track.player = player ->
        (* Find other dir and follow it *)
        let count =
          let* dir, _ = Dir.Set.remove track.dirs oppo_dir |> Dir.Set.pop_opt in
          let* x2, y2 = Trackmap.move_dir_bounds tracks ~x ~y ~dir in
          Option.return (loop x2 y2 dir)
        in
        (List.length train_idxs) + Option.get_or ~default:0 count

    | _ -> 0
  in
  let loc = x, y in
  match Trackmap.get tracks ~x ~y with
  | Some track when Track.is_station track ->
      let train_idxs = Trainmap.get_at_loc loc trains in
      let count = _train_count_in_ixn trains train_idxs dir in
      let count2 = match Trackmap.move_dir_bounds tracks ~x ~y ~dir with
        | Some (x, y) -> loop x y dir
        | _ -> 0
      in
      count + count2

  | Some track ->
      (* We don't care about user-provided dir when it's an ixn or track *)
      let count = Trainmap.get_at_loc loc trains |> List.length in
      Dir.Set.fold (fun count dir ->
        (* We only want to count in the dir we're going so we don't double-count*)
        match Trackmap.move_dir_bounds tracks ~x ~y ~dir with
        | Some (x, y) -> count + loop x y dir
        | _ -> count)
      count
      track.dirs

  | _ -> 0

