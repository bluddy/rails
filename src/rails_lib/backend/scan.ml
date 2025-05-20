open! Containers
open! Utils
module C = Constants
module LocdSet = Utils.LocdSet

(* Module for searching the trackmap for stations and ixns, and the trainmap for trains
   For updating the graph and the station block connectivity
 *)
(* This is our return type representing an ixn. Will be used to introspect *)
type ixn = {
  x: int; (* keep this x,y separate *)
  y: int;
  dist: int;
  dir: Dir.t; (* out dir from station/ixn *)
  search_dir: Dir.t; (* search dir to get here *)
  station: bool; (* This ixn is a station *)
  double: bool; (* Fully double track to this ixn/station *)
} [@@deriving show]

let max_scan_dist = 1000

let equal_ixn res1 res2 = res1.x = res2.x && res1.y = res2.y
let nequal_ixn res1 res2 = not (equal_ixn res1 res2)

let _make_ixn (x,y) dist dir search_dir ~station ~double =
  { x; y; dist; dir; search_dir; station; double }

  (* Check more closely if the train is within the block
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

  (* Scan for a new block ending in a station or ixn *)
  (* x, y: before movement *)
let _scan_for_ixn tracks loc ~dir ~double player_idx =
  let search_dir = dir in
  let player2_idx = player_idx in
  let rec loop_to_node loc dir double_acc ~dist =
    if dist > max_scan_dist then None else
    let oppo_dir = Dir.opposite dir in
    match Trackmap.get loc tracks with
    | Some ({ixn = true; player; _} as track) when Owner.(player = player2_idx) ->
        (* Found ixn *)
        let double = double_acc && Track.acts_like_double track in
        Some (_make_ixn loc dist oppo_dir search_dir ~station:false ~double) 
    | Some {kind = Station _; player; _} when Owner.(player = player2_idx) ->
        (* Found station *)
        Some (_make_ixn loc dist oppo_dir search_dir ~station:true ~double:double_acc)
    | Some track when Owner.(track.player = player2_idx) ->
        (* Find other dir and follow it *)
        let (let*) = Option.bind in
        let* other_dir, _ = Dir.Set.remove track.dirs oppo_dir |> Dir.Set.pop_opt in
        let* loc2 = Trackmap.move_dir_bounds loc tracks ~dir:other_dir in
        let double = double_acc && Track.acts_like_double track in
        loop_to_node loc2 other_dir double ~dist:(dist + 1)
    | _ -> None
  in
  let loc2 = Dir.adjust_loc dir loc in
  loop_to_node loc2 dir double ~dist:1

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
  [@@deriving eq, show]
        
(* Return a query about the block from a particular tile
   Get back a list of scan results
 *)
let scan tracks loc player_idx =
  let player2_idx = player_idx in
  match Trackmap.get loc tracks with
  | None -> NoResult
  | Some {player; _} when Owner.(player <> player2_idx) -> NoResult
  | Some track ->
      let scan =
        let double = Track.acts_like_double track in
        Dir.Set.fold (fun acc dir ->
          match _scan_for_ixn tracks loc player_idx ~dir ~double with
          | None -> acc
          | Some res -> res::acc)
        [] track.dirs
      in
      if Track.is_ixn track then Ixn scan
      else if Track.is_station track then Station scan
      else Track scan

  (* Scan for number of trains in the station block. *)
let scan_station_block tracks trains loc dir player_idx =
  let (let*) = Option.bind in
  let seen_ixns = Hashtbl.create 10 in
  let rec loop loc dir =
    let oppo_dir = Dir.opposite dir in
    let not_been_here = not @@ Hashtbl.mem seen_ixns loc in
    let train_idxs = Trainmap.get_at_loc loc trains in
    match Trackmap.get loc tracks with
    | Some ({kind = Station _; _} as track) when not_been_here
          && Owner.(track.player = player_idx) ->
        Hashtbl.replace seen_ixns loc ();
        (* Found station at edge: count just on incoming track *)
        let count = _train_count_in_ixn trains train_idxs oppo_dir in
        (* Station is always double *)
        count, true

    | Some ({ixn = true; _} as track) when not_been_here
          && Owner.(track.player = player_idx) ->
        (* Found ixn: iterate over remaining dirs *)
        Hashtbl.replace seen_ixns loc ();
        let double = Track.acts_like_double track in
        let count = List.length train_idxs in
        let other_dirs = Dir.Set.remove track.dirs oppo_dir |> Dir.Set.to_list in
        List.fold_left (fun ((count, double) as acc) dir ->
          match Trackmap.move_dir_bounds loc ~dir tracks with
          | Some loc2 ->
            let count2, double2 = loop loc2 dir in
            count + count2, double && double2
          | _ -> acc)
          (count, double)
          other_dirs

    | Some track when Owner.(track.player = player_idx) ->
        (* Find other dir and follow it *)
        let double = Track.acts_like_double track in
        let count = List.length train_idxs in
        let res =
          let* dir, _ = Dir.Set.remove track.dirs oppo_dir |> Dir.Set.pop_opt in
          let* loc2 = Trackmap.move_dir_bounds loc ~dir tracks in
          Option.return (loop loc2 dir)
        in
        begin match res with
        | Some (count2, double2) -> count + count2, double && double2
        | None -> count, double
        end

    | _ ->
      (* Don't cancel double once we run out of track *)
      0, true
  in
  let count, double =
    match Trackmap.get loc tracks with
    | Some track when Track.is_station track ->
        let train_idxs = Trainmap.get_at_loc loc trains in
        let count = _train_count_in_ixn trains train_idxs dir in
        let count2, double = match Trackmap.move_dir_bounds loc ~dir tracks with
          | Some loc -> loop loc dir
          | _ -> 0, true
        in
        count + count2, double

    | Some track ->
        (* We don't care about user-provided dir when it's an ixn or track *)
        let count = Trainmap.get_at_loc loc trains |> List.length in
        let double = Track.acts_like_double track in
        Dir.Set.fold (fun ((count, double) as acc) dir ->
          (* We only want to count in the dir we're going so we don't double-count*)
          match Trackmap.move_dir_bounds loc ~dir tracks with
          | Some loc ->
            let count2, double2 = loop loc dir in
            count + count2, double && double2
          | _ -> acc)
        (count, double)
        track.dirs

    | _ -> 0, false
  in
  count, Track.double_of_bool double

