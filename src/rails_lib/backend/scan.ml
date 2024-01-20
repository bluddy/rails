open! Containers
open! Utils

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
    count: int; (* Count of trains up to mid-ixn or mid-station *)
  } [@@deriving show]

  let equal_ixn res1 res2 = res1.x = res2.x && res1.y = res2.y
  let nequal_ixn res1 res2 = not (res1 = res2)

  let _make_ixn x y dist dir search_dir ~station ~double ~count =
    {x; y; dist; dir; search_dir; station; double; count}

    (* Scan for a new segment ending in a station or ixn *)
    (* x, y: before movement *)
  let _scan_for_ixn v ~x ~y ~dir ~double ~player =
    let search_dir = dir in
    let player2 = player in
    let rec loop_to_node x y dir double_acc dist =
      let oppo_dir = Dir.opposite dir in
      match Trackmap.get v ~x ~y with
      | Some ({ixn = true; player; _} as track) when player = player2 ->
          let double = double_acc && Track.acts_like_double track in
          Some (_make_ixn x y dist oppo_dir search_dir ~station:false ~double ~count:0) 
      | Some {kind = Station _; player; _} when player = player2 ->
          Some (_make_ixn x y dist oppo_dir search_dir ~station:true ~double:double_acc ~count:0)
      | Some track when track.player = player2 ->
          (* Find other dir and follow it *)
          let (let*) = Option.bind in
          let* other_dir, _ = Dir.Set.remove track.dirs oppo_dir |> Dir.Set.pop_opt in
          let* x2, y2 = Trackmap.move_dir_bounds v ~x ~y ~dir:other_dir in
          let double = double_acc && Track.acts_like_double track in
          loop_to_node x2 y2 other_dir double (dist + 1)
      | _ -> None
    in
    let x2, y2 = Dir.adjust dir x y in
    loop_to_node x2 y2 dir double 1

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
  let scan v ~x ~y ~player =
    let player2 = player in
    match Trackmap.get v ~x ~y with
    | None -> NoResult
    | Some {player; _} when player <> player2 -> NoResult
    | Some track ->
        let scan =
          let double = Track.acts_like_double track in
          Dir.Set.fold (fun acc dir ->
            match _scan_for_ixn v ~x ~y ~player ~dir ~double with
            | None -> acc
            | Some res -> res::acc)
          [] track.dirs
        in
        if Track.is_ixn track then Ixn scan
        else if Track.is_station track then Station scan
        else Track scan

