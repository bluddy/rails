open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils

(* This map contains independent tiles of track *)

type t = {
  map: Track.t IntMap.t;
  width: int;
  height: int;
} [@@deriving yojson]

let empty width height =
  let map = IntMap.empty in
  {map; width; height}

let get v ~x ~y = IntMap.find_opt (Utils.calc_offset v.width x y) v.map

let get_exn v ~x ~y = IntMap.find (Utils.calc_offset v.width x y) v.map 

  (* get, buf if there's nothing, create a track *)
let get_track_default ?(kind=(Track.Track `Single)) v ~x ~y ~player =
  get v ~x ~y
  |> Option.get_lazy (fun () -> Track.empty player kind)

let set v ~x ~y ~t =
  let map = IntMap.add (Utils.calc_offset v.width x y) t v.map in
  {v with map}

let remove v ~x ~y =
  let map = IntMap.remove (Utils.calc_offset v.width x y) v.map in
  {v with map}

let iter v f =
  IntMap.iter (fun i track ->
    let x, y = Utils.x_y_of_offset v.width i in
    f x y track)
  v.map

let out_of_bounds v ~x ~y =
  x < 0 || y < 0 || x >= v.width || y >= v.height

(* Common function for moving by dir, with bounds check *)
let move_dir_bounds v ~x ~y ~dir =
  let x2, y2 = Dir.adjust dir x y in
  if out_of_bounds v ~x:x2 ~y:y2 then None
  else Some(x2, y2)

let check_build_track v ~x ~y ~dir ~player =
  match out_of_bounds v ~x ~y, move_dir_bounds v ~x ~y ~dir with
  | true, _ | _, None  -> false
  | _, Some (x2, y2) ->
    let track1 = get_track_default v ~x ~y ~player in
    let track2 = get_track_default v ~x:x2 ~y:y2 ~player in
    let track12 = Track.add_dir track1 ~dir in
    let track22 = Track.add_dir track2 ~dir:(Dir.opposite dir) in
    (* Check that we changed something *)
    if Track.equal_dirs track1 track12 && Track.equal_dirs track2 track22 then false
    else
      track1.player = player && track2.player = player
      && Track.is_legal track12 && Track.is_legal track22

  (* do build. Assumes we already checked
     kind1: kind of track in from tile
     kind2: kind of track in to tile
   *)
let build_track ?kind1 ?kind2 v ~x ~y ~dir ~player =
  match move_dir_bounds v ~x ~y ~dir with
  | None -> v
  | Some (x2, y2) ->
    let track1 = get_track_default ?kind:kind1 v ~x ~y ~player in
    let track2 = get_track_default ?kind:kind2 v ~x:x2 ~y:y2 ~player in
    let track1 = Track.add_dir track1 ~dir in
    let track2 = Track.add_dir track2 ~dir:(Dir.opposite dir) in
    let v = set v ~x ~y ~t:track1 in
    let v = set v ~x:x2 ~y:y2 ~t:track2 in
    v

let check_build_station v ~x ~y ~player station_type =
  if out_of_bounds v ~x ~y then `Illegal
  else match get v ~x ~y with
  | None -> `NoTrack
  | Some ({kind=Track _;_} as t) when t.player = player && Track.is_straight t ->
       let range = Station.to_range station_type in
       let match_fn j i =
         match get v ~x:j ~y:i with
         | Some {kind=Station(st);_} ->
             let range2 = Station.to_range st in
             let range = range + range2 in
             abs (j - x) < range && abs (i - y) < range
         | _ -> false
       in
       let station_test =
         Utils.scan ~range ~x ~y ~width:v.width ~height:v.height ~f:match_fn
       in
       begin match station_test with
       | Some _ -> `TooClose
       | None -> `Ok
       end
  | _ -> `Illegal
   
let build_station v ~x ~y station_type =
  match get v ~x ~y with
  | Some ({kind=Track _; _} as t) ->
      (* Do we build new track *)
      let build_new_track = Dir.Set.cardinal t.dirs > 1 in
      let track = Track.straighten t in
      let station = {track with kind=Station(station_type)} in
      let v = set v ~x ~y ~t:station in
      v, build_new_track
  | _ -> assert false

  (* Check that a stretch of track is clear: tunnel or bridge
     No track can be in the middle, but it's ok if track is at the end
     Length: includes middle stretch + last piece
   *)
let check_build_stretch v ~x ~y ~dir ~player ~length =
  let dx, dy = Dir.to_offsets dir in
  let x1, y1 = x, y in
  let x3, y3 = x + dx * length, y + dy * length in
  (* check boundaries *)
  if out_of_bounds v ~x ~y || out_of_bounds v ~x:x3 ~y:y3 then false
  else (
    (* No track in between *)
    let rec loop x y i =
      if i <= 0 then true
      else
        match get v ~x ~y with
        | Some _ -> false
        | None ->
            loop (x+dx) (y+dy) (i-1)
    in
    let test_track x y dir =
      match get v ~x ~y with
      | Some track when track.player = player ->
          let track = Track.add_dir track ~dir in
          Track.is_legal track
      | Some _ ->
          (* wrong player *)
          false
      | _ ->
          (* no track = fine *)
          true
    in
    test_track x1 y1 dir &&
    loop (x1+dx) (y1+dy) (length-1) &&
    test_track x3 y3 (Dir.opposite dir)
  )

    (* Used by bridge and tunnel *)
let build_stretch v ~x ~y ~dir ~player ~n ~kind =
  let dx, dy = Dir.to_offsets dir in
  let x1, y1 = x, y in
  let x3, y3 = x + dx * n, y + dy * n in
  if out_of_bounds v ~x ~y || out_of_bounds v ~x:x3 ~y:y3 then v (* error *)
  else (
    let track1 = get_track_default v ~x:x1 ~y:y1 ~player
      |> Track.add_dir ~dir
    in
    let v = set v ~x:x1 ~y:y1 ~t:track1 in
    let track2 = Track.empty player kind
      |> Track.add_dir ~dir
      |> Track.add_dir ~dir:(Dir.opposite dir)
    in
    let rec dig_tunnel ~x ~y i v =
      if i <= 0 then v
      else
        let v = set v ~x ~y ~t:track2 in
        dig_tunnel ~x:(x+dx) ~y:(y+dy) (i-1) v
    in
    let v = dig_tunnel ~x:(x1+dx) ~y:(y1+dy) (n-1) v in
    let track3 =
      get_track_default v ~x:x3 ~y:y3 ~player
      |> Track.add_dir ~dir:(Dir.opposite dir) in
    let v = set v ~x:x3 ~y:y3 ~t:track3 in
    v
  )

let build_bridge v ~x ~y ~dir ~player ~kind =
  build_stretch v ~x ~y ~dir ~player ~n:2 ~kind:(Track.Bridge kind)

let build_tunnel v ~x ~y ~dir ~player ~length =
  build_stretch v ~x ~y ~dir ~player ~n:length ~kind:Track.Tunnel
   
  (* Can work for all kinds of constructs *)
let check_remove_track v ~x ~y ~dir ~player =
  match out_of_bounds v ~x ~y, move_dir_bounds v ~x ~y ~dir with
  | true, _ | _, None  -> false
  | _, Some (x2, y2) ->
    let track1 = get_track_default v ~x ~y ~player in
    let track2 = get_track_default v ~x:x2 ~y:y2 ~player in
    if track1.player <> player || track2.player <> player then false
    else
      match track1.kind, track2.kind with
      | Bridge _, _ | _, Bridge _ -> true
      | Tunnel, _ | _, Tunnel -> true
      | Station _, _ | _, Station _ -> true
      | _ ->
          Track.has_dir track1 ~dir || Track.has_dir track2 ~dir:(Dir.opposite dir)

let remove_track v ~x ~y ~dir ~player =
  let remove_track_dir v ~x ~y ~dir =
    match get v ~x ~y with
    | Some track ->
      let track = Track.remove_dir track ~dir in
      if Track.is_empty track then
        remove v ~x ~y
      else
        set v ~x ~y ~t:track
    | None -> v
  in
  let x2, y2 = Dir.adjust dir x y in
  let track1 = get_track_default v ~x ~y ~player in
  let v =
    if track1.player = player then
      match track1.kind with
      | Track _ | Ferry _ ->
          (* TODO: handle ixn *)
          remove_track_dir v ~x ~y ~dir;
      | Station _ ->
          (* TODO: handle station removal *)
          remove v ~x ~y
      | Bridge _ | Tunnel ->
          remove v ~x ~y
    else v
  in
  let track2 = get_track_default v ~x:x2 ~y:y2 ~player in
  match track2.kind with
  | Track _ | Ferry _ when track2.player = player ->
      remove_track_dir v ~x:x2 ~y:y2 ~dir:(Dir.opposite dir);
  | _ ->
      (* All other constructs remain whole with full dirs *)
      v

let has_station (x,y) v =
  match get ~x ~y v with
  | Some t -> Track.is_station t
  | None -> false

module Search = struct
  (* Module for searching the trackmap for stations and ixns.
     For updating the graph and the station segment connectivity
   *)
  (* This is our return type representing an ixn. Will be used to introspect *)
  type ixn = {
    x: int;
    y: int;
    dist: int;
    dir: Dir.t; (* out dir from station/ixn *)
    search_dir: Dir.t; (* search dir to get here *)
    station: bool;
  } [@@deriving show]

  let equal_ixn res1 res2 = res1.x = res2.x && res1.y = res2.y
  let nequal_ixn res1 res2 = not (res1 = res2)

  let _make_ixn x y dist dir search_dir station =
    {x; y; dist; dir; search_dir; station}

    (* Scan for a new segment ending in a station or ixn *)
    (* x, y: before movement *)
  let _scan_for_ixn v ~x ~y ~dir ~player =
    let search_dir = dir in
    let player2 = player in
    let rec loop_to_node x y dir dist =
      let oppo_dir = Dir.opposite dir in
      match get v ~x ~y with
      | Some {ixn = true; player; _} when player = player2 ->
          Some (_make_ixn x y dist oppo_dir search_dir false) 
      | Some {kind = Station _; player; _} when player = player2 ->
          Some (_make_ixn x y dist oppo_dir search_dir true)
      | Some {dirs; player; _} when player = player2 ->
          (* Find other dir and follow it *)
          let other_dirs = Dir.Set.remove dirs oppo_dir in
          begin match Dir.Set.pop_opt other_dirs with
          | Some (other_dir, _) ->
            begin match move_dir_bounds v ~x ~y ~dir:other_dir with
            | None -> None
            | Some (x2, y2) ->
                loop_to_node x2 y2 other_dir (dist+1)
            end
          | None -> None (* dead end *)
          end
      | _ -> None
    in
    let x2, y2 = Dir.adjust dir x y in
    loop_to_node x2 y2 dir 1

    (* Ixn/Station/Track: what we're pointing at.
       List: what we're connected to. *)
  type scan =
    | NoResult
    | Ixn of ixn list (* 0/1/2 ixns *)
    | Station of ixn list  (* 0/1 ixns *)
    | Track of ixn list (* 0/1 ixns *)
    [@@deriving eq,show]
          
  (* Return a query about the segment from a particular tile
     Get back a list of directions and result pairs
   *)
  let scan v ~x ~y ~player =
    let player2 = player in
    match get v ~x ~y with
    | None -> NoResult
    | Some {player; _} when player <> player2 -> NoResult
    | Some track ->
        let scan =
          Dir.Set.fold (fun acc dir ->
            match _scan_for_ixn v ~x ~y ~player ~dir with
            | None -> acc
            | Some res -> res::acc)
          [] track.dirs
        in
        if Track.is_ixn track then Ixn scan
        else if Track.is_station track then Station scan
        else Track scan
end


