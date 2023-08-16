open Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type double_track = [ `Double | `Single ] [@@deriving eq, yojson, hash]

type kind =
  | Track of double_track * Dir.t
  | Track2 of double_track * Dir.t * Dir.t
  | Ixn3 of double_track * Dir.t * Dir.t * Dir.t
  | Ixn4 of Dir.t * Dir.t * Dir.t * Dir.t
  | Ferry of double_track * Dir.t
  | Ferry2 of double_track * Dir.t * Dir.t
  (* Dir in the following represents 2 opposite dirs *)
  | Tunnel of Dir.t 
  | Station of Station.kind * Dir.t
  | Bridge of Bridge.t * Dir.t
  [@@deriving eq, hash, yojson]

type t = {
    kind: kind;
    player: int;
  } [@@deriving yojson]

let is_ixn v = match v.kind with
  | Ixn3 _ | Ixn4 _ -> true
  | _ -> false

let is_station v = match v.kind with
  | Station _ -> true | _ -> false

let canonical kind =
  let open Dir.Infix in
  match kind with
  | Track2(d, dir, dir2) when dir > dir2 -> Track2(d, dir2, dir)
  | Ferry2(d, dir, dir2) when dir > dir2 -> Ferry2(d, dir2, dir)
  | Ixn3(d, dir, dir2, dir3) as x ->
      let ddir, ddir2, ddir3 = Utils.sort3 ~geq:(>=) dir dir2 dir3 in
      if dir = ddir && dir2 = ddir2 && dir3 = ddir3 then
        x
      else
        Ixn3(d, ddir, ddir2, ddir3)
  | Ixn4(dir, dir2, dir3, dir4) as x ->
      let ddir, ddir2, ddir3, ddir4 = Utils.sort4 ~geq:(>=) dir dir2 dir3 dir4 in
      if dir = ddir && dir2 = ddir2 && dir3 = ddir3 && dir4 = ddir4 then x
      else 
        Ixn4(ddir, ddir2, ddir3, ddir4)
  | x -> x

let make kind ~player = {kind=canonical kind; player}

let add_dir v ~dir =
  let open Dir in
  let kind = match v.kind with
    | Track(d, dir2) when dir <> dir2 -> Track2(d, dir2, dir)
    | Track2(d, dir2, dir3) when dir <> dir2 && dir <> dir3 ->
        Ixn3(d, dir2, dir3, dir)
    | Ixn3(d, dir2, dir3, dir4) when dir <> dir2 && dir <> dir3 && dir <> dir4 ->
        Ixn3(d, dir2, dir3, dir4, dir)
    | Ferry(d, dir2) when dir <> dir2 -> Ferry2(d, dir2, dir)
    | x -> x
  in
  {v with kind}

let remove_dir v ~dir =
  let open Dir in
  let kind = match v.kind with
    | Track2(d, dir2, dir3) when dir = dir2 -> Track(d, dir3)
    | Track2(d, dir2, dir3) when dir = dir3 -> Track(d, dir2)
    | Ixn3(d, dir2, dir3, dir4) when dir = dir2 -> Track2(d, dir3, dir4)
    | Ixn3(d, dir2, dir3, dir4) when dir = dir3 -> Track2(d, dir2, dir4)
    | Ixn3(d, dir2, dir3, dir4) when dir = dir4 -> Track2(d, dir2, dir3)
    | Ferry2(d, dir2, dir3) when dir = dir2 -> Ferry(d, dir3)
    | Ferry2(d, dir2, dir3) when dir = dir3 -> Ferry(d, dir2)
    | x -> x
  in
  {v with kind}

let get_dirs v =
  match v.kind with
  | Track(_, dir) ->
    Dir.Set.singleton dir
  | Track2(_, dir, dir2) ->
    Dir.Set.singleton dir |> Dir.Set.add dir2
  | Track3(_, dir, dir2, dir3) ->
    Dir.Set.singleton dir |> Dir.Set.add dir2 |> Dir.Set.add dir3
  | Ixn3(_, dir, dir2, dir3) ->
    Dir.Set.singleton dir |> Dir.Set.add dir2 |> Dir.Set.add dir3
  | Ixn4(_, dir, dir2, dir3, dir4) ->
    Dir.Set.singleton dir |> Dir.Set.add dir2
    |> Dir.Set.add dir3 |> Dir.Set.add dir4
  | Ferry(_, dir) -> Dir.Set.singleton dir
  | Ferry2(_, dir, dir2) ->
    Dir.Set.singleton dir |> Dir.Set.add dir2
  | Tunnel(dir)
  | Station(_, dir)
  | Bridge(_, dir) ->
    Dir.Set.singleton dir |> Dir.Set.add (Dir.opposite dir)

let has_dir v ~dir =
  get_dirs v |> Dir.Set.mem dir

  (* Make hashtabl that only uses track dirs and kind for quick texture lookup *)
module Htbl = Hashtbl.Make(struct
  type nonrec t = t
  let equal x y = equal_kind x.kind y.kind
  let hash x = hash_kind x.kind
end)

  (* Hash-based set for quick checking of legal dirs for Track *)
module TrackSet = CCHashSet.Make(struct
  type t = Dir.Set.t
  let equal = Dir.Set.equal
  let hash = Dir.Set.to_int
end)

let _add_to_set h li =
  List.iter (fun l ->
    let set = Dir.Set.of_list l in
    TrackSet.insert h set
  )
  li

  (* Set of all sets of legal tracks *)
let legal_tracks =
  let h = TrackSet.create 30 in
  _add_to_set h Resources.track_dirs;
  _add_to_set h Resources.track_turns;
  h

  (* Set of all sets of non-track things. Full straight *)
let straight_dirs =
  let h = TrackSet.create 10 in
  _add_to_set h Resources.straight_track;
  h

 (* Set of tracks that can be doubled *) 
let undoubleable_dirs =
  let h = TrackSet.create 10 in
  _add_to_set h Resources.track_turns;
  h

let is_legal_dirs dirs = TrackSet.mem legal_tracks dirs

let is_legal v =
  match v.kind with
  | Track2 _ | Ferry2 _ | Ixn3 _ | Ixn4 _ ->
    let dirs = get_dirs v in
    TrackSet.mem legal_tracks dirs
  | _ -> true

let is_straight v =
  (* Check for straight track of all sorts *)
  match v.kind with
  | Track2 _ | Ferry2 _ | Ixn3 _ | Ixn4 _ ->
    TrackSet.mem straight_dirs (get_dirs v)
  | _ -> true

  (* Convert a single dir to a straight track *)
let straighten v =
  let open Dir.Infix in
  let kind = match v.kind with
  | Track(d, dir) -> canonical @@ Track2(d, dir, Dir.opposite dir)
  | Ferry(d, dir) -> canonical @@ Ferry2(d, dir, Dir.opposite dir)
  | Track2(_, dir, dir2) when dir = Dir.opposite dir2 -> v.kind
  | Ferry2(_, dir, dir2) when dir = Dir.opposite dir2 -> v.kind
  | Tunnel _ | Station _ | Bridge _ -> v.kind
  | _ -> failwith "Unexpected non-straight track"
  in
  [%up {v with kind}]

let is_doubleable v =
  not @@ TrackSet.mem undoubleable_dirs (get_dirs v)

let is_double v =
  match v.kind with
  | Track(`Double, _)
  | Track2(`Double, _)
  | Ixn3(`Double, _)
  | Ixn4(`Double, _)
  | Ferry(`Double, _)
  | Ferry2(`Double, _) -> true
  | _ -> false

let double_track_offsets v =
  (* A funky algorithm in the original code for determining the offsets of 
     double tracks
     returns the offsets in pixels for each side
   *)
  let dirs = get_dirs v in
  let x_offset, y_offset, _ =
    List.fold_left (fun ((x_offset, y_offset, mult) as acc) dir ->
      if Dir.Set.mem dirs dir then
        let dir90 = dir |> Dir.cw |> Dir.cw in
        let offx, offy = Dir.to_offsets dir90 in
        (x_offset + offx * mult, y_offset + offy * mult, -mult)
      else acc)
    (0, 0, -1)
    Dir.dirlist_left
  in
  (* Printf.printf "x_off:%d, y_off:%d\n%!" x_offset y_offset; *)
  if abs(x_offset) + abs(y_offset) = 4 then (
    (-x_offset - 1, -y_offset), (x_offset/2 - 1, y_offset/2)
  ) else
    (-x_offset, -y_offset), (x_offset, y_offset)

