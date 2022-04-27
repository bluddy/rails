open Containers

type kind =
  | Track
  | Tunnel
  | SignalTower
  | Depot
  | Station
  | Terminal
  | WoodBridge
  | MetalBridge
  | StoneBridge

type elt =
  {
    dirs: Dir.Set.t;
    kind: kind;
    ixn: bool;
    player: int;
  }

type t = {
  map: elt option array;
  w: int;
  h: int;
}

let empty w h =
  let map = Array.make (w * h) None in
  {map; w; h}

let calc_offset v x y = y * v.w + x

let get v x y = v.map.(calc_offset v x y)

let set v x y tile =
  v.map.(calc_offset v x y) <- tile

  (* Set of all sets of legal tracks *)
module TrackSet = CCHashSet.Make(struct
  type t = Dir.Set.t
  let equal = Dir.Set.equal
  let hash = Dir.Set.to_int
end)

let legal_tracks =
  let h = TrackSet.create 30 in
  let add_to_set li =
    List.iter (fun l ->
      let set = Dir.Set.of_list l in
      TrackSet.insert h set
    )
    li
  in
  add_to_set Dir.track_dirs;
  add_to_set Dir.track_turns;
  h

let build_track v ~x ~y ~dir ~player =
  match get v x y with
  | None ->
      let dirs = Dir.Set.singleton dir in
      set v x y @@ Some {dirs; player; ixn=false; kind=Track};
      true
  | Some({kind=Track;_} as t) when t.player = player ->
      let dirs = Dir.Set.add t.dirs dir in
      if TrackSet.mem legal_tracks dirs then (
        let ixn = Dir.Set.count dirs > 2 in
        set v x y @@ Some {t with dirs; ixn};
        true
      ) else
        false
  | _ -> false

