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
  [@@deriving eq, enum]

type t =
  {
    dirs: Dir.Set.t;
    kind: kind;
    ixn: bool;
    player: int;
  }

let make dirs kind ~player =
  let ixn = Dir.Set.cardinal dirs > 2 in
  {dirs; kind; ixn; player}

  (* Make hashtabl that only uses track dirs and kind *)
module Htbl = Hashtbl.Make(struct
  type nonrec t = t
  let equal x y =
    Dir.Set.equal x.dirs y.dirs && equal_kind x.kind y.kind
  let hash x =
    Dir.Set.to_int x.dirs lxor kind_to_enum x.kind
end)

  (* Hash-based set for quick checking of legal tracks *)
module TrackSet = CCHashSet.Make(struct
  type t = Dir.Set.t
  let equal = Dir.Set.equal
  let hash = Dir.Set.to_int
end)

  (* Set of all sets of legal tracks *)
let legal_tracks =
  let h = TrackSet.create 30 in
  let add_to_set li =
    List.iter (fun l ->
      let set = Dir.Set.of_list l in
      TrackSet.insert h set
    )
    li
  in
  add_to_set Resources.track_dirs;
  add_to_set Resources.track_turns;
  h

