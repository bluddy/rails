open! Containers
type loc = Utils.loc

type phase =
  | Player of {track_idx: int}
  | Ai of {owner: Owner.t; route_idx: int; track_idx: int}

type t = {
  map_tex: Renderer.Texture.t;
  year: int;
  phase: phase;
  player_track_history: int array;
  ai_route_history: int array;
}

