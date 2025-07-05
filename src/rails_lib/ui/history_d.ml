open! Containers
type loc = Utils.loc

type phase =
  | Player of {track_idx: int}
  | Ai of {route_idx: int; left: loc list}

type t = {
  map_tex: Renderer.Texture.t;
  year: int;
  phase: phase;
  player_track_history: int array;
  ai_track_history: int array;
}

