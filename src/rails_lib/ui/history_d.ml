open! Containers
type loc = Utils.loc

type phase =
  | Player of {end_: bool}
  | Ai of {owner: Owner.t}

type t = {
  map_tex: Renderer.Texture.t;
  year: int;
  phase: phase;
  player_track_history: int array;
  ai_route_history: int array;
  player_track_idx: int;
  ai_route_idx: int;
  ai_track_idx: int;
}

