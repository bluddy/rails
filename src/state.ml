open Containers

(* The actual game (server) state *)
type game = {
  area: Gmap.area;
  map : Gmap.t;
  cities: Gmap.city array;
}
[@@deriving lens]

(* All state *)
type t = {
  random: Random.State.t;
  seed: int;
  game: game;
  screen: Screen.t;
  ui: Ui_d.t;
  view: Mapview_d.t;
  resources: Resources.t;
  textures: Textures.t;
}
[@@deriving lens]
