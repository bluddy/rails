open Containers

(* All state *)
type t = {
  random: Random.State.t;
  seed: int;
  backend: Backend.t;
  screen: Screen.t;
  ui: Main_ui_d.t;
  mutable view: Mapview_d.t;
  resources: Resources.t;
  textures: Textures.t;
}
[@@deriving lens]
