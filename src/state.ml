open Containers

(* All state *)
type t = {
  random: Random.State.t;
  seed: int;
  mutable backend: Backend.t;
  screen: Screen.t;
  mutable ui: t Main_ui_d.t; (* get around circular modules *)
  mutable view: Mapview_d.t;
  resources: Resources.t;
  textures: Textures.t;
}
[@@deriving lens]
