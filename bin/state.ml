open Containers

(* All state *)
type t = {
  random: Random.State.t;
  seed: int;
  mutable backend: Backend.t;
  screen: Screen.t;
  mutable ui: t Main_ui_d.t; (* get around circular modules *)
  resources: Resources.t;
  textures: Textures.t;
  fonts: Fonts.t;
}
[@@deriving lens]
