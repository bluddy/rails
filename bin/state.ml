open Containers

(* All state *)
type t = {
  mutable map_tex: Renderer.Texture.t;
  mutable backend: Backend.t;
  screen: Screen.t;
  mutable ui: t Main_ui_d.t; (* get around circular modules *)
  textures: Textures.t;

  resources: Resources.t;
  fonts: Fonts.t;
}
[@@deriving lens]
