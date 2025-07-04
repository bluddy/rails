open! Containers

(* All state *)
type t = {
  (* saveable *)
  mutable backend: Backend.t;
  mutable ui: t Main_ui_d.t; (* get around circular modules *)

  (* non-saveable *)
  screen: Modules_d.t;
  mutable map_tex: Renderer.Texture.t;
  map_silhouette_tex: Renderer.Texture.t;
  textures: Textures.t;
  resources: Resources.t;
  fonts: Fonts.t;
}

