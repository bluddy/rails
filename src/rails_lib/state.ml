open! Containers

(* Main modules of game. They don't carry much state between them *)
type module_t =
  | Intro of Intro_d.t
  | Menu of t Start_menu_d.t
  | MapGen of Mapgen.t option
  | Game

(* All state *)
and t = {
  (* saveable *)
  mutable backend: Backend.t;
  mutable ui: t Main_ui_d.t; (* get around circular modules *)

  (* non-saveable *)
  mode: module_t;
  mutable map_tex: Renderer.Texture.t;
  map_silhouette_tex: Renderer.Texture.t;
  textures: Textures.t;
  resources: Resources.t;
  fonts: Fonts.t;
  win: Renderer.window;
  random: Utils.Random.State.t; (* separate random state for UI stuff *)
}

