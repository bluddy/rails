open! Containers

module R = Engine.Renderer

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
  mutable map_tex: R.Texture.t;
  map_silhouette_tex: R.Texture.t;
  textures: Textures.t;
  resources: Resources.t;
  fonts: Engine.Fonts.t;
  win: R.window;
  random: Utils.Random.State.t; (* separate random state for UI stuff *)
  sound: Sound.t;
}

