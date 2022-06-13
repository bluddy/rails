open Containers

(* All state *)
type t = {
  (* saveable *)
  mutable backend: Backend.t;
  mutable ui: t Main_ui_d.t; (* get around circular modules *)

  (* non-saveable *)
  screen: (Screen.t [@sexp.opaque]);
  mutable map_tex: (Renderer.Texture.t [@sexp.opaque]);
  textures: (Textures.t [@sexp.opaque]);
  resources: (Resources.t [@sexp.opaque]);
  fonts: (Fonts.t [@sexp.opaque]);
}
[@@deriving lens, sexp]
