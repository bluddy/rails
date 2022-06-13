open Containers

(* All state *)
type t = {
  (* saveable *)
  mutable backend: Backend.t;
  mutable ui: t Main_ui_d.t; (* get around circular modules *)

  (* non-saveable *)
  screen: Screen.t; [@yojson.opaque]
  mutable map_tex: Renderer.Texture.t; [@yojson.opaque]
  textures: Textures.t; [@yojson.opaque]
  resources: Resources.t; [@yojson.opaque]
  fonts: Fonts.t; [@yojson.opaque]
}
[@@deriving lens, yojson]
