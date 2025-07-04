open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

include History_d

let sp = Printf.sprintf

let create (s:State.t) = 
  let params = s.backend.params in
  {
    map_tex=s.map_tex;
    year=params.year_start;
    phase=Player {track_idx=0}
  }

let render win state (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  R.draw_rect win ~x:0 ~y:0 ~w:256 ~h:8 ~fill:true ~color:Ega.white;
  R.draw_rect win ~x:256 ~y:0 ~w:64 ~h:200 ~fill:true ~color:Ega.yellow;
  R.Texture.render win state.map_tex ~x:0 ~y:8;
  ()

