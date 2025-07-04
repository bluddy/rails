open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

include History_d

let sp = Printf.sprintf

let company_colors = [|Ega.black; Ega.bgreen; Ega.red; Ega.bblue|]

let create (s:State.t) = 
  let params = s.backend.params in
  {
    map_tex=s.map_silhouette_tex;
    year=params.year_start;
    phase=Player {track_idx=0}
  }

let render win v (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  let params = b.params in
  R.draw_rect win ~x:0 ~y:0 ~w:256 ~h:8 ~fill:true ~color:Ega.white;
  R.draw_rect win ~x:256 ~y:0 ~w:64 ~h:200 ~fill:true ~color:Ega.yellow;
  R.Texture.render win v.map_tex ~x:0 ~y:8;

  let write = Fonts.Render.write ~idx:`Standard win fonts in
  let write = write ~color:Ega.black in

  let _draw_year =
    R.draw_rect win ~x:111 ~y:191 ~w:29 ~h:9 ~fill:true ~color:Ega.white;
    write ~x:116 ~y:192 @@ string_of_int params.year;
  in

  let owners = B.players_and_ai b in
  let colors =
    Iter.foldi (fun acc i owner -> Owner.Map.add owner company_colors.(i) acc)
      Owner.Map.empty
      owners
  in
  (* We need to draw everything from start_year to this year *)
  Iter.iter (fun year ->
    ()
  )
  Iter.(params.year_start -- v.year);
  ()

