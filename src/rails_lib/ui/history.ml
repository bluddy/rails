open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

include History_d

let sp = Printf.sprintf

let company_colors = [|Ega.yellow; Ega.bcyan; Ega.red; Ega.bblue|]

let create (s:State.t) = 
  let player_idx = C.player in
  let b = s.backend in
  let params = b.params in
  let player = B.get_player player_idx s.backend in
  let player_track_history = Player.get_track_pieces_history player |> List.rev |> Array.of_list in
  let ai_track_history = Ai.get_route_history b.ai |> List.rev |> Array.of_list in
  {
    map_tex=s.map_silhouette_tex;
    year=params.year_start;
    phase=Player {track_idx=0};
    player_track_history;
    ai_track_history;
  }

let render win v (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  let player = B.get_player player_idx b in
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
  (* Draw player stuff *)

  let period = (v.year - params.year_start) / 2 in
  let _draw_player_track =
    let period_idx = v.player_track_history.(period) in
    Iter.iter (fun idx ->
      let x, y = Player.get_track_loc idx player in
      let y = y + 8 in
      R.draw_point win ~color:Ega.white ~x ~y;
    )
    Iter.(0 -- period_idx)
  in

  let _draw_stations =
    Station_map.iter (fun station ->
      if Station.get_year_built station <= v.year then (
        let x, y = Station.get_loc station in
        let y = y + 8 in
        let owner = Station.get_player_idx station in
        let color = Owner.Map.find owner colors in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true
      )
    )
    b.stations
  in

  ()

