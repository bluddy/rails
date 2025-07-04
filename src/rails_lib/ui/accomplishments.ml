  open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

let sp = Printf.sprintf

let render win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  let write = Fonts.Render.write ~idx:`Standard win fonts in
  let write_r = write ~color:Ega.red in
  let write_g = write ~color:Ega.gray in
  let write_gr = write ~color:Ega.gray ~tag_color:Ega.red in
  let write = write ~color:Ega.black in

  write ~x:100 ~y:4 "ACCOMPLISHMENTS";

  let x = 8 in
  let y = 16 in

  let params = b.params in
  let player = B.get_player player_idx b in
  let name = B.get_name player_idx b in
  write ~x ~y @@ sp "%d: Founded %s." params.year_start name;
  let y = y + 8 in

  Iter.fold (fun (x, y) year ->
    Station_map.fold (fun station (x, y) ->
      if Station.is_proper_station station && not @@ Station.has_suffix station && year = Station.get_year_built station then (
        let city = Station.get_city station |> Option.get_exn_or "city" in
        let city_s = Cities.name_of_loc city b.cities in
        write_gr ~x ~y @@ sp "%d: |service to %s.|" year city_s;
        x, y + 8)
      else x, y)
    b.stations
    ~init:(x, y))
  (x, y)
  Iter.(params.year_start -- params.year) |> ignore;

  ()
