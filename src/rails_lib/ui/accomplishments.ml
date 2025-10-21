  open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

let sp = Printf.sprintf

module IntMap = Utils.IntMap

let render win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  let write = Fonts.Render.write ~idx:`Standard win fonts in
  let write_g = write ~color:Ega.gray in
  let write = write ~color:Ega.black in

  write ~x:100 ~y:4 "ACCOMPLISHMENTS";

  let x = 8 in
  let y = 16 in

  let params = b.params in
  let player = B.get_player player_idx b in
  let name = B.get_name player_idx b in
  write ~x ~y @@ sp "%d: Founded %s." params.year_start name;
  let y = y + 8 in
  let a = Player.get_achievements player in


  let next_line x y = if y > 188 && x < 160 then 160, 24 else x, y + 8 in
  let write_achievements x y f ~year ~tag_color printer achievement =
    IntMap.fold (fun idx year2 (x, y) ->
      if year = year2 then (
        let value = f idx in
        write_g ~x ~y ~tag_color @@ printer year value;
        next_line x y)
      else x, y)
      achievement
      (x, y)
  in
  let region = params.region in
  let module A = Player.Achievement in
  Iter.fold (fun (x, y) year ->
    let x, y =
      Station_map.fold (fun station (x, y) ->
        if Station.is_proper_station station && not @@ Station.has_suffix station && year = Station.get_year_built station then (
          let city = Station.get_city station |> Option.get_exn_or "city" in
          let city_s = Cities.name_of_loc city b.cities in
          write_g ~x ~y ~tag_color:Ega.bred @@ sp "%d: |service to %s.|" year city_s;
          next_line x y)
        else x, y)
      b.stations
      ~init:(x, y)
    in
    let x, y = write_achievements x y A.track_length_of_idx ~year ~tag_color:Ega.bblue (sp "%d: |%d miles of track.|") a.track_length in
    let x, y = write_achievements x y A.revenue_of_idx ~year ~tag_color:Ega.bgreen
        (fun year value -> sp "%d: |%s of revenue.|" year @@ Money.print ~region value) a.revenue
    in
    let x, y = write_achievements x y A.net_worth_of_idx ~year ~tag_color:Ega.bcyan
        (fun year value -> sp "%d: |%s,000 Net Worth.|" year @@ Money.print ~region value) a.net_worth
    in
    x, y)
  (x, y)
  Iter.(params.year_start -- params.year) |> ignore
