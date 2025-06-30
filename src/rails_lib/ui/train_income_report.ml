open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

let sp = Printf.sprintf

let render msg win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  let write = Fonts.Render.write win fonts in
  let write_g = write ~color:Ega.gray in
  let write = write ~color:Ega.black in

  let heading_h = 6 + 2 * 8 in
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~color:Ega.bgreen ~x:0 ~y:0 ~w:320 ~h:heading_h ~fill:true;

  let _draw_headings =
    let name = B.get_name player_idx b in
    let heading = "Train Report:%s RR" b in
    Fonts.Render.write ~x:80 ~y:2 heading;
    let headings = "Train class/route  Revenue: YTD   Last Year  Lifetime" in
    Fonts.Render.write ~x:2 ~y:12 headings;
    let y = heading_h in
    R.draw_line win ~color:Ega.black ~x1:0 ~x2:319 ~y1:y ~y2:y
  in
  
  let draw_train i y train =
    let typ_s = Train.get_type train |> Train.show_train_type in
    let last_loc = Train.get_last_station train in
    let last_station = Station_map.get_exn last_loc b.stations in
    let last_short_name = Station.get_short_name last_station in
    let next_loc = Train.get_next_station train in
    let next_station = Station_map.get_exn next_loc b.stations in
    let next_short_name = Station.get_short_name next_station in
    let is_traveling = Train.is_traveling train in
    let desc_s = "%s-%s%s"
      last_short_name
      (if is_traveling then ">" else "")
      next_short_name
    in
    let num_type_dest = sp "%d)%s/%s" i typ_s desc_s in
    ()

