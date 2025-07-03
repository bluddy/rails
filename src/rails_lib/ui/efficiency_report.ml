open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

module Hashtbl = Utils.Hashtbl

let render win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  let write = Fonts.Render.write ~idx:`Standard win ~color:Ega.black fonts in

  write ~x:100 ~y:4 "Efficiency Report";

  let picked_up_goods, lost_goods =
    Station_map.fold (fun station (goods, lost) ->
      goods + Station.total_picked_up_goods station,
      lost + Station.total_lost_supply station)
      b.stations
      ~init:(0, 0)
  in
  let picked_up_cars = (picked_up_goods + 20) / 40 in
  let lost_cars = (picked_up_cars + 20) / 40 in
  let all_carloads = ((picked_up_cars + lost_cars) + 2) / 4 in
  
  let x, y = 8, 16 in
  write ~x ~y @@ sp "Carloads available: %d" all_carloads;
  let y = y + 8 in
  let carloads_carried = (picked_up_cars + 2) / 4 in
  write ~x ~y @@ sp "Carloads carried: %d" carloads_carried;
  let y = y + 8 in
  

  ()
