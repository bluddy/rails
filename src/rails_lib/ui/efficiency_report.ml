open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

module Hashtbl = Utils.Hashtbl

let sp = Printf.sprintf

let render win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  let write = Fonts.Render.write ~idx:`Standard win ~color:Ega.black fonts in

  write ~x:100 ~y:4 "Efficiency Report";

  let picked_up_cars, lost_cars =
    let picked_up_goods, lost_goods =
      Station_map.fold (fun station (goods, lost) ->
        goods + Station.total_picked_up_goods station,
        lost + Station.total_lost_supply station)
        b.stations
        ~init:(0, 0)
    in
    (picked_up_goods + 20) / 40, (lost_goods + 20) / 40 in

  let x, y = 8, 16 in
  let all_carloads = ((picked_up_cars + lost_cars) + 2) / 4 in
  write ~x ~y @@ sp "Carloads available: %d" all_carloads;
  let y = y + 8 in
  let carloads_carried = (picked_up_cars + 2) / 4 in
  write ~x ~y @@ sp "Carloads carried: %d" carloads_carried;
  let y = y + 8 in
  let lost_cars = if lost_cars = 0 then 1 else lost_cars in
  let efficiency = picked_up_cars * 100 / lost_cars in
  write ~x ~y @@ sp "Service Efficiency: %d%%" efficiency;

  let player = B.get_player player_idx b in
  let y = y + 8 in
  let ton_miles_traveled = Player.get_total_ton_miles player in
  write ~x ~y @@ sp "Ton-miles traveled: %d" @@ ton_miles_traveled * 10;

  let y = y + 8 in
  let ton_miles_delivered = Trainmap.fold (fun acc train ->
    Train.get_total_ton_miles train + acc)
    (Player.get_trains player)
    ~init:0
  in
  write ~x ~y @@ sp "Ton-miles delivered: %d" @@ ton_miles_delivered * 10;

  let y = y + 8 in
  let ton_miles_delivered = if ton_miles_delivered = 0 then 1 else ton_miles_delivered in
  write ~x ~y @@ sp "Utilization efficiency: %d%%" @@ ton_miles_traveled * 100 / ton_miles_delivered;

  let y = y + 8 in
  let revenue = Player.revenue_sum player in
  let ton_miles = Player.get_ton_miles b.params.current_period player in
  let div_v = (ton_miles + 2) / 2 in
  let efficiency = M.((revenue * 500) / div_v) |> M.to_int |> Int.to_float in
  let efficiency = Float.(efficiency / 100.) in
  (* TODO: print money here *)
  write ~x ~y @@ sp "Revenue efficiency: %.2f per Ton-Mile" efficiency;

  let y = y + 8 in
  let scaled_ton_miles = ton_miles / (b.params.time / 256 + 1) in
  let prev_ton_miles = Player.get_ton_miles (Params.last_period b.params) player in
  let track_length = Player.track_length player in
  let utilization = (scaled_ton_miles + prev_ton_miles) / (track_length / 10 + 1) in
  write ~x ~y @@ sp "Track Utilization: %d Tons/Miles of Track" utilization;
  ()
