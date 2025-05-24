open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants
module B = Backend

include Balance_sheet_d

let calc_real_estate region track_map tile_map player_idx =
  Trackmap.fold (fun loc track acc ->
    if Owner.(track.player = player_idx) then
      let tile = Tilemap.get_tile loc tile_map in
      let info = Tile.Info.get region tile in
      let cost = info.cost in
      let track_dist = Track.calc_dist ~use_double:false track in
      acc + track_dist * cost
    else acc)
  track_map
  ~init:0

let create (s:State.t) player_idx =
  let player = B.get_player player_idx s.backend in
  let operating_funds = Player.get_cash player in
  let treasury_stock = Stock_market.treasury_share_value player_idx s.backend.stocks in
  let other_rr_stock = Stock_market.total_owned_stock_value player_idx ~exclude_self:true s.backend.stocks in
  let facilities = 
    List.fold_left (fun acc loc ->
      let station = Station_map.get_exn loc s.backend.stations in
      acc + Station.value_of station + Station.total_upgrade_value station)
    0
    player.stations
  in
  let industries = player.m.owned_industry in
  let real_estate = calc_real_estate (B.get_region s.backend) s.backend.track s.backend.map player_idx in
  let dist = Trackmap.calc_total_dist s.backend.track ~player:C.player in
  let track_miles = dist * Region.dist_mult (B.get_region s.backend) in
  let track = dist * 3 / 2 in
  let engine_cost = Trainmap.total_engine_value player.trains in
  let car_cost = Trainmap.total_car_value player.trains in
  let rolling_stock = engine_cost + car_cost in
  let outstanding_loans = -player.m.bonds in
  let stockholders_equity = player.m.stockholders_equity in
  {
    operating_funds;
    treasury_stock;
    other_rr_stock;
    facilities;
    industries;
    real_estate;
    track_miles;
    track;
    rolling_stock;
    outstanding_loans;
    stockholders_equity;
  }

    
