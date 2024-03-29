open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants

include Balance_sheet_d

let calc_real_estate region track_map tile_map ~player =
  Trackmap.fold (fun (x, y) track acc ->
    if track.player = player then
      let tile = Tilemap.get_tile tile_map x y in
      let info = Tile.Info.get region tile in
      let cost = info.cost in
      let track_dist = Track.calc_dist ~use_double:false track in
      acc + track_dist * cost
    else acc)
  track_map
  ~init:0

let create (s:State.t) ~player_idx =
  let player = Backend.get_player s.backend player_idx in

  let operating_funds = Player.get_cash player in
  let treasury_stock = Stocks.compute_treasury_stock player.m.stock in
  let other_rr_stock =
    Iter.fold (fun acc ai_player_idx ->
      if ai_player_idx = C.player then acc
      else
        let ai_player = Backend.get_player s.backend ai_player_idx in
        let ai_stock = ai_player.m.stock in
        let owned_shares = Stocks.owned_shares ai_stock ai_player_idx in
        let owned_value = Stocks.compute_owned_share_value
           ~total_shares:ai_stock.total_shares ~owned_shares ~share_price:ai_stock.share_price
        in
        acc + owned_value)
    0
    Iter.(0 -- (Backend.num_players s.backend - 1))
  in
  let facilities = 
    List.fold_left (fun acc loc ->
      let station = Station_map.get_exn loc s.backend.stations in
      acc + Station.value_of station + Station.total_upgrade_value station)
    0
    player.stations
  in
  let industries = player.m.owned_industry in
  let real_estate = calc_real_estate s.backend.region s.backend.track s.backend.map ~player:C.player in
  let dist = Trackmap.calc_total_dist s.backend.track ~player:C.player in
  let track_miles = dist * Region.dist_mult s.backend.region in
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

    
