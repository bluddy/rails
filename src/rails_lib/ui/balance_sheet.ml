open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants
module R = Renderer

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
  

let render win (s:State.t) =
  let x_left, x_text, x_total, x_ytd = 8, 16, 128, 224 in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  Fonts.Render.write_shadow win s.fonts ~idx:2 ~color:Ega.gray ~x:32 ~y:8 @@
    Printf.sprintf "Balance Sheet: %d" s.backend.year;
  let player = s.backend.players.(C.player) in
  let rr_name = Player.get_name player s.backend.stations s.backend.cities in
  let y = 24 in
  let line = 8 in
  let write ?(color=Ega.black) = Fonts.Render.write win s.fonts ~idx:4 ~color in
  let write_money ~x ~y money =
    let money_s = Utils.show_cash ~spaces:6 ~region:s.backend.region money in
    let color = if money < 0 then Ega.red else Ega.black in
    write ~x ~y ~color money_s
  in
  let write_total_and_ytd ~y money oldval =
    write_money ~x:x_total ~y money;
    let ytd_money = money - oldval in
    write_money ~x:x_ytd ~y ytd_money
  in
  write ~x:80 ~y rr_name;
  let y = y + line + line in
  write ~x:x_total ~y "Total";
  write ~x:x_ytd ~y "YTD Changes";
  let y = y + line in
  write ~x:x_left ~y "Assets:";

  let player = Backend.get_player s.backend C.player in
  let prev_balance_sheet = player.m.last_balance_sheet in

   let y = y + line in
  write ~x:x_text ~y "Operating Funds:";
  let funds = Player.get_cash s.backend.players.(C.player) in
  write_total_and_ytd ~y funds prev_balance_sheet.operating_funds;

  let y = y + line in
  write ~x:x_text ~y "Treasury Stock:";
  let stock = Stocks.compute_treasury_stock player.m.stock in
  write_total_and_ytd ~y stock prev_balance_sheet.treasury_stock;

  let y = y + line in
  write ~x:x_text ~y "Other RR Stock:";
  let other_rr_stock =
    Iter.fold (fun acc ai_player_idx ->
      if ai_player_idx = C.player then acc
      else
        let ai_player = Backend.get_player s.backend ai_player_idx in
        let ai_stock = ai_player.m.stock in
        let owned_shares = Stocks.get_owned_shares ai_stock ai_player_idx in
        let owned_value = Stocks.compute_owned_share_value
           ~total_shares:ai_stock.total_shares ~owned_shares ~share_price:ai_stock.share_price
        in
        acc + owned_value)
    0
    Iter.(0 -- (C.num_players - 1))
  in
  write_total_and_ytd ~y other_rr_stock prev_balance_sheet.other_rr_stock;

  let y = y + line in
  write ~x:x_text ~y "Facilities:";
  let facilities = 
    List.fold_left (fun acc loc ->
      let station = Station_map.get_exn loc s.backend.stations in
      acc + Station.total_upgrade_value station)
    0
    player.stations
  in
  write_total_and_ytd ~y facilities prev_balance_sheet.facilities;

  let y = y + line in
  write ~x:x_text ~y "Industries:";
  let industries = player.m.owned_industry in
  write_total_and_ytd ~y industries prev_balance_sheet.industries;

   let y = y + line in
  write ~x:x_text ~y "Real Estate:";
  let real_estate = calc_real_estate s.backend.region s.backend.track s.backend.map ~player:C.player in
  write_total_and_ytd ~y real_estate prev_balance_sheet.real_estate;

   let y = y + line in
  let dist = Trackmap.calc_total_dist s.backend.track ~player:C.player in
  let dist = dist * Region.dist_mult s.backend.region in
  write ~x:x_text ~y @@ Printf.sprintf "Track: %d miles:" dist;
  let track = dist * 3 / 2 in
  write_total_and_ytd ~y track prev_balance_sheet.track;

  let y = y + line in
  write ~x:x_text ~y "Rolling Stock:"; let y = y + line + line in
  write ~x:x_left ~y "Liabilities:"; let y = y + line in
  write ~x:x_text ~y "Outstandling Loans:"; let y = y + line in
  write ~x:x_text ~y "Stockholders Equity:"; let y = y + line + line in
  write ~x:x_text ~y "PROFIT:";
  ()

    
