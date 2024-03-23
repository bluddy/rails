open! Containers
module C = Constants
module R = Renderer

let compute_assets (b:Balance_sheet.t) =
  (* TODO: for some reason, we have different ways of rounding different assets of the balance sheet *)
  b.operating_funds + b.treasury_stock + b.other_rr_stock + b.facilities + b.industries + b.real_estate + b.track + b.rolling_stock + b.outstanding_loans + b.stockholders_equity

let compute_liabilities (b:Balance_sheet.t) =
  b.outstanding_loans + b.stockholders_equity

let render win (s:State.t) (balance_sheet:Balance_sheet.t) =
  let player = Backend.get_player s.backend C.player in
  let b, pb = balance_sheet, player.m.last_balance_sheet in
  let x_left, x_text, x_total, x_ytd = 8, 16, 128, 224 in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  Fonts.Render.write_shadow win s.fonts ~idx:2 ~color:Ega.gray ~x:32 ~y:8 @@
    Printf.sprintf "Balance Sheet: %d" s.backend.year;
  let rr_name = Player.get_name player s.backend.stations s.backend.cities in
  let y = 24 in
  let line = 8 in
  let write ?(color=Ega.black) = Fonts.Render.write win s.fonts ~idx:4 ~color in
  let write_money ~x ~y money =
    let money_s = Utils.show_cash ~show_neg:false ~spaces:6 ~region:s.backend.region money in
    let color = if money < 0 then Ega.bred else Ega.black in
    write ~x ~y ~color money_s
  in
  let write_total_and_ytd ~y text money oldval =
    write ~x:x_text ~y text;
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
   let y = y + line in
  write_total_and_ytd ~y "Operating Funds:" b.operating_funds pb.operating_funds;
  let y = y + line in
  write_total_and_ytd ~y "Treasury Stock:" b.treasury_stock pb.treasury_stock;
  let y = y + line in
  write_total_and_ytd ~y "Other RR Stock:" b.other_rr_stock pb.other_rr_stock;
  let y = y + line in
  write_total_and_ytd ~y "Facilities:" b.facilities pb.facilities;
  let y = y + line in
  write_total_and_ytd ~y "Industries:" b.industries pb.industries;
  let y = y + line in
  write_total_and_ytd ~y "Real Estate:" b.real_estate pb.real_estate;
  let y = y + line in
  write_total_and_ytd ~y (Printf.sprintf "Track: %d miles:" b.track_miles) b.track pb.track;
  let y = y + line in
  write_total_and_ytd ~y "Rolling Stock:" b.rolling_stock pb.rolling_stock;
  R.draw_line win ~x1:128 ~y1:120 ~x2:180 ~y2:120 ~color:Ega.black;
  let y = y + line + 2 in
  let assets = compute_assets b in
  write_money ~x:x_total ~y assets;
  let y = y + line in
  write ~x:x_left ~y "Liabilities:";
  let y = y + line in
  write_total_and_ytd ~y "Outstanding Loans:" b.outstanding_loans pb.outstanding_loans;
  let y = y + line in
  write_total_and_ytd ~y "Stockholders Equity:" b.stockholders_equity pb.stockholders_equity;
  let y = y + line + line in
  let profit = assets + compute_liabilities b in
  let old_profit = compute_assets pb + compute_liabilities pb in
  write_total_and_ytd ~y "PROFIT:" profit old_profit;
  ()

    
