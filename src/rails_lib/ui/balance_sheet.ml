open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants
module R = Renderer

(* let compute_ytd backend player last_year = *)
(*   (* Compute the current balance sheet *) *)
(*   let operating_funds = player.cash in *)
(*   let treasury_stock = player.treasury_stock in *)
(*   let other_rr_stock = player.other_rr_stock in *)
(*   () *)


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
  write ~x:80 ~y rr_name;
  let y = y + line + line in
  write ~x:x_total ~y "Total";
  write ~x:x_ytd ~y "YTD Changes";
  let y = y + line in
  write ~x:x_left ~y "Assets:"; let y = y + line in

  let player = Backend.get_player s.backend C.player in
  let prev_balance_sheet = player.m.last_balance_sheet in

  write ~x:x_text ~y "Operating Funds:";
  let funds = Player.get_cash s.backend.players.(C.player) in
  write_money ~x:x_total ~y funds;
  let ytd_funds = funds - prev_balance_sheet.operating_funds in
  write_money ~x:x_ytd ~y ytd_funds;

  let y = y + line in

  write ~x:x_text ~y "Treasury Stock:";


  let y = y + line in

  write ~x:x_text ~y "Other RR Stock:"; let y = y + line in
  write ~x:x_text ~y "Facilities:"; let y = y + line in
  write ~x:x_text ~y "Industries:"; let y = y + line in
  write ~x:x_text ~y "Real Estate:"; let y = y + line in
  write ~x:x_text ~y @@ Printf.sprintf "Track: %d miles:" 41; let y = y + line in
  write ~x:x_text ~y "Rolling Stock:"; let y = y + line + line in
  write ~x:x_left ~y "Liabilities:"; let y = y + line in
  write ~x:x_text ~y "Outstandling Loans:"; let y = y + line in
  write ~x:x_text ~y "Stockholders Equity:"; let y = y + line + line in
  write ~x:x_text ~y "PROFIT:";
  ()

    
