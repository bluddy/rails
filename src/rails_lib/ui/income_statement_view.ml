open! Containers
module C = Constants
module R = Renderer

let render win (s:State.t) =
  let player = Backend.get_player s.backend C.player in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  Fonts.Render.write_shadow win s.fonts ~idx:2 ~color:Ega.gray ~x:16 ~y:4 @@
    Printf.sprintf "Income Statement: %d" s.backend.year;
  let climate = Climate.show s.backend.climate in
  Fonts.Render.write win s.fonts ~idx:4 ~color:Ega.gray ~x:80 ~y:23 @@
    Printf.sprintf "Economic Climate: %s" climate;

  let x_left, x_text, x_ytd, x_total = 16, 20, 160, 240 in
  let write ?(color=Ega.black) = Fonts.Render.write win s.fonts ~idx:4 ~color in
  let write_money ~x ~y money =
    let money_s = Utils.show_cash ~show_neg:false ~spaces:6 ~region:s.backend.region money in
    let color = if money < 0 then Ega.bred else Ega.black in
    write ~x ~y ~color money_s
  in
  let write_total_and_ytd ~y ?text money oldval =
    begin match text with
    | Some text -> write ~x:x_text ~y text;
    | None -> ()
    end;
    write_money ~x:x_total ~y money;
    let ytd_money = money - oldval in
    write_money ~x:x_ytd ~y ytd_money
  in
  let line = 8 in
  let y = 36 in
  write ~x:x_left ~y "REVENUES:";
  write ~x:x_ytd ~y "YTD:";
  write ~x:x_total ~y "Total:";
  let y = y + 12 in
  write_total_and_ytd ~y ~text:"Mail" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Passengers" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Fast Freight" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Slow Freight" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Bulk Freight" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Other Income" 0 0;
  let y = y + line in
  R.draw_line win ~x1:x_ytd ~y1:y ~x2:(x_ytd+54) ~y2:y ~color:Ega.black;
  R.draw_line win ~x1:x_total ~y1:y ~x2:(x_total+54) ~y2:y ~color:Ega.black;
  let y = y + 2 in
  write_total_and_ytd ~y 0 0;
  let y = y + line in
  write ~x:x_left ~y "EXPENSES:";
  let y = y + 12 in
  write_total_and_ytd ~y ~text:"Interest/Fees" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Train Maintenance" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Track Maintenance" 0 0;
  let y = y + line in
  write_total_and_ytd ~y ~text:"Station Maintenance" 0 0;
  let y = y + line in
  R.draw_line win ~x1:x_ytd ~y1:y ~x2:(x_ytd+54) ~y2:y ~color:Ega.black;
  R.draw_line win ~x1:x_total ~y1:y ~x2:(x_total+54) ~y2:y ~color:Ega.black;
  let y = y + 2 in
  write_total_and_ytd ~y 0 0;
  let y = y + line + 2 in
  R.draw_line win ~x1:x_ytd ~y1:y ~x2:(x_ytd+54) ~y2:y ~color:Ega.black;
  R.draw_line win ~x1:x_total ~y1:y ~x2:(x_total+54) ~y2:y ~color:Ega.black;
  let y = y + 2 in
  write_total_and_ytd ~y ~text:"Operating Profit" 0 0;
  let y = y + line + 1 in
  write ~x:x_text ~y "Stock Profits:";
  write_money ~x:x_ytd ~y 0;
  ()
