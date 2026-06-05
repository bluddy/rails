open! Containers

module Ega = Engine.Ega
module R = Engine.Renderer

let render_timebox win (x, y) =
  let x2, y2 = x + 159, y + 25 in
  let top, text_h = 3, 8 in
  R.draw_rect2 win ~x ~y ~x2 ~y2 ~color:Ega.dgray ~fill:true;
  R.draw_rect2 win ~x:(x+1) ~y:(y+1) ~x2:(x2-1) ~y2:(y2-1) ~color:Ega.black ~fill:false;
  R.draw_line win ~x1:(x+2) ~y1:(y+1) ~x2:(x2-1) ~y2:(y+1) ~color:Ega.gray;
  R.draw_line win ~x1:(x+2) ~y1:(y+1) ~x2:(x+2) ~y2:(y2-2) ~color:Ega.gray;
  R.draw_rect2 win ~x:(x+5) ~y:(y+top) ~x2:(x2-3) ~y2:(y+3+text_h) ~color:Ega.black ~fill:true;
  R.draw_rect2 win ~x:(x+15) ~y:(y2-3-text_h) ~x2:(x2-13) ~y2:(y2-3) ~color:Ega.black ~fill:true;
  R.draw_line win ~x1:(x+3) ~y1:(y+3) ~x2:(x+5) ~y2:(y+3) ~color:Ega.black;
  R.draw_line win ~x1:(x+13) ~y1:(y2-3-text_h) ~x2:(x+15) ~y2:(y2-3-text_h) ~color:Ega.black;
  R.draw_line win ~x1:(x+6) ~y1:(y+4) ~x2:(x+6) ~y2:(y+4+text_h-1) ~color:Ega.gray;
  R.draw_line win ~x1:(x+16) ~y1:(y2-3-text_h+1) ~x2:(x+16) ~y2:(y2-3) ~color:Ega.gray;
  R.draw_line win ~x1:(x2-4) ~y1:(y+4) ~x2:(x2-4) ~y2:(y+4+text_h-1) ~color:Ega.dgray;
  R.draw_line win ~x1:(x2-14) ~y1:(y2-3-text_h) ~x2:(x2-14) ~y2:(y2-3-text_h) ~color:Ega.dgray;
  ()

let render_time win (s:Services.t) ((x, y) as loc) (v:Case.t) =
  let city_s = (Loc.Map.find v.cur_loc v.d.locs).city in
  let time_s = Utils.time_date_print ~minutes:v.time_minutes ~months:v.world.time_months in
  render_timebox win loc;
  let write =
    Fonts.Render.write s.win s.fonts ~color:Ega.bgreen ~idx:`Large in
  let _write_city =
    let x, y = x + 37, y + 4 in
    write ~x ~y city_s
  in
  let _write_time =
    let x, y = x + 35, y + 15 in
    write ~x ~y time_s
  in
  ()




