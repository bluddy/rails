open !Containers

module B = Backend
module C = Constants
module R = Renderer
module M = Money

let sp = Printf.sprintf

include Retirement_bonus_d

let make ~fired player_idx b =
  let _, _, diff_factor = B.job_bonus_diff_factor player_idx b in
  {diff_factor; fired}

let render (v:t) win (s:State.t) =
  let b = s.backend in
  let params = b.Backend_d.params in
  let fonts = s.fonts in
  let player_idx = C.player in
  let player = B.get_player player_idx b in
  let region = B.get_region b in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  let write = Fonts.Render.write win fonts in
  let write_g = write ~color:Ega.gray ~idx:`Standard in
  let write = write ~color:Ega.black in
  let write_lg = write ~idx:`Large in
  let write = write ~idx:`Standard in
  let x_l, x_r, x_mult = 48, 228, 256 in
  let y = 16 in
  write_lg ~x:x_l ~y "Retirement Bonus";
  let y = y + 24 in

  write ~x:x_l ~y "Final asset value:";
  let net_worth = Player.get_net_worth player in
  write ~x:x_r ~y (Money.print ~region net_worth);

  let y = y + 16 in
  write ~x:x_l ~y @@ sp "Length of Service:\n(%d years)" @@ Params.age params;

  let draw_line y =
    R.draw_line win ~x1:228 ~x2:288 ~y1:y ~y2:y ~color:Ega.black
  in

  let value = net_worth in
  let age = Params.age params in
  let value =
    let pct = 1000 / (age + 20) in
    write_g ~x:x_mult ~y @@ sp "x %d%%" pct;

    let y = y + 10 in
    draw_line y;

    let value = M.(value * 100 / Int.(age + 20)) in
    let y = y + 2 in
    write ~x:x_r ~y @@ Money.print ~region value;
    value
  in

  let y = y + 24 in

  let owned_ais =
    Stock_market.other_companies_controlled_by player_idx b.stocks |> List.length in
  write ~x:x_l ~y @@ sp "Railroad Mogul Bonus:\n(You control %d RR)" owned_ais;

  let value =
    let owned_ai_factor = (1 lsl (owned_ais - 1)) in
    let owned_ai_pct = (owned_ai_factor * 25) + 100 in
    let value = M.(value + (value * owned_ai_factor / 4)) in
    write_g ~x:x_mult ~y @@ sp "x %d%%" owned_ai_pct;

    let y = y + 10 in
    draw_line y;

    let y = y + 2 in
    write ~x:x_r ~y (Money.print ~region value);
    value
  in

  let y = y + 16 in

  let value =
    if not v.fired then value else (
      write ~x:x_l ~y "Involuntary Retirement ";
      write_g ~x:x_mult ~y "x 75%";

      let y = y + 10 in
      draw_line y;

      let y = y + 2 in
      let value = M.(value * 3 / 4) in
      write ~x:x_r ~y @@ Money.print ~region value;
      value
    )
  in

  let y = y + 16 in

  let _retirement_bonus =
    write ~x:x_l ~y "Difficulty Factor:";
    write_g ~x:x_mult ~y @@ sp "x %d%%" v.diff_factor;

    let y = y + 10 in
    draw_line y;

    let y = y + 2 in
    write ~x:x_l ~y "Retirement Bonus:";

    let value = M.(value / 20 * v.diff_factor / 5) in
    write ~x:x_r ~y @@ Money.print ~region value
  in
  ()




