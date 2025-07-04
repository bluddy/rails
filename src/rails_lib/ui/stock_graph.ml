open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

let sp = Printf.sprintf

let company_colors = [|Ega.black; Ega.bgreen; Ega.red; Ega.bblue|]

let render win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let write = Fonts.Render.write win fonts ~idx:`Standard in

  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:3 ~y:3 ~w:23 ~h:194 ~fill:true ~color:Ega.yellow;
  R.draw_rect win ~x:2 ~y:2 ~w:(320-4) ~h:(200-4) ~color:Ega.black ~fill:false;
  R.draw_line win ~x1:26 ~x2:26 ~y1:2 ~y2:197 ~color:Ega.black;

  let _horizontal_lines =
    let y_base = 40 in
    Iter.iter (fun i ->
      let y = y_base + i * 40 in
      R.draw_line win ~x1:27 ~x2:316 ~y1:y ~y2:y ~color:Ega.gray;
    )
    Iter.(0 -- 3)
  in
  let owners = B.players_and_ai b in
  let colors =
    Iter.foldi (fun acc i owner -> Owner.Map.add owner company_colors.(i) acc)
      Owner.Map.empty
      owners
  in

  let _write_names =
    let y, h = 5, 9 in
    Iter.iteri (fun i owner ->
      let color = Owner.Map.find owner colors in
      let name = B.get_name owner b in
      write ~x:30 ~y:(y + i * h) ~color name)
      owners
  in
  let stocks = b.stocks in
  let max_value = Stock_market.max_history_price stocks in
  let max_value = M.(max max_value (of_int 80)) in
  let divider = if M.(max_value > of_int 200) then 100 else 20 in
  (* Round the number *)
  let max_value = M.((((max_value - of_int 1) / divider) + of_int 1) * divider) in

  let _write_value_labels =
    let price_delta = M.(max_value / 4) in
    let x, y_base = 4, 37 in
    Iter.fold (fun money i ->
      let y = y_base + i * 40 in
      let money_s = Money.print ~ks:false ~region:b.params.region money in
      write ~x ~y ~color:Ega.black money_s;
      M.(money - price_delta))
    M.(price_delta * 4)
    Iter.(0 -- 3)
  in

  let _create_graph =
    Iter.iter (fun owner ->
      let color = Owner.Map.find owner colors in
      let start_x, start_y = 30, 197 in
      let stock_data = Stock_market.get_history owner stocks in
      List.fold_right (fun value (x1, y1)  ->
        let y2 = start_y - M.((value * 16) /~ (max_value / 10)) in
        let x2 = x1 + 5 in
        R.draw_line win ~x1 ~y1 ~x2 ~y2 ~color;
        (x2, y2))
      stock_data
      (start_x, start_y)
      |> ignore)
    (Iter.rev owners)
  in
  ()

