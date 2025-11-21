open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

include Train_income_report_d

let src = Logs.Src.create "train_income_report" ~doc:"Train_income_report"
module Log = (val Logs.src_log src: Logs.LOG)

let sp = Printf.sprintf

let num_trains = 8
let heading_h = 8 + 2 * 8
let train_h = 19

let create ?start_id (s:State.t) =
  let player_idx = C.player in
  let trains = B.get_trains player_idx s.backend in
  let start_id = match start_id with
    | None -> Trainmap.first_train trains
    | Some id -> id
  in
  let _, first, last = Trainmap.subrange start_id ~num:num_trains trains in
  let prev_button = match first with `NotFirst -> true | _ -> false in
  let next_button = match last with `NotLast -> true | _ -> false in
  { start_id; next_button; prev_button; }

let render win state (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  let write = Fonts.Render.write ~idx:`Standard win fonts in
  let write_g = write ~color:Ega.gray in
  let write = write ~color:Ega.black in

  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~color:Ega.bgreen ~x:0 ~y:0 ~w:320 ~h:heading_h ~fill:true;

  let _draw_headings =
    let name = B.get_name player_idx b in
    let heading = sp "Train Report:%s" name in
    write ~x:64 ~y:4 heading;
    let headings = "Train class/route  Revenue: YTD   Last Year  Lifetime" in
    write ~x:1 ~y:14 headings;
    let y = heading_h in
    R.draw_line win ~color:Ega.black ~x1:0 ~x2:319 ~y1:y ~y2:y
  in
  
  let player = B.get_player player_idx b in
  let draw_train y i =
    let train = Trainmap.get (Train.Id.of_int i) player.trains in
    let typ_s = Train.get_type train |> Train.show_train_type in
    let last_loc = Train.get_last_station train in
    let last_station = Station_map.get_exn last_loc b.stations in
    let last_short_name = Station.get_short_name last_station in
    let next_loc = Train.get_next_station train in
    let next_station = Station_map.get_exn next_loc b.stations in
    let next_short_name = Station.get_short_name next_station in
    let is_traveling = Train.is_traveling train in
    let desc_s = sp "%s-%s%s"
      last_short_name
      (if is_traveling then ">" else "")
      next_short_name
    in
    let num_type_dest = sp "%d)%s/%s" (i+1) typ_s desc_s in
    write ~x:1 ~y num_type_dest;

    let current_period = B.get_period b in
    let last_period = Params.last_period b.params in
    let money_s = Money.print ~region:b.params.region ~spaces:6 in
    let last_revenue = Train.get_revenue last_period train |> money_s in
    let cur_revenue = Train.get_revenue current_period train |> money_s in
    let total_revenue = Train.get_total_revenue train |> money_s in
    let text = sp "%s  %s  %s" last_revenue cur_revenue total_revenue in
    write ~x:128 ~y text;

    let y = y + 8 in
    Train_report.draw_train win train ~x:61 ~y s;

    let avg_speed = Train.calc_avg_speed b.params.region train in
    write_g ~x:185 ~y @@ sp "(%d mph)" avg_speed;

    let maintenance = Train.displayed_maintenance_cost train in
    write_g ~x:256 ~y @@ money_s maintenance;

    let y = y + 9 in
    R.draw_line win ~color:Ega.black ~x1:0 ~x2:319 ~y1:y ~y2:y;

    y + 2
  in
  let player = B.get_player player_idx b in
  let range, first, last = Trainmap.subrange state.start_id ~num:8 @@ Player.get_trains player in
  Iter.fold draw_train (heading_h + 2) range |> ignore;

  let y = 190 in
  (match first with
  | `First -> ()
  | `NotFirst -> write ~x:30 ~y "Prev");
  (match last with
  | `Last -> ()
  | `NotLast -> write ~x:260 ~y "Next")

let handle_event v (s:State.t) (event:Event.t) =
  let b = s.backend in
  let player_idx = C.player in
  let player = B.get_player player_idx b in
  let action = match event with
  | MouseButton {x; y; button=`Left; down=true; _} ->
    if y <= heading_h then `Exit else
    if v.prev_button && x > 30 && x < 60 && y > 190 then `Prev else
    if v.next_button && x > 260 && x < 290 && y > 190 then `Next else
    let is_none x = match x with `None -> true | _ -> false in
    let choice, y_end = Trainmap.foldi (fun i ((choice, y_end) as acc) _ ->
      if is_none choice && y <= y_end then (`OpenTrain i, y_end + train_h) else acc)
      ~init:(`None, heading_h + 1 + train_h) @@
      Player.get_trains player
    in
    let choice = if is_none choice && y > y_end then `Exit else choice in
    choice
  | Key {down=true; key=_; _} -> `Exit
  | _ -> `None
  in
  match action with
  | `Next ->
      let start_id = Train.Id.to_int v.start_id + 8 |> Train.Id.of_int in
      create ~start_id s, `None
  | `Prev ->
      let start_id = Train.Id.to_int v.start_id - 8 |> Train.Id.of_int in
      create ~start_id s, `None
  | _ -> v, action

