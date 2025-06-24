module List = Utils.List

module R = Renderer
module C = Constants
module B = Backend

let render_bg win (s:State.t) =
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~color:Ega.black ~x:2 ~y:2 ~w:316 ~h:196 ~fill:false;
  R.draw_rect win ~color:Ega.cyan ~x:8 ~y:8 ~w:303 ~h:183 ~fill:true;
  let text =
    Printf.sprintf
    "End of\n\
    Fiscal Period\n\
    %d-%d"
    (s.backend.params.Params.year - 2) (s.backend.params.year - 1)
  in
  Fonts.Render.write_shadow win s.fonts ~color:Ega.bcyan ~idx:2 text ~x:80 ~y:72;
  ()

let render_stock_eval win stock_data (s:State.t) =
  let b = s.backend in
  R.paint_screen win ~color:Ega.green;
  R.draw_rect win ~color:Ega.yellow ~x:270 ~y:0 ~w:50 ~h:200 ~fill:true;
  R.draw_line win ~color:Ega.black ~x1:270 ~y1:0 ~x2:270 ~y2:200;
  Menu.MsgBox.render_box win 2 2 236 184;
  ()

let get_warnings backend msgs =
  let process = function
    | Ui_msg.TrainNoRevenue train_id ->
      Printf.sprintf "Train #%d produced no revenue." @@ Train.Id.to_int train_id

    | TrainNoMaintenance train_id ->
      Printf.sprintf "Train #%d received no maintenance." @@ Train.Id.to_int train_id

    | TrainNoSchedule train_id ->
      Printf.sprintf "Train #%d has no schedule." @@ Train.Id.to_int train_id

    | TrainOldEngine train_id ->
      Printf.sprintf "Train #%d's engine is getting old." @@ Train.Id.to_int train_id

    | StationHasHold loc ->
      let name =
        let station = B.get_station loc backend |> Option.get in
        Station.get_name station in
      Printf.sprintf "Hold signal near %s." name

    | ConsiderBankruptcy -> "Consider Bankruptcy relief!"

    | _  -> ""
  in
  msgs
  |> List.map process
  |> List.filter (fun s -> String.length s > 0)
  |> String.concat "\n"

let get_records backend msgs =
  let region = B.get_region backend in
  let process = function
  | Ui_msg.AvgSpeedRecord speed ->
    Printf.sprintf
    "New Record:\n\
    Average speed: %d mph!\n"
    speed

  | TonMileRecord ton_miles ->
    Printf.sprintf
    "New Record:\n\
    Yearly Ton/Miles: %d!\n"
    ton_miles

  | RevenueRecord money ->
    Printf.sprintf
    "New Record:\n\
    Yearly revenues: %s!\n"
    (Money.print ~region money)

  | _ -> ""
  in
  msgs
  |> List.map process
  |> List.filter (fun s -> String.length s > 0)
  |> String.concat "\n"

let get_record_earnings backend msgs =
  let player = B.get_player C.player backend in
  let process = function
    | Ui_msg.RecordEarnings money ->
       (Printf.sprintf "Record Profits on %s" @@
          Player.get_handle backend.stations backend.cities player,
        Printf.sprintf "%s earnings" @@
          Money.print ~region:backend.params.region money,
        "in last two years.")
        |> Option.some
    | _ -> None
  in
  msgs
  |> List.map process
  |> List.filter (fun s -> Option.is_some s)
  |> List.head_opt
  |> (function | Some x -> x | _ -> None)

let get_stock_msgs msgs =
  let all_players =
    List.filter (function
      | Ui_msg.SharePriceChange _ -> true
      | _ -> false)
      msgs
    |> List.sort (fun x y -> match x, y with
       | Ui_msg.SharePriceChange x, SharePriceChange y -> x.avg_share_price_growth_pct - y.avg_share_price_growth_pct
       | _ -> assert false
    ) |> List.rev
  in
  let human =
    List.filter (function
      | Ui_msg.SharePriceChange s when Owner.is_human s.player_idx -> true
      | _ -> false)
      msgs
    |> List.hd
  in
  human, all_players

let handle_msgs backend msgs =
  let record_earnings = get_record_earnings backend msgs in
  let warnings = get_warnings backend msgs in
  let records = get_records backend msgs  in
  let stock_msgs = get_stock_msgs msgs in
  record_earnings, warnings, records, stock_msgs

