module List = Utils.List

module R = Renderer
module C = Constants
module B = Backend
module M = Money

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

let stock_price_diff_s ~ai ~region x y =
  let print_money x = Money.print ~region ~ks:false ~decimal:true x in
  let per_share = if ai then "/" else " per " in
  let s = Printf.sprintf in
  if M.(x > y) then s "Stock drops from %s to %s%sshare" (print_money x) (print_money y) per_share
  else if M.(x < y) then s "Stock rises from %s to %s%sshare" (print_money x) (print_money y) per_share
  else s "Stock stays at %s%sshare" (print_money x) per_share

let render_stock_eval win stock_data (s:State.t) =
  let player_idx = C.player in
  let b = s.backend in
  R.paint_screen win ~color:Ega.green;
  R.draw_rect win ~color:Ega.yellow ~x:270 ~y:0 ~w:50 ~h:200 ~fill:true;
  R.draw_line win ~color:Ega.black ~x1:270 ~y1:0 ~x2:270 ~y2:200;
  Menu.MsgBox.render_box win 2 2 236 184;
  let player_data = fst stock_data in
  let avg_growth = player_data.Ui_msg.share_price_growth in
  let text = Printf.sprintf
    "%s\n\
    %d,000 shares outstanding.\n\
    %d%% Average Share Price Growth.\n\
    Investors are %s.\n\
    %s"
    (stock_price_diff_s ~ai:false ~region:b.params.region player_data.Ui_msg.from_ player_data.to_)
    (Stock_market.total_shares player_idx b.stocks)
    avg_growth
    (Stock_market.investor_opinion avg_growth |> Stock_market.show_investor)
    (match player_data.fired with
      | `Fired -> "You are replaced by NEW MANAGEMENT!"
      | `Warning -> "They may replace you as president!"
      | `Normal -> "")
  in
  Fonts.Render.write win s.fonts ~color:Ega.white ~idx:4 ~x:7 ~y:7 text

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
    msgs
    |> List.filter (function | Ui_msg.SharePriceChange _ -> true | _ -> false)
    |> List.map (function | Ui_msg.SharePriceChange x -> x | _ -> assert false)
    |> List.sort (fun x y -> x.Ui_msg.share_price_growth - y.share_price_growth)
    |> List.rev
  in
  let human =
    msgs
    |> List.filter (function | Ui_msg.SharePriceChange s when Owner.is_human s.player_idx -> true | _ -> false)
    |> List.map (function | Ui_msg.SharePriceChange x -> x | _ -> assert false)
    |> List.hd
  in
  human, all_players

let handle_msgs backend msgs =
  let record_earnings = get_record_earnings backend msgs in
  let warnings = get_warnings backend msgs in
  let records = get_records backend msgs  in
  let stock_msgs = get_stock_msgs msgs in
  record_earnings, warnings, records, stock_msgs

