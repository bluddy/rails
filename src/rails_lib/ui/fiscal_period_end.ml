module List = Utils.List

module R = Renderer
module C = Constants
module B = Backend
module M = Money

let sp = Printf.sprintf

let render_bg win (s:State.t) =
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~color:Ega.black ~x:2 ~y:2 ~w:316 ~h:196 ~fill:false;
  R.draw_rect win ~color:Ega.cyan ~x:8 ~y:8 ~w:303 ~h:183 ~fill:true;
  let text = sp
    "End of\n\
    Fiscal Period\n\
    %d-%d"
    (s.backend.params.Params.year - 2) (s.backend.params.year - 1)
  in
  Fonts.Render.write_shadow win s.fonts ~color:Ega.bcyan ~idx:2 text ~x:80 ~y:72;
  ()

let _stock_price_diff_s ~region from_ to_ player_idx =
  let print_money from_ = Money.print ~region ~ks:false ~decimal:true from_ in
  let is_human = Owner.is_human player_idx in
  let per_share = if not is_human then "/" else " per " in
  if M.(from_ > to_) then sp "Stock drops from %s to %s%sshare" (print_money from_) (print_money to_) per_share
  else if M.(from_ < to_) then sp "Stock rises from %s to %s%sshare" (print_money from_) (print_money to_) per_share
  else sp "Stock stays at %s%sshare" (print_money from_) per_share

let _share_price_growth_s growth = sp "%d%% Average Share Price Growth." growth

let render_stock_eval win stock_data (s:State.t) =
  let player_idx = C.player in
  let b = s.backend in
  let write text = Fonts.Render.write win s.fonts ~idx:4 ~color:Ega.black text in
  let write_wh text = Fonts.Render.write win s.fonts ~idx:4 ~color:Ega.white text in
  let _draw_background =
    R.paint_screen win ~color:Ega.green;
    R.draw_rect win ~color:Ega.yellow ~x:270 ~y:0 ~w:50 ~h:200 ~fill:true;
    R.draw_line win ~color:Ega.black ~x1:270 ~y1:0 ~x2:270 ~y2:200;
    Menu.MsgBox.render_box win 2 2 236 184;
  in
  let y =
    (* Write player msg *)
    let msg = fst stock_data in
    let avg_growth = msg.Ui_msg.share_price_growth in
    let text = Printf.sprintf
      "%s%s\n\
      %d,000 shares outstanding.\n\
      %s\n\
      Investors are %s.%s"
      (if msg.split then "Your stock splits\ntwo for one!\n" else "")
      (_stock_price_diff_s ~region:b.params.region msg.from_ msg.to_ msg.player_idx)
      (Stock_market.total_shares player_idx b.stocks)
      (_share_price_growth_s avg_growth)
      (Stock_market.investor_opinion avg_growth |> Stock_market.show_investor)
      (match msg.fired with
        | `Fired -> "\nYou are replaced by NEW MANAGEMENT!"
        | `Warning -> "\nThey are looking for a new president!"
        | `MinorWarning -> "\nThey may replace you as president!"
        | `Normal -> "")
    in
    write_wh ~x:7 ~y:7 text;
    match msg.fired with | `Normal -> 40 | _ -> 50
  in
  let msgs = snd stock_data in
  (* Write ordered company data *)
  (* TODO: fix this up *)
  (* TODO: take cae of newline if fired/warned *)
  let _write_stock_data =
    List.fold_left (fun y Ui_msg.{from_; to_; player_idx; _} ->
      let name = B.get_name player_idx b in
      let text = sp
        "%s\n\
        %s\n\
        "
        name
        (_stock_price_diff_s ~region:b.params.region from_ to_ player_idx)
      in
      write ~x:7 ~y:7 text;
      y + 40 (* TODO *)
    )
    y
    msgs |> ignore
  in
  let _draw_portraits =
    let x = 200 in (* TODO *)
    List.fold_left (fun y Ui_msg.{player_idx; _} ->
      if Owner.is_human player_idx then
        () (* TODO: player frame *)
      else
        let ai = Ai.get_ai player_idx |> Option.get in
        let oppo = Ai.get_opponent player_idx b.ai |> Option.map Opponent.get_name in
        let tex = Hashtbl.find s.textures.opponents oppo in
        R.Texture.render win ~x ~y tex
    )
    8
    msgs
    |> ignore
  in
  let _write_rankings =
    let x = 220 in (* TODO *)
    let w, h = 10, 10 in (* TODO *)
    List.fold_left (fun (y, i) _ ->
      R.draw_rect win ~color:Ega.green ~x ~y ~h:10 ~w:10 ~fill:true;
      (* draw glint *)
      R.draw_line win ~color:Ega.bgreen ~x1:x ~y1:y ~x2:x ~y2:(y+h);
      R.draw_line win ~color:Ega.bgreen ~x1:x ~y1:(y+h) ~x2:(x+w) ~y2:(y+h);
      let text = match i with
       | 0 -> "1st"
       | 1 -> "2nd"
       | 2 -> "3rd"
       | _ -> sp "%dth" (i+1)
      in
      write text ~x:(x + 2) ~y; (* TODO *)
      (y + 80, i+1) (* TODO *)
    )
    (8 + 30, 0) (* TODO *)
    msgs
  in
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

