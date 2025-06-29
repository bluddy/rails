module List = Utils.List

module R = Renderer
module C = Constants
module B = Backend
module M = Money

include Fiscal_period_end_d

let src = Logs.Src.create "Fiscal_period_end" ~doc:"Fiscal_period_end"
module Log = (val Logs.src_log src: Logs.LOG)

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

let _stock_price_diff_s ~split ~region from_ to_ player_idx =
  let print_money x = Money.print ~region ~ks:false ~decimal:true x in
  let is_human = Owner.is_human player_idx in
  let per_share = if not is_human then "/" else " per " in
  if not is_human && split then sp "Stock splits 2 for 1 to %s/share" (print_money to_)
  else if M.(from_ > to_) then sp "Stock drops from %s to %s%sshare" (print_money from_) (print_money to_) per_share
  else if M.(from_ < to_) then sp "Stock rises from %s to %s%sshare" (print_money from_) (print_money to_) per_share
  else sp "Stock stays at %s%sshare" (print_money from_) per_share

let _share_price_growth_s growth = sp "%d%% Average Share Price Growth." growth

let create_stock_eval stock_data (s:State.t) =
  let player_idx = C.player in
  let b = s.backend in
  let price_s money = Money.print ~ks:false ~decimal:true ~region:b.params.region money in
  let money_s money = Money.print ~spaces:6 ~show_neg:false ~region:b.params.region money in
  let heading =
    (* Write player msg *)
    let msg = fst stock_data in
    let avg_growth = msg.Ui_msg.share_price_growth in
    Printf.sprintf
      "%s%s\n\
      %d,000 shares outstanding.\n\
      %s\n\
      Investors are %s.%s"
      (if msg.split then "Your stock splits\ntwo for one!\n" else "")
      (_stock_price_diff_s ~split:msg.split ~region:b.params.region msg.from_ msg.to_ msg.player_idx)
      (Stock_market.total_shares player_idx b.stocks)
      (_share_price_growth_s avg_growth)
      (Stock_market.investor_opinion avg_growth |> Stock_market.show_investor)
      (match msg.fired with
        | `Fired -> "\nYou are replaced by NEW MANAGEMENT!"
        | `Warning -> "\nThey are looking for a new president!"
        | `MinorWarning -> "\nThey may replace you as president!"
        | `Normal -> "")
  in
  let text =
    let player = B.get_player player_idx b in
    sp
    "%s %s per share\n\
    Profit:%s Cash:%s\n\
    Net Worth:%s Track: %d miles\n"
    (B.get_name player_idx b)
    (Stock_market.share_price player_idx b.stocks |> price_s)
    (Player.get_profit player |> money_s)
    (Player.get_cash player |> money_s)
    (Player.net_worth player |> money_s)
    (Player.track_length player)
  in
  let msgs = snd stock_data in
  let text =
    List.fold_left (fun acc Ui_msg.{from_; to_; player_idx; split; fired; _} ->
      if Owner.is_human player_idx then acc else
      let text1 = sp
        "\n%s\n\
        %s\n"
        (B.get_name player_idx b)
        (_stock_price_diff_s ~split ~region:b.params.region from_ to_ player_idx)
      in
      let text2 = match fired with
        | `Fired -> " --------- DISSOLVED ---------\n"
        | _ -> sp
          "Profit:%s Cash:%s\n\
          Net worth:%s Track: %d miles\n"
          (Ai.calc_profit player_idx b.ai |> money_s)
          (Ai.get_cash player_idx b.ai |> money_s)
          (Ai.get_net_worth player_idx b.ai |> money_s)
          (B.get_track_length player_idx b)
      in
      acc ^ text1 ^ text2)
    text 
    msgs
  in
  let stock_msgbox = Menu.MsgBox.make_basic ~x:2 ~y:2 ~heading ~fonts:s.fonts s text in
  { stock_msgbox; msgs }

  
let render_stock_eval win state (s:State.t) =
  let player_idx = C.player in
  let b = s.backend in
  let write text = Fonts.Render.write win s.fonts ~idx:4 ~color:Ega.black text in
  let _draw_background =
    R.paint_screen win ~color:Ega.green;
    R.draw_rect win ~color:Ega.yellow ~x:270 ~y:0 ~w:50 ~h:200 ~fill:true;
    R.draw_line win ~color:Ega.black ~x1:270 ~y1:0 ~x2:270 ~y2:200;
    Menu.MsgBox.render_box win 2 2 236 184;
  in
  Menu.MsgBox.render win s state.stock_msgbox;

  let draw_player_frame x y =
    let tex = Hashtbl.find s.textures.opponents CorneliusVanderbilt in
    R.Texture.render win ~x ~y tex;
    R.draw_rect win ~x:(x+2) ~y:(y+2) ~w:36 ~h:41 ~color:Ega.bcyan ~fill:true;
    let handle = Backend.get_handle player_idx b in
    write ~x:(x+8) ~y:(y+16) handle;
    let x1, x2 = x + 7, x + 33 in
    let y = y + 12 in
    R.draw_line win ~x1 ~y1:y ~x2 ~y2:y ~color:Ega.black;
    let y = y + 2 in
    R.draw_line win ~x1 ~y1:y ~x2 ~y2:y ~color:Ega.black;
    let y = y + 11 in
    R.draw_line win ~x1 ~y1:y ~x2 ~y2:y ~color:Ega.black;
    let y = y + 2 in
    R.draw_line win ~x1 ~y1:y ~x2 ~y2:y ~color:Ega.black;
  in

  let msgs = state.msgs in
  let _draw_portraits =
    let x = 276 in
    List.fold_left (fun y Ui_msg.{player_idx; _} ->
      let () =
        if Owner.is_human player_idx then
          draw_player_frame x y
        else
          let oppo = Ai.get_opponent player_idx b.ai
            |> Option.map Opponent.get_name |> Option.get
          in
          let tex = Hashtbl.find s.textures.opponents oppo in
          R.Texture.render win ~x ~y tex;
      in
      y + 49)
    2
    msgs
    |> ignore
  in
  let _write_rankings =
    let x = 286 in
    let w, h = 21, 9 in
    List.fold_left (fun (y, i) _ ->
      R.draw_rect win ~color:Ega.bgreen ~x ~y ~h ~w ~fill:true;
      (* draw shadow *)
      R.draw_line win ~color:Ega.green ~x1:x ~y1:y ~x2:x ~y2:(y+h);
      R.draw_line win ~color:Ega.green ~x1:x ~y1:(y+h) ~x2:(x+w-1) ~y2:(y+h);
      let text = match i with
       | 0 -> "1st"
       | 1 -> "2nd"
       | 2 -> "3rd"
       | _ -> sp "%dth" (i+1)
      in
      write text ~x:(x + 2) ~y:(y+1);
      (y + 49, i+1)
    )
    (40, 0)
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

let get_rate_war_msgs msgs =
  msgs
  |> List.filter (function | Ui_msg.RateWar _ -> true | _ -> false)
  |> List.map (function | Ui_msg.RateWar info -> info | _ -> assert false)

let get_job_msg msgs =
  msgs
  |> List.filter (function | Ui_msg.JobOffer _ -> true | _ -> false)
  |> List.map (function Ui_msg.JobOffer job -> job | _ -> assert false)
  |> List.head_opt

let handle_msgs backend msgs =
  let rate_war_msgs = get_rate_war_msgs msgs in
  let record_earnings = get_record_earnings backend msgs in
  let warnings = get_warnings backend msgs in
  let records = get_records backend msgs  in
  let stock_msgs = get_stock_msgs msgs in
  let job_msg = get_job_msg msgs in
  rate_war_msgs, record_earnings, warnings, records, stock_msgs, job_msg

