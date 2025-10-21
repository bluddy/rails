open! Containers
open Stock_broker_d
module C = Constants
module R = Renderer
module B = Backend
module M = Money

open Utils.Infix

let src = Logs.Src.create "stock_broker" ~doc:"Stock_broker"
module Log = (val Logs.src_log src: Logs.LOG)

let sp = Printf.sprintf

let make_menu (b:Backend.t) cities region fonts (dims:Main_ui_d.dims) =
  let open Menu in
  let open MsgBox in
  let player_idx = C.player in
  let cash_menu =
    let check_bankruptcy (s:State.t) = Backend.check_bankruptcy player_idx s.backend in
    let check_bond (s:State.t) = Backend.player_has_bond player_idx s.backend in
    let open MsgBox in
    make ~fonts ~x:16 ~y:8 [
      make_entry (sp "&Sell %s bond" @@ M.print ~region @@ M.of_int 500)  @@ `Action `SellBond;
      make_entry ~test_enabled:check_bond (sp "&Buy %s bond" @@ M.print ~region @@ M.of_int 500) @@ `Action `RepayBond;
      make_entry ~test_enabled:check_bankruptcy "Declare Bankruptcy" @@ `Action `Declare_bankruptcy;
    ]
  in
  let create_stock_menu ~x ~y action f =
    let player_entry =
      let s = Printf.sprintf "%s 10,000 shares Treasury stock" action in
      (make_entry s @@ `Action(f player_idx))
    in
    let ai_entries =
      (Ai.ai_iter b.ai |> Iter.map (fun i ->
        let s =
          let name = Ai.get_name i ~cities b.ai in
          Printf.sprintf "%s 10,000 shares of %s" action name
        in
        make_entry s @@ `Action(f i)
      ) |> Iter.to_list)
    in
    make ~fonts ~x ~y @@ player_entry::ai_entries
  in
  let sell_stock_menu =
    create_stock_menu ~x:2 ~y:8 "Sell" (fun i -> `SellStock i)
  in
  let buy_stock_menu =
    create_stock_menu ~x:2 ~y:8 "Buy" (fun i -> `BuyStock i)
  in
  let operate_rr_menu =
    let companies = Backend.companies_controlled_by player_idx b in
    let company_menu company_idx =
      let has_money_to_take x _ =
        let cash = Backend.get_player company_idx b |> Player.get_cash in
        M.(cash >= x)
      in
      let has_money_to_give x _ =
        let cash = Backend.get_player player_idx b |> Player.get_cash in
        M.(cash >= x)
      in
      make ~fonts [
        make_entry "Financial Report" @@ `Action(`OperateRR(company_idx, `FinancialReport));
        make_entry (sp "Take %s" @@ M.print ~region @@ M.of_int 100) @@ `Action(`OperateRR(company_idx, `TakeMoney 100));
        make_entry ~test_enabled:(has_money_to_give @@ M.of_int 500)(sp "Take %s" @@ M.print ~region @@ M.of_int 250)
          @@ `Action(`OperateRR(company_idx, `TakeMoney 250));
        make_entry ~test_enabled:(has_money_to_take @@ M.of_int 2000) (sp "Take %s" @@ M.print ~region @@ M.of_int 500)
          @@ `Action(`OperateRR(company_idx, `TakeMoney 500));
        make_entry (sp "Give %s" @@ M.print ~region @@ M.of_int 100) @@ `Action(`OperateRR(company_idx, `GiveMoney 100));
        make_entry ~test_enabled:(has_money_to_give @@ M.of_int 500) (sp "Give %s" @@ M.print ~region @@ M.of_int 250)
          @@ `Action(`OperateRR(company_idx, `GiveMoney 200));
        make_entry "Build Track" @@ `Action(`OperateRR(company_idx, `BuildTrack));
        make_entry "Repay Bond" @@ `Action(`OperateRR(company_idx, `RepayBond));
      ]
    in
    let entries =
      let controls_company i (s:State.t) =
         Stock_market.controls_company player_idx ~target:i s.backend.stocks 
      in
      List.map (fun company_idx ->
        make_entry
          ~test_enabled:(controls_company company_idx)
          (Backend.get_name company_idx b) @@
          `MsgBox(company_menu company_idx))
      companies
    in
    make ~fonts entries
  in
  let titles =
    let owns_some_company (s:State.t) =
      Stock_market.controls_any_other_company player_idx s.backend.stocks
    in
    let open Menu.Title in
    [
      make ~fonts ~x:7 ~y:1 "&Cash" cash_menu;
      make ~fonts ~x:72 ~y:1 "&Buy Stock" buy_stock_menu;
      make ~fonts ~x:136 ~y:1 "&Sell Stock" sell_stock_menu;
      make ~test_enabled:owns_some_company ~fonts ~x:220 ~y:1 "&Operate RR" operate_rr_menu;
    ]
  in
  Menu.Animated.make_global fonts ~h:dims.menu.h ~w:dims.menu.w titles

let make (s:State.t) =
  let b = s.backend in
  let menu = make_menu b b.cities (B.get_region b) s.fonts s.ui.dims in
  {
    menu;
    modal = Normal;
  }

let render win (s:State.t) v =
  let player_idx = C.player in
  R.paint_screen win ~color:Ega.white;
  let dims = s.ui.dims in
  let write ?(color=Ega.black) = Fonts.Render.write win s.fonts ~idx:`Standard ~color in
  R.draw_rect win ~x:2 ~y:(2 + C.menu_h) ~w:(dims.screen.w - 4) ~h:(dims.screen.h - 4 - C.menu_h) ~color:Ega.black ~fill:false;
  write ~x:100 ~y:12 "Financial Summaries";

  let region = B.get_region s.backend in
  let x_left, x_right = 8, 160 in
  let y = 24 in
  let line = 8 in

  let render_player_info (player_idx:Owner.t) backend y =
    let name = B.get_name player_idx backend in
    let is_ai, color = 
      if Owner.is_human player_idx then (
        write ~x:x_left ~y name;
        let y = y + line in
        write ~x:x_left ~y @@ sp "Track: %d miles" @@ B.get_track_length player_idx backend;
        false, Ega.black
      ) else (
        let ai = Ai.get_ai_exn player_idx backend.ai in
        let color = Ega.green in
        write ~color ~x:x_left ~y @@ sp "%s's" @@ Opponent.show ai.opponent;
        let y = y + line in
        write ~color ~x:x_left ~y name;
        true, color
      )
    in
    let cash_s = M.print ~region ~spaces:7 @@ B.get_cash player_idx backend in
    write ~color ~x:x_right ~y @@ sp "Cash:%s" cash_s;
    let y = y + line in
    write ~color ~x:x_right ~y @@ sp "Bonds:%s" @@ M.print ~region ~spaces:6 @@ B.get_bonds player_idx backend;
    let y = y + line in
    write ~color ~x:x_left ~y @@ sp "Net Worth:%s" @@ M.print ~region ~spaces:8 @@ B.get_net_worth player_idx backend;
    let per_s = if is_ai then "/" else " per " in
    write ~color ~x:x_right ~y @@ sp "Stock at %s.00%sshare"
      (M.print ~ks:false ~region @@ Stock_market.share_price player_idx s.backend.stocks) per_s;
    let y = y + line in
    let treasury, non = Stock_market.treasury_shares player_idx s.backend.stocks, Stock_market.non_treasury_shares player_idx s.backend.stocks in
    write ~color ~x:x_left ~y @@ sp "Public: %d,000 Treasury %d,000" non treasury;
    y + line + line
  in
  let y = Iter.fold (fun y player_idx -> render_player_info player_idx s.backend y) y @@ B.players_and_ai s.backend in
  write ~x:65 ~y @@ sp "Interest Rates: (%s) %d%%"
    (Climate.show @@ B.get_climate s.backend)
    (Backend.get_interest_rate s.backend player_idx);

  let x, y = 275, 50 in
  let render_opponent player_idx textures (b:Backend.t) y =
    if Owner.is_human player_idx then y else
    let ai = Ai.get_ai_exn player_idx b.ai in
    let tex = Hashtbl.find textures ai.opponent.name in
    R.Texture.render win ~x ~y tex;
    y + 50
  in
  Iter.fold (fun y player_idx -> render_opponent player_idx s.textures.opponents s.backend y) y @@ B.players_and_ai s.backend |> ignore;

  Menu.Animated.render win s v.menu;

  match v.modal with
  | Normal -> ()
  | MsgBox msgbox -> Menu.MsgBox.render win s msgbox
  | Confirm_menu msgbox -> Menu.MsgBox.render win s msgbox
  | Newspaper newspaper -> Newspaper.render win s newspaper
  | RR_build state -> Rr_command.render win s.fonts state

let handle_modal_event (s:State.t) modal (event:Event.t) time =
  let player_idx = C.player in
  let nobaction = B.Action.NoAction in
  match modal with
  | Normal -> `Exit, Normal, nobaction
  | MsgBox msgbox -> 
     begin match Menu.modal_handle_event ~is_msgbox:true s msgbox event time with
     | `Stay _ -> `Stay, modal, nobaction
     | _ -> `Exit, Normal, nobaction
     end
  | Newspaper newspaper ->
     begin match Newspaper.handle_event s newspaper event time with
     | `Stay -> `Stay, modal, nobaction
     | `Exit -> `Exit, Normal, nobaction
     end
  | Confirm_menu menu ->
     begin match Menu.modal_handle_event ~is_msgbox:false s menu event time with
     | `Stay modal -> `Stay, Confirm_menu modal, nobaction
     | `Activate(`BuyStock stock) -> `Stay, Normal, B.Action.BuyStock{player_idx; stock}
     | `Activate(`Declare_bankruptcy) -> `Stay, Normal, B.Action.Declare_bankruptcy{player_idx}
     | `Activate `None -> `Stay, modal, nobaction
     | `Exit -> `Exit, Normal, nobaction
     end
  | RR_build state ->
      let cities = s.backend.cities in
      begin match Rr_command.handle_event event cities state with
      | _, `Route (ai, src, dst) ->
         let action = B.Action.OperateRR {player_idx; company=ai; action=B.Action.RRBuildTrack(src, dst)} in
         `Exit, Normal, action
      | _, `Exit -> `Exit, Normal, nobaction
      | state2, _ when state === state2 -> `Stay, modal, nobaction
      | state2, _ -> `Stay, RR_build state2, nobaction
      end

let handle_event (s:State.t) v (event:Event.t) time =
  let nobaction = B.Action.NoAction in
  match v.modal with
  | Normal ->
      let menu, event = Menu.Animated.handle_event s v.menu event time in
      let status = if Event.key_modal_dismiss event then `Exit else `Stay in
      status, [%up {v with menu}], nobaction

  | modal ->
      let exit, modal, bk_action = handle_modal_event s modal event time in
      let v = [%up {v with modal}] in
      exit, v, bk_action

let handle_tick (s:State.t) v time =
  let basic_msgbox text = MsgBox(Menu.MsgBox.make_basic ~x:80 ~y:8 ~fonts:s.fonts s text) in
  let b = s.backend in
  let player_idx = C.player in
  match v.modal with
  | Normal ->
    let menu, menu_action = Menu.Animated.handle_tick s v.menu time in
    let exit, v, bk_action = match menu_action with
    | Menu.On(`SellBond) -> `Stay, v, [B.Action.SellBond {player_idx}]
    | Menu.On(`RepayBond) -> `Stay, v, [B.Action.RepayBond {player_idx}]
    | Menu.On(`BuyStock stock) ->
      let difficulty = B.get_difficulty b in
      begin match Stock_market.can_buy_stock player_idx ~target:stock ~cash:(B.get_cash player_idx b) b.params b.stocks with
      | `Ok -> `Stay, v, [B.Action.BuyStock {player_idx; stock}]
      | `Error -> `Stay, v, []
      | `Anti_trust_violation max_num ->
          let msgbox = Menu.MsgBox.make_basic ~x:180 ~y:8 ~fonts:s.fonts s @@
            Printf.sprintf "Anti-Trust Violation\nAs a %s you are\nonly authorized to invest\nin %d other RailRoad%s."
              (B_options.show_difficulty difficulty)
              max_num (if max_num > 1 then "s" else "")
          in
          `Stay, {v with modal=MsgBox msgbox}, []
      | `Offer_takeover(share_price, shares_to_buy) ->
          let open Menu in
          let open MsgBox in
          let menu =
            let text = Printf.sprintf "Management demands %s.00\n per share for treasury stock.\nTotal Cost: %s.\n"
               (M.print ~ks:false share_price)
               (M.print @@ M.(share_price * shares_to_buy))
            in
            make ~fonts:s.fonts ~x:180 ~y:8 [
              static_entry ~color:Ega.white text;
              make_entry "Never Mind" @@ `Action `None;
              make_entry "Buy Stock" @@ `Action (`BuyStock stock);
            ]
          in
          `Stay, {v with modal=Confirm_menu(menu)}, []
      end
    | Menu.On(`SellStock stock) ->
        `Stay, v, [B.Action.SellStock {player_idx; stock}]
    | Menu.On(`Declare_bankruptcy) ->
        let menu =
          let text = "Are you sure you want\nto declare bankruptcy?" in
          let open Menu.MsgBox in
          make ~fonts:s.fonts ~x:180 ~y:8 [
            static_entry ~color:Ega.white text;
            make_entry "Oops!" @@ `Action `None;
            make_entry "YES" @@ `Action(`Declare_bankruptcy);
          ]
        in
        `Stay, {v with modal=Confirm_menu(menu)}, []
    | Menu.On(`OperateRR (company_idx, `FinancialReport)) ->
        let ai = Ai.get_ai_exn company_idx b.ai in
        let build_order_s = match Ai.get_build_order company_idx b.ai with
          | None -> ""
          | Some(src, dst) ->
            sp "\n\nSurveying route from\n%s to %s."
            (Cities.name_of_loc src b.cities)
            (Cities.name_of_loc dst b.cities)
        in
        let region = B.get_region b in
        let text = sp "%s\nRevenue YTD: %s\nYearly Interest: %s%s"
          (B.get_name company_idx b)
          (Ai.get_revenue_ytd ai |> M.print ~region)
          (Ai.get_yearly_interest ai |> M.print ~region)
          build_order_s
        in
        `Stay, {v with modal=basic_msgbox text}, []

    | Menu.On(`OperateRR (company, `TakeMoney amount)) ->
        `Stay, v, [OperateRR{player_idx; company; action=RRTakeMoney(M.of_int amount)}]

    | Menu.On(`OperateRR (company, `GiveMoney amount)) ->
        `Stay, v, [OperateRR{player_idx; company; action=RRGiveMoney(M.of_int amount)}]

    | Menu.On(`OperateRR (company, `BuildTrack)) ->
        `Stay, {v with modal=RR_build(Rr_command.make company)}, []

    | Menu.On(`OperateRR (company, `RepayBond)) ->
        `Stay, v, [OperateRR{player_idx; company; action=RRRepayBond}]

    | _ -> `Stay, v, []
    in
    let v = [%up {v with menu}] in
    exit, v, bk_action

  | _ -> `Stay, v, []

let handle_msg (s:State.t) v ui_msg =
  (* Create a msgbox *)
  let show_cash = M.print ~show_neg:false ~region:(B.get_region s.backend) in
  let modal =
    let basic_msgbox text = MsgBox(Menu.MsgBox.make_basic ~x:80 ~y:8 ~fonts:s.fonts s text) in
    match ui_msg with
    | Ui_msg.StockBroker x -> begin match x with
      | BondSold {interest_rate; player_idx} when Owner.(player_idx = C.player) ->
          let text = sp "%s bond sold\nat %d%% interest." (show_cash C.bond_value) interest_rate in
          basic_msgbox text
      | BondRepaid {player_idx} when Owner.(player_idx = C.player) ->
          let text = sp "%s bond repaid." (show_cash C.bond_value) in
          basic_msgbox text
      | StockSold {player_idx; stock; cost} when Owner.(player_idx = C.player && stock = player_idx) ->
          let text = sp "10,000 shares of\ncompany stock sold for\n------ %s ------." (show_cash cost) in
          basic_msgbox text
      | StockSold {player_idx; stock; cost} when Owner.(player_idx = C.player) ->
          let text = sp "10,000 shares of\n%s\nstock sold\n for %s."
            (B.get_name stock s.backend) (show_cash cost)
          in
          basic_msgbox text
      | StockBought{player_idx; stock; cost} when Owner.(player_idx = C.player && stock = player_idx) ->
          let text = sp "10,000 shares of company\nstock purchased for\n------ %s ------." (show_cash cost) in
          basic_msgbox text
      | StockBought{player_idx; stock; cost} when Owner.(player_idx = C.player) ->
          let text = sp "10,000 shares of\n%s\nstock purchased for %s."
            (B.get_name stock s.backend) (show_cash cost)
          in
          basic_msgbox text
      | Takeover {player_idx; stock} when Owner.(player_idx = C.player) ->
          let text = sp "You take control of the\n%s!" (B.get_name stock s.backend ) in
          basic_msgbox text
      | MoneyTransferredFrom {player_idx; company; amount} when Owner.(player_idx = C.player) ->
          let text = sp "%s transferred from\n%s" (show_cash amount) (B.get_name company s.backend ) in
          basic_msgbox text
      | MoneyTransferredTo {player_idx; company; amount} when Owner.(player_idx = C.player) ->
          let text = sp "%s transferred to\n%s" (show_cash amount) (B.get_name company s.backend) in
          basic_msgbox text
      | AiBondRepaid {player_idx; _} when Owner.(player_idx = C.player) ->
          let text = sp "%s bond repaid." (show_cash @@ M.of_int 500) in
          basic_msgbox text
      | BankruptcyDeclared {player_idx} when Owner.(player_idx = C.player) ->
          let company_name = Backend.get_name player_idx s.backend in
          let text = sp "%s\nBankruptcy declared!" company_name in
          Newspaper(Newspaper.make_simple s Newspaper.FinancialNews text None)
      | _ -> Normal
      end
    | _ -> Normal
  in
  [%up {v with modal}]

