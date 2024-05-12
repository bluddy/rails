open! Containers
open Stock_broker_d
module C = Constants
module R = Renderer
module B = Backend

let src = Logs.Src.create "stock_broker" ~doc:"Stock_broker"
module Log = (val Logs.src_log src: Logs.LOG)

let sp = Printf.sprintf

let make_menu b players stations cities region fonts =
  let open Menu in
  let open MsgBox in
  let cash_menu =
    let check_bankruptcy (s:State.t) = Backend.check_bankruptcy s.backend C.player in
    let check_bond (s:State.t) = Backend.player_has_bond s.backend C.player in
    let open MsgBox in
    make ~fonts ~x:16 ~y:8 [
      make_entry (sp "&Sell %s bond" @@ Utils.show_cash ~region 500)  @@ `Action `SellBond;
      make_entry ~test_enabled:check_bond (sp "&Buy %s bond" @@ Utils.show_cash ~region 500) @@ `Action `RepayBond;
      make_entry ~test_enabled:check_bankruptcy "Declare Bankruptcy" @@ `Action `Declare_bankruptcy;
    ]
  in
  let create_stock_menu ~x ~y action f =
    let l =
      Array.to_iter players |> Iter.mapi (fun i player ->
        let s =
          if i = C.player then
            Printf.sprintf "%s 10,000 shares Treasury stock" action
          else
            let name = Player.get_name player stations cities in
            Printf.sprintf "%s 10,000 shares of %s" action name
        in
        make_entry s @@ `Action(f i)
      ) |> Iter.to_list
    in
    make ~fonts ~x ~y l
  in
  let sell_stock_menu =
    create_stock_menu ~x:2 ~y:8 "Sell" (fun i -> `SellStock i)
  in
  let buy_stock_menu =
    create_stock_menu ~x:2 ~y:8 "Buy" (fun i -> `BuyStock i)
  in
  let operate_rr_menu =
    let companies = Backend.companies_controlled_by b C.player in
    let company_menu company_idx =
      let has_money_to_take x _ =
        let cash = Backend.get_player b company_idx |> Player.get_cash in
        cash >= x
      in
      let has_money_to_give x _ =
        let cash = Backend.get_player b C.player |> Player.get_cash in
        cash >= x
      in
      make ~fonts [
        make_entry "Financial Report" @@ `Action(`OperateRR(company_idx, `FinancialReport));
        make_entry (sp "Take %s" @@ Utils.show_cash ~region 100) @@ `Action(`OperateRR(company_idx, `TakeMoney 100));
        make_entry ~test_enabled:(has_money_to_give 500)(sp "Take %s" @@ Utils.show_cash ~region 250) @@ `Action(`OperateRR(company_idx, `TakeMoney 250));
        make_entry ~test_enabled:(has_money_to_take 2000) (sp "Take %s" @@ Utils.show_cash ~region 500) @@ `Action(`OperateRR(company_idx, `TakeMoney 500));
        make_entry (sp "Give %s" @@ Utils.show_cash ~region 100) @@ `Action(`OperateRR(company_idx, `GiveMoney 100));
        make_entry ~test_enabled:(has_money_to_give 500) (sp "Give %s" @@ Utils.show_cash ~region 250) @@ `Action(`OperateRR(company_idx, `GiveMoney 200));
        make_entry "Build Track" @@ `Action(`OperateRR(company_idx, `BuildTrack));
        make_entry "Repay Bond" @@ `Action(`OperateRR(company_idx, `RepayBond));
      ]
    in
    let owns_company i (s:State.t) =
       Player.owns_company_by_idx s.backend.players ~player_idx:C.player ~company_idx:i
    in
    let entries =
      List.map (fun company_idx ->
        make_entry
          ~test_enabled:(owns_company company_idx)
          (Backend.get_company_name b company_idx) @@
          `MsgBox(company_menu company_idx))
      companies
    in
    make ~fonts entries
  in
  let titles =
    let open Menu.Title in
    [
      make ~fonts ~x:7 ~y:1 "&Cash" cash_menu;
      make ~fonts ~x:72 ~y:1 "&Buy Stock" buy_stock_menu;
      make ~fonts ~x:136 ~y:1 "&Sell Stock" sell_stock_menu;
      make ~fonts ~x:220 ~y:1 "&Operate RR" operate_rr_menu;
    ]
  in
  Menu.Global.make ~menu_h:C.menu_h titles

let make (s:State.t) =
  let b = s.backend in
  let menu = make_menu b b.players b.stations b.cities b.region s.fonts in
  {
    menu;
    modal = None;
  }

let render win (s:State.t) v =
  R.paint_screen win ~color:Ega.white;
  let dims = s.ui.dims in
  let write ?(color=Ega.black) = Fonts.Render.write win s.fonts ~idx:4 ~color in
  R.draw_rect win ~x:2 ~y:(2 + C.menu_h) ~w:(dims.screen.w - 4) ~h:(dims.screen.h - 4 - C.menu_h) ~color:Ega.black ~fill:false;
  write ~x:100 ~y:12 "Financial Summaries";

  let region = s.backend.region in
  let x_left, x_right = 8, 160 in
  let y = 24 in
  let line = 8 in

  let render_player player y =
    let name = Player.get_name player s.backend.stations s.backend.cities in
    (* TODO: render owned stock by player in ai *)
    let is_ai, color = match player.ai with
    | Some ai ->
      let color = Ega.green in
      write ~color ~x:x_left ~y @@ sp "%s's" @@ Opponent.show ai.opponent;
      let y = y + line in
      write ~color ~x:x_left ~y name;
      true, color

    | None ->
      write ~x:x_left ~y name;
      let y = y + line in
      write ~x:x_left ~y @@ sp "Track: %d miles" @@ player.track_length;
      false, Ega.black
    in
    let cash_s = Utils.show_cash ~region ~spaces:7 player.m.cash in
    write ~color ~x:x_right ~y @@ sp "Cash:%s" cash_s;
    let y = y + line in
    write ~color ~x:x_right ~y @@ sp "Bonds:%s" @@ Utils.show_cash ~region ~spaces:6 player.m.bonds;
    let y = y + line in
    write ~color ~x:x_left ~y @@ sp "Net Worth:%s" @@ Utils.show_cash ~region ~spaces:8 player.m.net_worth;
    let per_s = if is_ai then "/" else " per " in
    write ~color ~x:x_right ~y @@ sp "Stock at %s.00%sshare"
      (Utils.show_cash ~ks:false ~region player.m.stock.share_price) per_s;
    let y = y + line in
    let treasury, non = Stocks.treasury_shares player.m.stock, Stocks.non_treasury_shares player.m.stock in
    write ~color ~x:x_left ~y @@ sp "Public: %d,000 Treasury %d,000" non treasury;
    y + line
  in
  let y =
    Array.fold (fun y player -> render_player player y)
    y
    s.backend.players
  in
  let y = y + line in
  write ~x:65 ~y @@ sp "Interest Rates: (%s) %d%%" (Climate.show s.backend.climate)
    (Backend.get_interest_rate s.backend C.player);

  Menu.Global.render win s s.fonts v.menu ~w:dims.screen.w ~h:C.menu_h;

  Option.iter begin function 
    | MsgBox msgbox -> Menu.MsgBox.render win s msgbox
    | Confirm_menu msgbox -> Menu.MsgBox.render win s msgbox
    | Newspaper newspaper -> Newspaper.render win s newspaper
    end
    v.modal;
  ()

let handle_modal_event (s:State.t) modal (event:Event.t) =
  let nobaction = B.Action.NoAction in
  match modal with
  | MsgBox msgbox -> 
     begin match Menu.modal_handle_event ~is_msgbox:true s msgbox event with
     | `Stay _ -> false, Some modal, nobaction
     | _ -> true, None, nobaction
     end
  | Newspaper newspaper ->
     begin match Newspaper.handle_event s newspaper event with
     | `Stay -> false, Some modal, nobaction
     | `Exit -> true, None, nobaction
     end
  | Confirm_menu menu ->
     begin match Menu.modal_handle_event ~is_msgbox:false s menu event with
     | `Stay modal -> false, Some (Confirm_menu modal), nobaction
     | `Activate(`BuyStock stock) -> false, None, B.Action.BuyStock{player=C.player; stock}
     | `Activate(`Declare_bankruptcy) -> false, None, B.Action.Declare_bankruptcy{player=C.player}
     | `Activate `None -> false, Some modal, nobaction
     | `Exit -> true, None, nobaction
     end
  
  

let handle_event (s:State.t) v (event:Event.t) =
  let basic_msgbox text = Some(MsgBox(Menu.MsgBox.make_basic ~x:80 ~y:8 ~fonts:s.fonts s text)) in
  let nobaction = B.Action.NoAction in
  let b = s.backend in
  match v.modal with
  | Some modal ->
    let exit, modal, bk_action = handle_modal_event s modal event in
    let v = [%up {v with modal}] in
    exit, v, bk_action
  | None ->
    let menu, menu_action, event = Menu.Global.update s v.menu event in
    let exit, v, bk_action = match menu_action with
    | Menu.On(`SellBond) -> false, v, B.Action.SellBond {player=C.player}
    | Menu.On(`RepayBond) -> false, v, B.Action.RepayBond {player=C.player}
    | Menu.On(`BuyStock stock) ->
      let difficulty = b.options.difficulty in
      begin match Player.can_buy_stock b.players ~player_idx:C.player ~target_idx:stock ~difficulty with
      | `Ok -> false, v, B.Action.BuyStock {player=C.player; stock}
      | `Error -> false, v, B.Action.NoAction
      | `Anti_trust_violation max_num ->
          let msgbox = Menu.MsgBox.make_basic ~x:180 ~y:8 ~fonts:s.fonts s @@
            Printf.sprintf "Anti-Trust Violation\nAs a %s you are\nonly authorized to invest\nin %d other RailRoad%s."
              (B_options.show_difficulty difficulty)
              max_num (if max_num > 1 then "s" else "")
          in
          false, {v with modal=Some (MsgBox msgbox)}, nobaction
      | `Offer_takeover(share_price, shares_to_buy) ->
          let open Menu in
          let open MsgBox in
          let menu =
            let text = Printf.sprintf "Management demands %s.00\n per share for treasury stock.\nTotal Cost: %s.\n"
               (Utils.show_cash ~ks:false share_price)
               (Utils.show_cash @@ share_price * shares_to_buy)
            in
            make ~fonts:s.fonts ~x:180 ~y:8 [
              static_entry ~color:Ega.white text;
              make_entry "Never Mind" @@ `Action `None;
              make_entry "Buy Stock" @@ `Action (`BuyStock stock);
            ]
          in
          false, {v with modal=Some(Confirm_menu(menu))}, nobaction
      end
    | Menu.On(`SellStock stock) ->
        false, v, B.Action.SellStock {player=C.player; stock}
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
        false, {v with modal=Some(Confirm_menu(menu))}, nobaction
    | Menu.On(`OperateRR (company_idx, `FinancialReport)) ->
        let player = Backend.get_player b company_idx in
        let build_order_s = Option.map_or ~default:"" (fun ((x1, y1), (x2, y2)) ->
          sp "\nSurveying route from\n%s to %s."
            (Cities.find_exn b.cities x1 y1 |> fst)
            (Cities.find_exn b.cities x2 y2 |> fst)) @@
          Player.build_order player
        in
        let text = sp "%s\nRevenue YTD: %s\nYearly Interest: %s%s"
          (Player.get_name player b.stations b.cities)
          (Income_statement.total_revenue player.m.income_statement
           |> Utils.show_cash ~region:b.region)
          (player.m.yearly_interest_payment
           |> Utils.show_cash ~region:b.region)
          build_order_s
        in
        false, {v with modal=basic_msgbox text}, nobaction

    | Menu.On(`OperateRR (company, `TakeMoney amount)) ->
        false, v, OperateRR{player=C.player; company; action=RRTakeMoney amount}

    | Menu.On(`OperateRR (company, `GiveMoney amount)) ->
        false, v, OperateRR{player=C.player; company; action=RRGiveMoney amount}

      (* TODO: fill this in *)
    | Menu.On(`OperateRR (company, `BuildTrack)) ->
        false, v, OperateRR{player=C.player; company; action=RRBuildTrack((0,0),(0,0))}

    | Menu.On(`OperateRR (company, `RepayBond)) ->
        false, v, OperateRR{player=C.player; company; action=RRRepayBond}
      
    | _ when Event.key_modal_dismiss event ->
        true, v, nobaction
    | _ ->
        false, v, nobaction
    in
    let v = [%up {v with menu}] in
    exit, v, bk_action

let handle_msg (s:State.t) v ui_msg =
  (* Create a msgbox *)
  let show_cash = Utils.show_cash ~show_neg:false ~region:s.backend.region in
  let modal =
    let basic_msgbox text = Some(MsgBox(Menu.MsgBox.make_basic ~x:80 ~y:8 ~fonts:s.fonts s text)) in
    match ui_msg with
    | Backend_d.StockBroker x -> begin match x with
      | BondSold {interest_rate; player} when player = C.player ->
          let text = sp "%s bond sold\nat %d%% interest." (show_cash C.bond_value) interest_rate in
          basic_msgbox text
      | BondRepaid {player} when player = C.player ->
          let text = sp "%s bond repaid." (show_cash C.bond_value) in
          basic_msgbox text
      | StockSold {player; stock; cost} when player = C.player && stock = player ->
          let text = sp "10,000 shares of\ncompany stock sold for\n------ %s ------." (show_cash cost) in
          basic_msgbox text
      | StockSold {player; stock; cost} when player = C.player ->
          let text = sp "10,000 shares of\n%s\nstock sold\n for %s."
            (B.get_company_name s.backend stock) (show_cash cost)
          in
          basic_msgbox text
      | StockBought{player; stock; cost} when player = C.player && stock = player ->
          let text = sp "10,000 shares of company\nstock purchased for\n------ %s ------." (show_cash cost) in
          basic_msgbox text
      | StockBought{player; stock; cost} when player = C.player ->
          let text = sp "10,000 shares of\n%s\nstock purchased for %s."
            (B.get_company_name s.backend stock) (show_cash cost)
          in
          basic_msgbox text
      | Takeover {player; stock} when player = C.player ->
          let text = sp "You take control of the\n%s!" (B.get_company_name s.backend stock) in
          basic_msgbox text
      | MoneyTransferredFrom {player; company; amount} when player = C.player ->
          let text = sp "%s transferred from\n%s" (show_cash amount) (B.get_company_name s.backend company) in
          basic_msgbox text
      | MoneyTransferredTo {player; company; amount} when player = C.player ->
          let text = sp "%s transferred to\n%s" (show_cash amount) (B.get_company_name s.backend company) in
          basic_msgbox text
      | AiBondRepaid {player; _} when player = C.player ->
          let text = sp "%s bond repaid." (show_cash 500) in
          basic_msgbox text
      | BankruptcyDeclared {player} when player = C.player ->
          let company_name = Backend.get_company_name s.backend player in
          let text = sp "%s\nBankruptcy declared!" company_name in
          Some(Newspaper(Newspaper.make s Newspaper.FinancialNews text None))
      | _ -> None
      end
    | _ -> None
  in
  [%up {v with modal}]


