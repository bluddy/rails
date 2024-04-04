open! Containers
open Stock_broker_d
module C = Constants
module R = Renderer
module B = Backend

let src = Logs.Src.create "stock_broker" ~doc:"Stock_broker"
module Log = (val Logs.src_log src: Logs.LOG)

let sp = Printf.sprintf

let make_menu players stations cities region fonts =
  let open Menu in
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
    let open MsgBox in
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
  let titles =
    let open Menu.Title in
    [
      make ~fonts ~x:7 ~y:1 "&Cash" cash_menu;
      make ~fonts ~x:72 ~y:1 "&Buy Stock" buy_stock_menu;
      make ~fonts ~x:136 ~y:1 "&Sell Stock" sell_stock_menu;
    ]
  in
  Menu.Global.make ~menu_h:C.menu_h titles

let make (s:State.t) =
  let b = s.backend in
  let menu = make_menu b.players b.stations b.cities b.region s.fonts in
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
    | Some opponent ->
      let color = Ega.green in
      write ~color ~x:x_left ~y @@ sp "%s's" @@ Opponent.show opponent;
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

  Option.iter (function 
    | MsgBox msgbox -> Menu.MsgBox.render win s msgbox
    | Confirm_menu msgbox -> Menu.MsgBox.render win s msgbox)
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
  | Confirm_menu menu ->
     begin match Menu.modal_handle_event ~is_msgbox:false s menu event with
     | `Stay modal -> false, Some (Confirm_menu modal), nobaction
     | `Activate (`BuyStock stock) -> false, None, B.Action.BuyStock{player=C.player; stock}
     | `Activate `None -> false, Some modal, nobaction
     | `Exit -> true, None, nobaction
     end
  
  

let handle_event (s:State.t) v (event:Event.t) =
  match v.modal with
  | Some modal ->
    let exit, modal, bk_action = handle_modal_event s modal event in
    let v = [%up {v with modal}] in
    exit, v, bk_action
  | None ->
    let menu, menu_action, event = Menu.Global.update s v.menu event in
    let exit, v, bk_action =
      match menu_action with
      | Menu.On(`SellBond) -> false, v, B.Action.SellBond {player=C.player}
      | Menu.On(`RepayBond) -> false, v, B.Action.RepayBond {player=C.player}
      | Menu.On(`BuyStock stock) ->
        let difficulty = s.backend.options.difficulty in
        begin match Player.can_buy_stock s.backend.players ~player_idx:C.player ~target_idx:stock ~difficulty with
        | `Ok -> false, v, B.Action.BuyStock {player=C.player; stock}
        | `Error -> false, v, B.Action.NoAction
        | `Anti_trust_violation max_num ->
            let msgbox = Menu.MsgBox.make_basic ~x:180 ~y:8 ~fonts:s.fonts s @@
              Printf.sprintf "Anti-Trust Violation\nAs a %s you are\nonly authorized to invest\nin %d other RailRoad%s."
                (B_options.show_difficulty difficulty)
                max_num (if max_num > 1 then "s" else "")
            in
            false, {v with modal=Some (MsgBox msgbox)}, B.Action.NoAction
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
            false, {v with modal=Some(Confirm_menu(menu))}, B.Action.NoAction
        end
      | Menu.On(`SellStock stock) -> false, v, B.Action.SellStock {player=C.player; stock}
      | Menu.On(`Declare_bankruptcy) -> false, v, B.Action.Declare_bankruptcy {player=C.player}
      | _ when Event.key_modal_dismiss event -> true, v, B.Action.NoAction
      | _ -> false, v, B.Action.NoAction
    in
    let v = [%up {v with menu}] in
    exit, v, bk_action

let handle_msg (s:State.t) v ui_msg =
  (* Create a msgbox *)
  let open Printf in
  let show_cash = Utils.show_cash ~show_neg:false ~region:s.backend.region in
  let modal =
    let basic_msgbox text = Some(MsgBox(Menu.MsgBox.make_basic ~x:80 ~y:8 ~fonts:s.fonts s text)) in
    match ui_msg with
    | Backend_d.StockBroker x -> begin match x with
      | BondSold{interest_rate; player} when player = C.player ->
          let text = sprintf "%s bond sold\nat %d%% interest." (show_cash C.bond_value) interest_rate in
          basic_msgbox text
      | BondRepaid{player} when player = C.player ->
          let text = sprintf "%s bond repaid." (show_cash C.bond_value) in
          basic_msgbox text
      | StockSold{player; stock; cost} when player = C.player && stock = player ->
          let text = sprintf "10,000 shares of\ncompany stock sold for\n------ %s ------." (show_cash cost) in
          basic_msgbox text
      | StockSold{player; stock; cost} when player = C.player ->
          let text = sprintf "10,000 shares of\n%s\nstock sold\n for %s."
            (B.get_company_name s.backend stock) (show_cash cost)
          in
          basic_msgbox text
      | StockBought{player; stock; cost} when player = C.player && stock = player ->
          let text = sprintf "10,000 shares of company\nstock purchased for\n------ %s ------." (show_cash cost) in
          basic_msgbox text
      | StockBought{player; stock; cost} when player = C.player ->
          let text = sprintf "10,000 shares of\n%s\nstock purchased for %s."
            (B.get_company_name s.backend stock) (show_cash cost)
          in
          basic_msgbox text
      | Takeover {player; stock} when player = C.player ->
          let text = sprintf "You take control of the\n%s!" (B.get_company_name s.backend stock) in
          basic_msgbox text
      end
    | _ -> None
  in
  [%up {v with modal}]


