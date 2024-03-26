open! Containers
open Stock_broker_d
module C = Constants
module R = Renderer
module B = Backend

let src = Logs.Src.create "stock_broker" ~doc:"Stock_broker"
module Log = (val Logs.src_log src: Logs.LOG)

let sp = Printf.sprintf

let make_menu region fonts =
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
  let sell_stock_menu =
    let open MsgBox in
    make ~fonts ~x:2 ~y:8 [
      make_entry "S&ell 10,000 shares treasury stock" @@ `Action (`SellStock 0);
    ]
  in
  let buy_stock_menu =
    let open MsgBox in
    make ~fonts ~x:2 ~y:8 [
      make_entry "B&uy 10,000 shares treasury stock" @@ `Action (`BuyStock 0);
    ]
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
  let menu = make_menu s.backend.region s.fonts in
  {
    menu;
    msgbox=None;
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

  Option.iter (fun msgbox -> Menu.MsgBox.render win s msgbox) v.msgbox;
  ()

let handle_event (s:State.t) v (event:Event.t) =
  let menu, menu_action, event = Menu.Global.update s v.menu event in
  let exit, v, bk_action =
    let player = C.player in
    match menu_action with
    | Menu.On(`SellBond) -> false, v, B.Action.SellBond {player}
    | Menu.On(`RepayBond) -> false, v, B.Action.RepayBond {player}
    | Menu.On(`BuyStock stock) -> false, v, B.Action.BuyStock {player; stock}
    | Menu.On(`SellStock stock) -> false, v, B.Action.SellStock {player; stock}
    | Menu.On(`Declare_bankruptcy) -> false, v, B.Action.Declare_bankruptcy {player}
    | _ when Event.key_modal_dismiss event -> true, v, B.Action.NoAction
    | _ -> false, v, B.Action.NoAction
  in
  let v = [%up {v with menu}] in
  exit, v, bk_action

let handle_msg (s:State.t) v ui_msg =
  (* Create a msgbox *)
  let open Printf in
  let msgbox = match ui_msg with
    | Backend_d.StockBroker(BondSold{interest_rate; player}) when player = C.player ->
        Log.debug (fun f -> f "Received bond sold ui msg");
        let text = sprintf "%s bond sold\nat %d%% interest." (Utils.show_cash C.bond_value) interest_rate in
        let msgbox = Menu.MsgBox.make_basic ~x:180 ~y:8 ~fonts:s.fonts s text in
        Some msgbox
    | _ -> None
  in
  [%up {v with msgbox}]


