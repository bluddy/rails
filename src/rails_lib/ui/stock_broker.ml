open! Containers
open Stock_broker_d
module C = Constants
module R = Renderer
module B = Backend
open Utils.Infix

let sp = Printf.sprintf

let make_menu fonts =
  let open Menu in

  let titles =
    let cash_menu =
      let open MsgBox in
      make ~fonts ~x:16 ~y:8 [
        make_entry "&Sell $500,000 bond" @@ `Action `SellBond;
        make_entry "&Buy $500,000 bond" @@ `Action `RepayBond;
      ]
    in
    let buy_stock_menu =
      let open MsgBox in
      make ~fonts ~x:2 ~y:8 [
        make_entry "B&uy 10,000 shares treasury stock" @@ `Action (`BuyStock 0);
      ]
    in
    let sell_stock_menu =
      let open MsgBox in
      make ~fonts ~x:2 ~y:8 [
        make_entry "S&ell 10,000 shares treasury stock" @@ `Action (`SellStock 0);
      ]
    in
    let open Menu.Title in
    [
      make ~fonts ~x:7 ~y:1 "&Cash" cash_menu;
      make ~fonts ~x:72 ~y:1 "&Buy Stock" buy_stock_menu;
      make ~fonts ~x:136 ~y:1 "&Sell Stock" sell_stock_menu;
    ]
  in
  Menu.Global.make ~menu_h:C.menu_h titles

let make (s:State.t) =
  let menu = make_menu s.fonts in
  {
    menu;
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
    let color = match player.ai with
    | Some opponent ->
      let color = Ega.green in
      write ~color ~x:x_left ~y @@ sp "%s's" @@ Opponent.show opponent;
      let y = y + line in
      write ~color ~x:x_left ~y name;
      color

    | None ->
      write ~x:x_left ~y name;
      let y = y + line in
      write ~x:x_left ~y @@ sp "Track: %d miles" @@ player.track_length;
      Ega.black
    in
    let cash_s = Utils.show_cash ~region ~spaces:7 player.m.cash in
    write ~color ~x:x_right ~y @@ sp "Cash:%s" cash_s;
    let y = y + line in
    write ~color ~x:x_right ~y @@ sp "Bonds:%s" @@ Utils.show_cash ~region ~spaces:6 player.m.bonds;
    let y = y + line in
    write ~color ~x:x_left ~y @@ sp "Net Worth:%s" @@ Utils.show_cash ~region ~spaces:8 player.m.net_worth;
    write ~color ~x:x_right ~y @@ sp "Stock at %s.00/share" @@ Utils.show_cash ~ks:false ~region player.m.stock.share_price;
    let y = y + line in
    let treasury, non = Stocks.treasury_shares player.m.stock, Stocks.non_treasury_shares player.m.stock in
    write ~color ~x:x_left ~y @@ sp "Public: %d,000 Treasury %d,000" treasury non;
    y + line
  in
  let y =
    Array.fold (fun y player -> render_player player y)
    y
    s.backend.players
  in
  let y = y + line in
  write ~x:65 ~y @@ sp "Interest Rates: (%s) %d%%" (Climate.show s.backend.climate)
    (Climate.interest_rate s.backend.climate 0 |> Option.get_exn_or "Missing interest rate");
  Menu.Global.render win s s.fonts v.menu ~w:dims.screen.w ~h:C.menu_h;
  ()

let handle_event (s:State.t) v (event:Event.t) =
  let menu, menu_action, event = Menu.Global.update s v.menu event in
  let exit, bk_action =
  match menu_action, event with
  | Menu.On(`SellBond), _ -> true, B.Action.SellBond
  | Menu.On(`RepayBond), _ -> true, B.Action.RepayBond
  | Menu.On(`BuyStock i), _ -> true, B.Action.BuyStock i
  | Menu.On(`SellStock i), _ -> true, B.Action.SellStock i
  | _ -> false, B.Action.NoAction
  in
  let v = if menu === v.menu then v else {menu} in
  let exit = Event.pressed_esc event || exit in
  exit, v, bk_action

    
