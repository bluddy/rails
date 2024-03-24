open! Containers
open Stock_broker_d
module C = Constants
module R = Renderer
module B = Backend
open Utils.Infix

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
  Menu.Global.render win s s.fonts v.menu ~w:dims.screen.w ~h:C.menu_h;
  R.draw_rect win ~x:2 ~y:(2 + C.menu_h) ~w:(dims.screen.w - 4) ~h:(dims.screen.h - 4 - C.menu_h) ~color:Ega.black ~fill:false;
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

    
