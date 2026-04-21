
type 'state t = {
  stock_msgbox: (unit, 'state) Engine.Menu.MsgBox.t;
  msgs: Ui_msg.share_price_change list;
}
