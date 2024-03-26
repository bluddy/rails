open! Containers

type msg = [
  | `SellBond
  | `RepayBond
  | `BuyStock of int (* player *)
  | `SellStock of int (* player *)
  | `Declare_bankruptcy
  (* TODO: operate RR menu *)
  ]

type 'state t = {
  menu: (msg, 'state) Menu.Global.t;
  msgbox: (unit, 'state) Menu.MsgBox.t option;
}
