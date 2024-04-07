open! Containers

type msg = [
  | `SellBond
  | `RepayBond
  | `BuyStock of int (* player *)
  | `SellStock of int (* player *)
  | `Declare_bankruptcy
  | `OperateRR of int * (* company *)
      [ `FinancialReport
      | `TakeMoney of int
      | `GiveMoney of int 
      | `BuildTrack
      | `RepayBond ]
  ]

  (* For the "are you sure" menu *)
type confirm_msg = [
  | `None
  | `BuyStock of int
  ]

  (* These things prevent any other interaction *)
type 'state modal =
  | MsgBox of (unit, 'state) Menu.MsgBox.t
  | Confirm_menu of (confirm_msg, 'state) Menu.MsgBox.t

type 'state t = {
  menu: (msg, 'state) Menu.Global.t;
  modal: 'state modal option;
}
