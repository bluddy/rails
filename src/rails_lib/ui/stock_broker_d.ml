open! Containers

type msg = [
  | `SellBond
  | `RepayBond
  | `BuyStock of Owner.t (* player *)
  | `SellStock of Owner.t (* player *)
  | `Declare_bankruptcy
  | `OperateRR of Owner.t * (* company *)
      [ `FinancialReport
      | `TakeMoney of int
      | `GiveMoney of int 
      | `BuildTrack
      | `RepayBond ]
  ]

  (* For the "are you sure" menu *)
type confirm_msg = [
  | `None
  | `BuyStock of Owner.t
  | `Declare_bankruptcy
  ]

  (* These things prevent any other interaction *)
type 'state modal =
  | Normal
  | MsgBox of (unit, 'state) Menu.MsgBox.t
  | Confirm_menu of (confirm_msg, 'state) Menu.MsgBox.t
  | Newspaper of 'state Newspaper_d.t
  | RR_build of Rr_command.t

type 'state t = {
  menu: (msg, 'state) Menu.Global.t;
  modal: 'state modal;
}

