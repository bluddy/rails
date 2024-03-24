open! Containers

type msg = [
  | `SellBond
  | `RepayBond
  | `BuyStock of int (* player *)
  | `SellStock of int (* player *)
  (* TODO: operate RR menu *)
  ]

type 'state t = {
  menu: (msg, 'state) Menu.Global.t;
}