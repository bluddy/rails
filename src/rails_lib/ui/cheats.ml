open! Containers
open Cheat_d

let make_menu fonts =
  let open Menu in
  let open MsgBox in
  let money_menu =
    make ~fonts
    [
        make_entry "Add 500" @@ `Action(`Cheat(Add500Cash));
    ]
  in
  make ~fonts ~x:250 ~y:8
  [
    make_entry "Money" @@ `MsgBox money_menu;
    make_entry "Priority Shipment" @@ `Action(`Cheat(CreatePriorityShipment));
  ]
  
