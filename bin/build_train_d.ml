
type addcars = {
  anim: Train_animate_side_d.t;
  menu: ([`AddCar of Goods.t | `Done], unit) Menu.MsgBox.t;
  show_menu: bool;
}
