
type 'a addcars = {
  anim: Train_animate_side_d.t;
  menu: ([`AddCar of Goods.t | `Done], 'a) Engine.Menu.MsgBox.t;
  show_menu: bool;
  train_done: bool;
}
