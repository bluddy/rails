
type slot = {
  header: string option;
  slot: int;
}

type 'state t = {
  menu: (slot, 'state) Menu.MsgBox.t;
  action: [`Save | `Load];
}
