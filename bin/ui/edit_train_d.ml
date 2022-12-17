
type msg =
  [ `Dummy
  ]

type 'state t = {
  index: int;
  menu: (msg, 'state) Menu.Global.t;
}

