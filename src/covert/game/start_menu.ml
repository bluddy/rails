open! Containers

open Utils.Infix

module Ega = Engine.Ega

type menu_action =
  [
    | `New_character
    | `Load_game
    | `Practice_skill
    | `Hall_of_fame
  ]

type t =
  | StartMenu of menu_action Menu.t

let create_start_menu (srv:Services.t) =
  let open Menu in
  let open MsgBox in
  make_msgbox ~x:80 ~y:80 ~fonts:srv.fonts ~select_color:Ega.yellow ~heading:"Do you want to..." 
  [
    make_entry "Create a New Character" @@ `Action(`New_character);
    make_entry "Load a Saved Game" @@ `Action(`Load_game);
    make_entry "Practice a skill" @@ `Action(`Practice_skill);
    make_entry "Review Hall of Fame" @@ `Action(`Hall_of_fame);
  ]
  |> Menu.do_open_menu ~selected:(Some 0)

let create srv = StartMenu(create_start_menu srv)

let handle_event _srv event time v =
  match v with
  | StartMenu menu ->
    let menu2, _status = Menu.modal_handle_event menu event time in
    if menu2 === menu then v, `Stay
    else StartMenu menu2, `Stay

let handle_tick _srv time v =
  match v with
  | StartMenu menu ->
    let menu2, msg = Menu.modal_handle_tick menu time in
    if menu2 === menu then v, msg
    else StartMenu menu2, msg

let render win v = match v with
  | StartMenu menu -> Menu.render win menu

