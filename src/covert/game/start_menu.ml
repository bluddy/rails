open! Containers

open Utils.Infix

module Ega = Engine.Ega
module R = Engine.Renderer

type menu_action =
  [
    | `New_character
    | `Load_game
    | `Practice_skill
    | `Hall_of_fame
  ]

type gender = [ `Male | `Female ]

type t =
  | Start_menu of menu_action Menu.t
  | Gender_menu of gender Menu.t

let create_start_menu (srv:Services.t) =
  let open Menu in
  let open MsgBox in
  make_msgbox ~x:80 ~y:80 ~fonts:srv.fonts ~heading:"Do you want to..." 
  [
    make_entry "Create a New Character" @@ `Action(`New_character);
    make_entry "Load a Saved Game" @@ `Action(`Load_game);
    make_entry "Practice a skill" @@ `Action(`Practice_skill);
    make_entry "Review Hall of Fame" @@ `Action(`Hall_of_fame);
  ]
  |> Menu.do_open_menu ~selected:(Some 0)

let create_gender_menu (srv:Services.t) =
  let open Menu in
  let open MsgBox in
  make_msgbox ~draw_bg:false ~x:96 ~y:33 ~fonts:srv.fonts ~heading:"Select one..."
  [
    make_entry "Maximillian Remington" @@ `Action(`Male);
    make_entry "Maxine Remington" @@ `Action(`Female);
  ]
  |> Menu.do_open_menu ~selected:(Some 0)


let create srv = Start_menu(create_start_menu srv)

let handle_event srv event time v =
  match v with
  | Start_menu menu ->
    let menu2, _status = Menu.modal_handle_event menu event time in
    if menu2 === menu then v, `Stay
    else Start_menu menu2, `Stay
  | Gender_menu menu ->
    let menu2, status = Menu.modal_handle_event menu event time in
    begin match status with
    | `Stay when menu2 === menu -> v, `Stay
    | `Stay -> Gender_menu menu2, `Stay
    | `Exit -> Start_menu(create_start_menu srv), `Stay
    end

let handle_tick srv time v =
  match v with
  | Start_menu menu ->
    let menu2, status = Menu.modal_handle_tick menu time in
    begin match status with
    | `Stay when menu2 === menu -> v, `Stay
    | `Stay -> Start_menu menu2, `Stay
    | `Activate `New_character -> Gender_menu(create_gender_menu srv), `Stay
    | _ -> v, `Stay
    end
  | Gender_menu menu ->
    let menu2, status = Menu.modal_handle_tick menu time in
    begin match status with
    | `Stay when menu2 === menu -> v, `Stay
    | `Stay -> Gender_menu menu2, `Stay
    | _ -> v, `Stay
    end

let render (srv:Services.t) v = match v with
  | Start_menu menu ->
      R.draw_rect srv.win ~x:0 ~y:0 ~w:320 ~h:200 ~color:Ega.blue ~fill:true;
      Menu.render srv.win menu
  | Gender_menu menu ->
      R.clear_screen srv.win;
      let tex = Hashtbl.find srv.textures.images `Gender in
      R.Texture.render ~x:0 ~y:0 srv.win tex;
      Menu.render srv.win menu


