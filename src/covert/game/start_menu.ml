open! Containers

open Utils.Infix

module Ega = Engine.Ega
module R = Engine.Renderer
module Text_entry = Engine.Text_entry

type menu_action =
  [
    | `New_character
    | `Load_game
    | `Practice_skill
    | `Hall_of_fame
  ]

type info = {
  gender: Gender.t;
  name: string;
  difficulty: Difficulty.t;
  training: Training.map;
}

let default_info gender name difficulty = {
  gender;
  name;
  difficulty;
  training=Training.Map.default;
}

type t =
  | Start_menu of menu_action Menu.t
  | Char_menu of {
      gender_menu: Gender.t Menu.t;
      codename: (Text_entry.t * Gender.t) option;
      diff_menu: (Difficulty.t Menu.t * string) option;
    }
  | Training of {
      info: info;
      menu: Training.field Menu.t;
    }

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

let difficulty_menu (srv:Services.t) =
  let open Menu in
  let open MsgBox in
  let entries = List.map (fun diff ->
    make_entry (Difficulty.show diff) @@ `Action diff)
    Difficulty.list
  in
  entries
  |> make_msgbox ~draw_bg:false ~x:96 ~y:129 ~fonts:srv.fonts ~heading:"Which difficulty level?"
  |> Menu.do_open_menu ~selected:(Some 0)

let training_menu (srv:Services.t) =
  let open Menu in
  let open MsgBox in
  let entries = List.map (fun field ->
    make_entry (Training.show_field field ^ " training") @@ `Action field)
    Training.field_list
  in
  entries
  |> make_msgbox ~draw_bg:false ~x:94 ~y:28 ~fonts:srv.fonts
  |> Menu.do_open_menu ~selected:(Some 0)

let make_codename_entry () =
  Text_entry.make ~font_idx:2 "" ~x:106 ~y:100 ~chars:15 ~text_color:Ega.bgreen
    ~cursor_flash:100 ~cursor_color:Ega.green ~cursor_height:0 ~frame_color:None

let render_codename_box win =
  R.draw_rect2 win ~x:99 ~y:85 ~x2:221 ~y2:114 ~color:Ega.dgray ~fill:true;
  R.draw_rect2 win ~x:100 ~y:87 ~x2:220 ~y2:113 ~color:Ega.black ~fill:false;
  (* R.draw_rect2 win ~x:101 ~y:88 ~x2:219 ~y2:112 ~color:Ega.dgray ~fill:true; *)
  R.draw_rect2 win ~x:105 ~y:101 ~x2:215 ~y2:110 ~color:Ega.black ~fill:true;
  R.draw_line win ~x1:101 ~y1:86 ~x2:219 ~y2:86 ~color:Ega.gray;
  R.draw_line win ~x1:103 ~y1:99 ~x2:217 ~y2:99 ~color:Ega.black;
  R.draw_line win ~x1:102 ~y1:112 ~x2:218 ~y2:112 ~color:Ega.black;
  R.draw_line win ~x1:104 ~y1:101 ~x2:104 ~y2:110 ~color:Ega.gray;
  R.draw_line win ~x1:216 ~y1:101 ~x2:216 ~y2:110 ~color:Ega.gray

let create srv = Start_menu(create_start_menu srv)

let handle_event srv event time v =
  match v with
  | Start_menu menu ->
    let menu2, _status = Menu.modal_handle_event menu event time in
    if menu2 === menu then v, `Stay
    else Start_menu menu2, `Stay
  | Char_menu ({diff_menu=Some (menu, codename); _} as s) ->
    let menu2, status = Menu.modal_handle_event menu event time in
    begin match status with
    | `Stay when menu2 === menu -> v, `Stay
    | `Stay -> Char_menu {s with diff_menu=Some (menu2, codename)}, `Stay
    | `Exit -> Char_menu {s with diff_menu=None}, `Stay
    end
  | Char_menu ({codename=Some (entry, gender);_} as s) ->
    let entry2, status = Text_entry.handle_event entry event in
    begin match status with
    | `Stay when entry2 === entry -> v, `Stay
    | `Stay -> Char_menu{s with codename=Some (entry2, gender)}, `Stay
    | `Exit -> Char_menu{s with codename=None}, `Stay
    | `Return text -> Char_menu{s with diff_menu=(difficulty_menu srv, text) |> Option.some}, `Stay
    end
  | Char_menu s ->
    let menu2, status = Menu.modal_handle_event s.gender_menu event time in
    begin match status with
    | `Stay when menu2 === s.gender_menu -> v, `Stay
    | `Stay -> Char_menu {s with gender_menu=menu2}, `Stay
    | `Exit -> Start_menu(create_start_menu srv), `Stay
    end
  | Training s ->
    let menu2, status = Menu.modal_handle_event s.menu event time in
    begin match status with
    | `Stay when menu2 === s.menu -> v, `Stay
    | `Stay -> Training {s with menu=menu2}, `Stay
    (* Early exit *)
    | `Exit -> v, `Stay
    end

let handle_tick srv time v =
  match v with
  | Start_menu menu ->
    let menu2, status = Menu.modal_handle_tick menu time in
    begin match status with
    | `Stay when menu2 === menu -> v, `Stay
    | `Stay -> Start_menu menu2, `Stay
    | `Activate `New_character ->
        Char_menu({
          gender_menu=create_gender_menu srv;
          codename=None;
          diff_menu=None}), `Stay
    | _ -> v, `Stay
    end
  | Char_menu ({diff_menu=Some (menu, name); codename=Some(_,gender);_} as s) ->
    let menu2, status = Menu.modal_handle_tick menu time in
    begin match status with
    | `Stay when menu2 === menu -> v, `Stay
    | `Stay -> Char_menu {s with diff_menu=Some (menu2, name)}, `Stay
    | `Activate difficulty ->
        let info = default_info gender name difficulty in
        Training{info; menu=training_menu srv}, `Stay
    end
  | Char_menu ({codename=Some (entry, gender);_} as s) ->
    let entry2, status = Text_entry.handle_tick time entry in
    begin match status with
    | `Stay when entry2 === entry -> v, `Stay
    | `Stay -> Char_menu {s with codename=Some (entry2, gender)}, `Stay
    end
  | Char_menu s ->
    let menu2, status = Menu.modal_handle_tick s.gender_menu time in
    begin match status with
    | `Stay when menu2 === s.gender_menu -> v, `Stay
    | `Stay -> Char_menu {s with gender_menu=menu2}, `Stay
    | `Activate gender -> Char_menu {s with codename=(make_codename_entry (), gender) |> Option.some}, `Stay
    end
  | Training s ->
    let menu2, status = Menu.modal_handle_tick s.menu time in
    begin match status with
    | `Stay when menu2 === s.menu -> v, `Stay
    | `Stay -> Training {s with menu=menu2}, `Stay
    | `Activate field ->
        let info = {s.info with training=Training.Map.incr field s.info.training} in
        Training {s with info}, `Stay
    end


let render (srv:Services.t) v = match v with
  | Start_menu menu ->
      R.draw_rect srv.win ~x:0 ~y:0 ~w:320 ~h:200 ~color:Ega.blue ~fill:true;
      Menu.render srv.win menu
  | Char_menu s ->
      R.clear_screen srv.win;
      let tex = Hashtbl.find srv.textures.images `Gender in
      R.Texture.render ~x:0 ~y:0 srv.win tex;
      Menu.render srv.win s.gender_menu;
      Option.iter (fun (entry, _) ->
        render_codename_box srv.win;
        Fonts.Render.write srv.win srv.fonts ~color:Ega.green ~idx:`Large "Max's code name is:" ~x:104 ~y:90;
        Text_entry.render srv.win srv.fonts entry)
        s.codename;
      Option.iter (fun (menu, _) ->
        Menu.render srv.win menu
      ) s.diff_menu
  | Training s ->
      R.clear_screen srv.win;
      let tex = Hashtbl.find srv.textures.images `Training in
      R.Texture.render ~x:0 ~y:0 srv.win tex;
      Menu.render srv.win s.menu;
      let y, x = 188, 19 in
      List.fold_left (fun x field ->
        let level = Training.Map.find field s.info.training
          |> Training.Level.show
        in
        Fonts.Render.write srv.win srv.fonts ~x ~y ~color:Ega.white ~idx:`Large level;
        x + 80
      ) x Training.field_list
      |> ignore


