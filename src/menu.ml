open Containers

let menu_font = 1

module MsgBox = struct

  type 'a action =
    | On of 'a
    | Off of 'a
    | NoAction

  type 'a entry = {
    y: int;
    h: int;
    name: string;
    fire: [ `Action of 'a
          | `Checkbox of bool * 'a (* checked *)
          | `MsgBox of bool * 'a t (* open *)
          ]
  }

  and 'a exc_entry = {
    y: int;
    h: int;
    name: string;
    check: bool;
    action: 'a;
  }

  and 'a t =
    | ExclusiveBox of
      { x: int; y: int; w: int; h: int;
        entries: 'a exc_entry list;
        selected: int;
      }
    | MsgBox of
      { x: int; y: int; w: int; h: int;
        entries: 'a entry list;
        selected: int option;
      }

  let get_entry_w_h fonts v =
    Fonts.get_str_w_h ~fonts ~idx:menu_font v.name

  let make_entry ?(check=None) ~fonts name fire =
    let _, h = Fonts.get_str_w_h ~fonts ~idx:menu_font name in
    {
      y=0;
      h;
      check;
      name;
      fire;
    }

  let is_entry_clicked v ~x ~y =
    (* We already checked the >= condition *)
    let entry_click = y < v.y + v.h in
    match v.fire with
    | 

  let render_entry win fonts v ~x =
    Fonts.Render.write win fonts ~color:Ega.white ~idx:menu_font v.name ~x ~y:v.y

  let make ~fonts ~x ~y ~exclusive entries =
    let w, h =
      List.fold_left (fun (max_h, max_w) entry ->
        let w, h = get_entry_w_h fonts entry in
        let max_w = max max_w w in
        let max_h = max_h + h in
        (max_h, max_w))
      (0, 0)
      entries
    in
    {x; y; w; h; entries; selected=None; exclusive}

  let is_clicked v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

  let rec handle_entry_click v ~x ~y =
    match v.fire with
    | `MsgBox msgbox ->
        let msgbox, action = handle_click msgbox ~x ~y in
        {v with fire=`MsgBox msgbox}, action
    | `Action action ->
      let check, action =
        match v.check with
        | None -> None, On(action)
        | Some true -> Some false, Off(action)
        | Some false -> Some true, On(action)
      in
      {v with check}, action

  and handle_click v ~x ~y =
    let idx = List.find_idx (is_entry_clicked ~x ~y) v.entries in
    let entries, action =
      match idx with
      | Some (idx, _) ->
          let a, b =
            Utils.List.modify_make_at_idx idx (handle_entry_click ~x ~y) v.entries
          in
          a, b |> Option.get_exn_or "bad state"
      | _ ->
          failwith "Should never get here"
    in
    {v with entries}, action

  let render win fonts menu =
    Renderer.draw_rect win ~x:menu.x ~y:menu.y ~w:menu.w ~h:menu.h ~color:Ega.dgray ~fill:true;
    List.iter (render_entry win fonts ~x:menu.x) menu.entries

end

module Title = struct

  (* menu in the upper bar *)
  type 'a t = {
    x: int;
    y: int;
    w: int;
    h: int;
    name: string;
    msgbox: 'a MsgBox.t;
  }

  let make ~fonts ~name ~x ~y msgbox =
    let w, h = Fonts.get_str_w_h ~fonts ~idx:1 name in
    {
      x; y;
      w; h;
      name;
      msgbox;
    }

  let is_title_clicked v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

  let handle_click v ~x ~y =
    if MsgBox.is_clicked v.msgbox ~x ~y then
      let msgbox, event = MsgBox.handle_click v.msgbox ~x ~y in
      {v with msgbox}, event
    else
      v, None

end

module Global = struct

  type 'a t = {
    menu_h: int;
    open_menu: int option;
    menus: 'a Title.t list;
  }

  let make ~menu_h menus = {
    menu_h;
    open_menu=None;
    menus;
  }

  let is_not_clicked v ~x ~y =
    let _x = x in
    y > v.menu_h && Option.is_none v.open_menu

  let handle_click v ~x ~y =
    (* Check for closed menu *)
    if is_not_clicked v ~x ~y then (v, None) else

    (* Handle a top menu click first *)
    let clicked_top_menu = List.find_idx (Title.is_title_clicked ~x ~y) v.menus in
    match clicked_top_menu, v.open_menu with
    | Some (i, _), Some mopen when i = mopen ->
        {v with open_menu = None}, None
    | Some (i, _), _ ->
        {v with open_menu = Some i}, None
    | None, None ->
        (* no menu open *)
        v, None
    | None, Some open_menu ->
        (* Handle other clicks *)
        let menus, event = 
          Utils.List.modify_make_at_idx open_menu (fun menu ->
            Title.handle_click menu ~x ~y)
            v.menus
        in
        {v with menus}, event

  let update v (event:Event.t) =
    match event with
    | MouseButton {down=true; x; y; _} ->
        handle_click v ~x ~y
    | _ -> v, None

  let render win fonts v =
    (* Render menu titles *)
    List.iter (fun menu ->
      String.fold (fun (x, y, key) c ->
        if Char.Infix.(c = '&') then
          (x, y, true)
        else
          let color = if key then Ega.white else Ega.bcyan in
          let (x, y) = Fonts.Render.write_char win fonts c ~idx:menu_font ~x ~y ~color in
          (x, y, false))
      (menu.Title.x, menu.y, false)
      menu.name
      |> ignore
    )
    v.menus

end




