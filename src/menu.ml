open Containers

let menu_font = 1

module MsgBox = struct

  type 'a action =
    | On of 'a
    | Off of 'a
    | NoAction

  let no_action = function
    | NoAction -> true
    | _ -> false

  type 'a fire =
    | Action of 'a
    | Checkbox of bool * 'a (* checked *)
    | MsgBox of bool * 'a t (* open *)

  and 'a entry = {
    y: int;
    h: int;
    name: string;
    fire: 'a fire
  }

  and 'a t =
    { x: int; y: int;
      w: int; h: int;
      entries: 'a entry list;
      selected: int option;
      exclusive: (int * int list) option;
    }

  let get_entry_w_h fonts v =
    Fonts.get_str_w_h ~fonts ~idx:menu_font v.name

  let make_entry name fire =
    let fire =
      match fire with
      | `MsgBox m -> MsgBox(false, m)
      | `Action a -> Action a
      | `Checkbox b -> Checkbox(false, b)
    in
    {
      y=0;
      h=0;
      name;
      fire;
    }

  let render_entry win fonts v ~x =
    Fonts.Render.write win fonts ~color:Ega.white ~idx:menu_font v.name ~x ~y:v.y

  let make ?(exclusive=None) ~fonts ~x ~y entries =
    let (w, h), entries =
      List.fold_map (fun (max_h, max_w) entry ->
        let w, h = get_entry_w_h fonts entry in
        let max_w = max max_w w in
        let max_h = max_h + h in
        let entry = {entry with y=y+max_h; h} in
        (max_h, max_w), entry)
      (0, 0)
      entries
    in
    let exclusive = match exclusive with
      | None -> None
      | Some [] -> Some (0, [])
      | Some li -> Some (List.hd li, li)
    in
    {
      x; y; w; h;
      entries;
      selected=None;
      exclusive;
    }

  let rec is_entry_clicked v ~x ~y ~recurse =
    (* We already checked the >= condition *)
    let self_click = y < v.y + v.h in
    let deep =
      match v.fire with
      | MsgBox(true, mbox) when recurse ->
          is_clicked mbox ~x ~y ~recurse
      | _ ->
          false
    in
    deep || self_click

  and is_clicked v ~x ~y ~recurse =
    let self_click = x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h in
    let deep =
      (* Only recurse for msgboxes, not for exclusive boxes *)
      if Option.is_some v.exclusive then false
      else
        match v.selected with
        | Some i when recurse ->
            is_entry_clicked (List.nth v.entries i) ~x ~y ~recurse
        | _ -> 
            false
    in
    deep || self_click

  let find_clicked_entry_shallow v ~x ~y =
    List.find_idx (is_entry_clicked ~x ~y ~recurse:false) v.entries

  let rec handle_entry_click (v:'a entry) ~x ~y =
    match v.fire with
    | MsgBox (visible, box) ->
        let box, action = handle_click box ~x ~y in
        {v with fire=MsgBox(visible, box)}, action
    | Action action ->
        v, On(action)
    | Checkbox(false, action) ->
        {v with fire=Checkbox(true, action)}, On(action)
    | Checkbox(true, action) ->
        {v with fire=Checkbox(false, action)}, Off(action)

  and handle_click (v:'a t) ~x ~y =
    (* We assume something has been clicked *)
    let entries, action =
      match v.selected with
      | Some idx ->
          (* deep search first *)
          Utils.List.modify_make_at_idx idx (handle_entry_click ~x ~y) v.entries
      | None ->
          v.entries, None
    in
    let entries, action =
      if Option.is_none action then
        (* Didn't find in deep search *)
        let idx, _ = find_clicked_entry_shallow v ~x ~y |> Option.get_exn_or "error" in
        let entries, action =
          Utils.List.modify_make_at_idx idx (handle_entry_click ~x ~y) v.entries
        in
        entries, (action |> Option.get_exn_or "bad state")

      else
        entries, NoAction
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

  let make ~fonts ~x ~y name msgbox =
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
    if MsgBox.is_clicked v.msgbox ~x ~y ~recurse:true then
      let msgbox, event = MsgBox.handle_click v.msgbox ~x ~y in
      {v with msgbox}, event
    else
      v, NoAction

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




