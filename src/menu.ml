open Containers

let menu_font = 1

  type 'a action =
    | On of 'a
    | Off of 'a
    | Handled
    | NoAction

  let show_action = function
    | On _ -> "On"
    | Off _ -> "Off"
    | Handled -> "Handled"
    | NoAction -> "NoAction"

  let is_action = function
    | NoAction -> false
    | _ -> true

module MsgBox = struct

  type 'a fire =
    | Action of 'a
    | Checkbox of bool * 'a (* checked *)
    | MsgBox of bool * 'a t (* open *)

  and 'a entry = {
    y: int;
    h: int;
    name: string;
    fire: 'a fire;
    visibility: ((Backend.t * Mapview_d.t) -> bool) option;
    visible: bool;
  }

  and 'a t =
    { x: int; y: int;
      w: int; h: int;
      border_x: int; border_y: int;
      entries: 'a entry list;
      selected: int option;
      exclusive: (int * int list) option;
      font: Fonts.Font.t;
    }

  let get_entry_w_h font v =
    Fonts.Font.get_str_w_h font v.name

  let make_entry ?(visibility=None) name fire =
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
      visible=true;
      visibility;
    }

  let render_entry win font v ~x =
    if v.visible then
      Fonts.Font.write win font ~color:Ega.white v.name ~x ~y:v.y

  let make ?(exclusive=None) ~fonts ~x ~y entries =
    let exclusive = match exclusive with
      | None -> None
      | Some [] -> Some (0, [])
      | Some li -> Some (List.hd li, li)
    in
    {
      border_x=8; border_y=6;
      x; y; w=0; h=0;
      entries;
      selected=None;
      exclusive;
      font=fonts.(menu_font);
    }

    (* Compute menu size dynamically *)
  let do_open_menu s (v:'a t) =
    let (w, h), entries =
      List.fold_map (fun (max_w, max_h) entry ->
        let visible =
          match entry.visibility with
          | Some f -> f s
          | None -> true
        in
        let w, h = get_entry_w_h v.font entry in
        let y = v.y + max_h in
        let entry = {entry with y; h; visible} in
        let max_w, max_h =
          if visible then
            (max max_w w, max_h + h)
          else
            max_w, max_h
        in
        (max_w, max_h), entry)
      (v.border_x, v.border_y)
      v.entries
    in
    let w, h = w + v.border_x, h + v.border_y in
    {v with entries; w; h}

  let rec is_entry_clicked v ~x ~y ~recurse =
    if not v.visible then false else
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


  let render win v =
    (* draw background *)
    Renderer.draw_rect win ~x:(v.x+1) ~y:(v.y+1) ~w:v.w ~h:v.h ~color:Ega.gray ~fill:true;
    Renderer.draw_rect win ~x:(v.x+1) ~y:(v.y+1) ~w:v.w ~h:v.h ~color:Ega.white ~fill:false;
    Renderer.draw_rect win ~x:(v.x) ~y:(v.y) ~w:(v.w+2) ~h:(v.h+2) ~color:Ega.black ~fill:false;
    List.iter (render_entry win v.font ~x:(v.x + v.border_x)) v.entries

end

module Title = struct

  (* menu in the upper bar *)
  type 'a t = {
    x: int;
    y: int;
    w: int; (* for clicking only *)
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

  let render win ~fonts v =
    String.fold (fun (x, y, key) c ->
      if Char.Infix.(c = '&') then
        (x, y, true)
      else
        let color = if key then Ega.white else Ega.bcyan in
        let (x, y) = Fonts.Render.write_char win fonts c ~idx:menu_font ~x ~y ~color in
        (x, y, false))
    (v.x, v.y, false)
    v.name
    |> ignore

  let render_msgbox win v =
    MsgBox.render win v.msgbox

  let do_open_menu s v =
    let msgbox = MsgBox.do_open_menu s v.msgbox in
    {v with msgbox}

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

  let handle_click s v ~x ~y =
    (* Check for closed menu *)
    if is_not_clicked v ~x ~y && Option.is_none v.open_menu then (v, NoAction)
    else (
      (* Handle a top menu click first *)
      let clicked_top_menu = List.find_idx (Title.is_title_clicked ~x ~y) v.menus in
      match clicked_top_menu, v.open_menu with
      | Some (i, _), Some mopen when i = mopen ->
          (* close menu *)
          Printf.printf "1. i[%d]\n%!" i;
          {v with open_menu = None}, Handled
      | Some (i, _), _ ->
          Printf.printf "2. i[%d]\n%!" i;
          (* open menu *)
          let menus = Utils.List.modify_at_idx i (Title.do_open_menu s) v.menus in
          {v with menus; open_menu = Some i}, Handled
      | None, (Some open_menu as sopen) ->
          Printf.printf "3. open[%d]\n%!" open_menu;
          (* Open menu, check for other clicks *)
          let menus, action = 
            Utils.List.modify_make_at_idx open_menu (Title.handle_click ~x ~y) v.menus
          in
          let action = action |> Option.get_exn_or "error" in
          Printf.printf "%s\n%!" (show_action action);
          let open_menu =
            (* Close the menu if it's a random click *)
            match action with
            | NoAction -> None
            | _ -> sopen
          in
          ({v with menus; open_menu}, action)
      | None, None ->
          Printf.printf "4. NoAction\n%!";
          (* no menu open *)
          v, NoAction
    )

  let update s v (event:Event.t) =
    match event with
    | MouseButton {down=true; x; y; _} ->
        handle_click s v ~x ~y
    | _ -> v, NoAction

  let render win fonts v =
    (* Render menu titles *)
    List.iter (Title.render win ~fonts) v.menus;
    match v.open_menu with
    | None -> ()
    | Some i ->
        Title.render_msgbox win (List.nth v.menus i)

end




