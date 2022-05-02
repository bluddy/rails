open Containers

let menu_font = 1

  type 'a action =
    | On of 'a
    | Off of 'a
    | Internal
    | NoAction

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
    fire: 'a fire
  }

  and 'a t =
    { x: int; y: int;
      w: int; h: int;
      entries: 'a entry list;
      selected: int option;
      exclusive: (int * int list) option;
      font: Fonts.Font.t;
    }

  let get_entry_w_h font v =
    Fonts.Font.get_str_w_h font v.name

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

  let render_entry win font v ~x =
    Fonts.Font.write win font ~color:Ega.white v.name ~x ~y:v.y

  let make ?(exclusive=None) ~fonts ~x ~y entries =
    let exclusive = match exclusive with
      | None -> None
      | Some [] -> Some (0, [])
      | Some li -> Some (List.hd li, li)
    in
    {
      x; y; w=0; h=0;
      entries;
      selected=None;
      exclusive;
      font=fonts.(menu_font);
    }

    (* Compute menu size dynamically *)
  let open_menu (v:'a t) =
    let (w, h), entries =
      List.fold_map (fun (max_h, max_w) entry ->
        let w, h = get_entry_w_h v.font entry in
        let max_w = max max_w w in
        let max_h = max_h + h in
        let entry = {entry with y=v.y+max_h; h} in
        (max_h, max_w), entry)
      (0, 0)
      v.entries
    in
    { v with entries; w; h}

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


  let render win v =
    Renderer.draw_rect win ~x:v.x ~y:v.y ~w:v.w ~h:v.h ~color:Ega.dgray ~fill:true;
    List.iter (render_entry win v.font ~x:v.x) v.entries

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

  let open_menu v =
    let msgbox = MsgBox.open_menu v.msgbox in
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

  let handle_click v ~x ~y =
    (* Check for closed menu *)
    if is_not_clicked v ~x ~y then (v, NoAction)
    else (
      (* Handle a top menu click first *)
      let clicked_top_menu = List.find_idx (Title.is_title_clicked ~x ~y) v.menus in
      match clicked_top_menu, v.open_menu with
      | Some (i, _), Some mopen when i = mopen ->
          Printf.printf "1. i[%d]\n%!" i;
          {v with open_menu = None}, Internal
      | Some (i, _), _ ->
          Printf.printf "2. i[%d]\n%!" i;
          let menus = Utils.List.modify_at_idx i Title.open_menu v.menus in
          {v with menus; open_menu = Some i}, Internal
      | None, None ->
          Printf.printf "3.\n%!";
          (* no menu open *)
          v, NoAction
      | None, Some open_menu ->
          Printf.printf "3. open[%d]\n%!" open_menu;
          (* Handle other clicks *)
          let menus, action = 
            Utils.List.modify_make_at_idx open_menu (Title.handle_click ~x ~y) v.menus
          in
          ({v with menus}, action |> Option.get_exn_or "error")
    )

  let update v (event:Event.t) =
    match event with
    | MouseButton {down=true; x; y; _} ->
        handle_click v ~x ~y
    | _ -> v, NoAction

  let render win fonts v =
    (* Render menu titles *)
    List.iter (Title.render win ~fonts) v.menus;
    match v.open_menu with
    | None -> ()
    | Some i ->
        Title.render_msgbox win (List.nth v.menus i)

end




