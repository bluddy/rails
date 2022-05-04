open Containers

let menu_font = 1

  type 'a action =
    | On of 'a
    | Off of 'a
    | Handled
    | OpenMsgBox
    | CloseMsgBox
    | ClickInMsgBox (* a click but no action *)
    | OpenMenu
    | CloseMenu
    | NoAction

  let show_action = function
    | On _ -> "On"
    | Off _ -> "Off"
    | Handled -> "Handled"
    | OpenMsgBox -> "OpenMsgBox"
    | CloseMsgBox -> "CloseMsgBox"
    | ClickInMsgBox -> "ClickInMsgBox"
    | OpenMenu -> "OpenMenu"
    | CloseMenu -> "CloseMenu"
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
    let name = name in
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
        let y = v.y + max_h in
        let w, h = get_entry_w_h v.font entry in
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
    let w = w + 2 * v.border_x in
    {v with entries; w; h}

  let is_entry_clicked_shallow v ~y =
    if not v.visible then false else
    y < v.y + v.h

  let find_clicked_entry_shallow v ~y =
    List.find_idx (is_entry_clicked_shallow ~y) v.entries

  let is_clicked_shallow v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

    (* Do not recurse deeply *)
  let handle_entry_click_shallow s (v:'a entry) =
    (* Assume we were clicked. Only handle shallow events *)
    match v.fire with
    | MsgBox(false, box) ->
        let box = do_open_menu s box in
        {v with fire=MsgBox(true, box)}, OpenMsgBox
    | MsgBox(true, box) ->
        {v with fire=MsgBox(false, box)}, CloseMsgBox
    | Action action ->
        v, On(action)
    | Checkbox(false, action) ->
        {v with fire=Checkbox(true, action)}, On(action)
    | Checkbox(true, action) ->
        {v with fire=Checkbox(false, action)}, Off(action)

  let entry_close_msgbox v =
    match v.fire with
    | MsgBox(true, box) ->
        {v with fire=MsgBox(false, box)}
    | _ -> v

    (* Only search depth-first *)
  let rec handle_entry_click_deep s (v:'a entry) ~x ~y =
    match v.fire with
    | MsgBox(true, box) ->
        (* open msgbox -> recurse *)
        let box, action = handle_click s box ~x ~y in
        (* TODO: handle different action cases, like if an action was chosen *)
        {v with fire=MsgBox(true, box)}, action
    | _ ->
        v, NoAction

  and handle_click s (v:'a t) ~x ~y =
    let entries, action =
      match v.selected with
      | Some idx ->
          (* deep search first *)
          let a, b =
            Utils.List.modify_make_at_idx idx (handle_entry_click_deep s ~x ~y) v.entries
          in
          a, b |> Option.get_exn_or "error"
      | None ->
          (* Nothing selected, we're done *)
          v.entries, NoAction
    in
    let entries, action, selected =
      match action with
      | NoAction when is_clicked_shallow v ~x ~y ->
          (* Didn't find in deep search, do shallow search in this msgbox *)
          begin match find_clicked_entry_shallow v ~y, v.selected with
          | None, None ->
              (* clicked in msgbox but not an option *)
              entries, ClickInMsgBox, v.selected
          | None, Some entry_idx ->
              (* clear selection *)
              let entries =
                Utils.List.modify_at_idx entry_idx (entry_close_msgbox) v.entries
              in
              entries, action, None
          | Some (entry_idx, _), _ ->
              (* clicked an entry, handle and switch selection *)
            let entries, action =
              Utils.List.modify_make_at_idx entry_idx (handle_entry_click_shallow s) v.entries
            in
            entries, (action |> Option.get_exn_or "bad state"), Some entry_idx
          end

      | _ ->
          (* Got an action, pass it on *)
          entries, action, v.selected
    in
    {v with entries; selected}, action


    let render_entry win font v ~selected ~x ~w ~border_x =
      if v.visible then (
        if selected then
          Renderer.draw_rect win ~x:(x+3) ~y:(v.y-2) ~w:(w-3) ~h:v.h ~fill:true ~color:Ega.bcyan;

        let prefix =
          match v.fire with
          | Checkbox(true, _) -> "^"
          | _ -> " "
        in
        Fonts.Font.write win font ~color:Ega.black (prefix^v.name) ~x:(x+border_x) ~y:v.y;
      )

    let rec render win v =
      (* draw background *)
      let x = v.x in
      Renderer.draw_rect win ~x:(x+1) ~y:(v.y+1) ~w:v.w ~h:v.h ~color:Ega.gray ~fill:true;
      Renderer.draw_rect win ~x:(x+1) ~y:(v.y+1) ~w:v.w ~h:v.h ~color:Ega.white ~fill:false;
      Renderer.draw_rect win ~x:(x) ~y:(v.y) ~w:(v.w+2) ~h:(v.h+2) ~color:Ega.black ~fill:false;
      let selected = Option.get_or v.selected ~default:(-1) in
      List.iteri (fun i entry ->
        render_entry win v.font ~selected:(i=selected) ~x:v.x ~border_x:v.border_x ~w:v.w entry)
        v.entries;
      match v.selected with
      | Some selected ->
          let entry = List.nth v.entries selected in
          begin match entry.fire with
          | MsgBox(true, box) -> render win box
          | _ -> ()
          end
      | _ -> ();

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

  let handle_click s v ~x ~y =
    let msgbox, event = MsgBox.handle_click s v.msgbox ~x ~y in
    {v with msgbox}, event

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
          (* clicked top menu, same menu is open *)
          Printf.printf "1. i[%d]\n%!" i;
          {v with open_menu = None}, CloseMenu
      | Some (i, _), _ ->
          Printf.printf "2. i[%d]\n%!" i;
          (* clicked top menu, some other or none are open *)
          let menus = Utils.List.modify_at_idx i (Title.do_open_menu s) v.menus in
          {v with menus; open_menu = Some i}, OpenMenu
      | None, (Some open_menu as sopen) ->
          Printf.printf "3. open[%d]\n%!" open_menu;
          (* clicked elsewhere with open top menu *)
          let menus, action = 
            (* check menu itself *)
            Utils.List.modify_make_at_idx open_menu (Title.handle_click s ~x ~y) v.menus
          in
          let action = action |> Option.get_exn_or "error" in
          Printf.printf "%s\n%!" (show_action action);
          (* Close the menu if it's a random click *)
          let open_menu, action =
            match action with
            | NoAction -> None, CloseMenu
            | _ -> sopen, action
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




