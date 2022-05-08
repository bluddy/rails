open Containers

(* Menu is specialized to 2 type parameters:
  1. The type of the messages it sends
  2. The type of the data it reads into its checkbox and visibility lambdas
*)
let menu_font = 1
let max_width = 320

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

    (* Get the active char for the menu item *)
let get_active_char str =
  String.fold (fun (amp, letter) c ->
    match c with
    | '&' -> (true, letter)
    | c when amp -> (false, Some c)
    | _ -> (amp, letter)
  )
  (false, None)
  str
  |> snd

module CharMap = Utils.CharMap

module MsgBox = struct

  type ('a, 'b) fire =
    | Action of 'a
    | Checkbox of 'a * ('b -> bool)
    | MsgBox of bool * ('a, 'b) t (* open *)

  and ('a, 'b) entry = {
    y: int;
    h: int;
    name: string;
    fire: ('a, 'b) fire;
    test_enabled: ('b -> bool) option;
    enabled: bool;
  }

  and ('a, 'b) t =
    { 
      x: int; y: int;
      w: int; h: int;
      border_x: int; border_y: int;
      heading: string option;
      entries: ('a, 'b) entry list;
      selected: int option;
      font: Fonts.Font.t;
      indexes: int CharMap.t;
    }

  let get_entry_w_h font v =
    Fonts.Font.get_str_w_h font @@ " "^v.name

  let make_entry ?(test_enabled=None) name fire =
    let name = name in
    let fire =
      match fire with
      | `MsgBox m -> MsgBox(false, m)
      | `Action a -> Action a
      | `Checkbox (b, fn) -> Checkbox(b, fn)
    in
    {
      y=0;
      h=0;
      name;
      fire;
      enabled=true;
      test_enabled;
    }

  let get_width s v =
    List.fold_left (fun max_w entry ->
      let w, _ = get_entry_w_h v.font entry in
      max max_w w)
    0
    v.entries

    (* Compute menu size dynamically *)
  let do_open_menu ?(x=0) ?(y=0) s (v: ('a, 'b) t) =
    (* start calculating internal y *)
    let max_h = v.border_y in
    (* entry coordinates are internal to the msgbox *)
    let max_w, max_h = match v.heading with
      | Some str ->
          let w, h = Fonts.Font.get_str_w_h v.font str in
          w, h + max_h
      | None ->
          0, max_h
    in
    let (w, h), entries =
      List.fold_map (fun (max_w, max_h) entry ->
        let enabled =
          match entry.test_enabled with
          | Some f -> f s
          | None -> true
        in
        let w, h = get_entry_w_h v.font entry in
        let entry = {entry with y=max_h; h; enabled} in
        let max_w, max_h = max max_w w, max_h + h in
        (max_w, max_h), entry)
      (max_w, max_h)
      v.entries
    in
    (* w never included the border_x *)
    let w, h = w + v.border_x, h in
    let x, y =
      (* check for offset from previous menu *)
      if x <> 0 || y <> 0 then
        let offset_y = 15 in
        let offset_x = 30 in
        let x =
          if x + w + offset_x > max_width then
            max_width - w - offset_x
          else x + offset_x
        in
        x, y + offset_y
      else
        v.x, v.y
    in
    {v with entries; w; h; x; y}

  let make ?heading ?(x=0) ?(y=0) ~fonts entries =
    let font=fonts.(menu_font) in
    let indexes =
      List.foldi (fun acc i entry ->
        match get_active_char entry.name with
        | Some c -> CharMap.add c i acc
        | None -> CharMap.add entry.name.[0] i acc)
      CharMap.empty
      entries
    in
    {
      border_x=8; border_y=6;
      x; y; w=0; h=0;
      entries;
      selected=None;
      font;
      heading;
      indexes;
    }

  let is_entry_clicked_shallow v ~y =
    if not v.enabled then false else
    y < v.y + v.h

  let find_clicked_entry_shallow v ~y =
    List.find_idx (is_entry_clicked_shallow ~y) v.entries

  let is_clicked_shallow v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

    (* Do not recurse deeply *)
  let handle_entry_click_shallow s ~x v =
    (* Assume we were clicked. Only handle shallow events *)
    match v.fire with
    | MsgBox(false, box) ->
        let box = do_open_menu s ~x ~y:(v.y) box in
        {v with fire=MsgBox(true, box)}, OpenMsgBox
    | MsgBox(true, box) ->
        {v with fire=MsgBox(false, box)}, CloseMsgBox
    | Action action ->
        v, On(action)
    | Checkbox(action, fn) when fn s ->
        v, On(action)
    | Checkbox(action, _) ->
        v, Off(action)

  let rec close_entry v =
    match v.fire with
    | MsgBox(true, box) ->
        let box = close box in
        {v with fire=MsgBox(false, box)}
    | _ -> v

  and close v =
    match v.selected with
    | Some i ->
        let entries = Utils.List.modify_at_idx i close_entry v.entries in
        {v with entries; selected=None}
    | None -> v

    (* Only search depth-first *)
  let rec handle_entry_click_deep s (v:('a, 'b) entry) ~x ~y =
    match v.fire with
    | MsgBox(true, box) ->
        (* open msgbox -> recurse *)
        let box, action = handle_click s box ~x ~y in
        (* TODO: handle different action cases, like if an action was chosen *)
        {v with fire=MsgBox(true, box)}, action
    | _ ->
        v, NoAction

  and handle_click s (v:('a, 'b) t) ~x ~y =
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
          begin match find_clicked_entry_shallow v ~y:(y-v.y), v.selected with
          | None, None ->
              (* clicked in msgbox but not an option *)
              entries, ClickInMsgBox, v.selected
          | None, Some entry_idx ->
              (* clear selection *)
              let entries =
                Utils.List.modify_at_idx entry_idx (close_entry) v.entries
              in
              entries, action, None
          | Some (entry_idx, _), _ ->
              (* clicked an entry, handle and switch selection *)
            let entries, action =
              Utils.List.modify_make_at_idx entry_idx (handle_entry_click_shallow ~x:v.x s) v.entries
            in
            entries, (action |> Option.get_exn_or "bad state"), Some entry_idx
          end

      | _ ->
          (* Got an action, pass it on *)
          entries, action, v.selected
    in
    {v with entries; selected}, action

    let handle_key s v key = v, NoAction

    let update s v (event:Event.t) =
      match event with
      | MouseButton {down=true; x; y; _} ->
          handle_click s v ~x ~y
      | Key {down=true; key } ->
          handle_key s v key (* TODO *)
      | _ -> v, NoAction

    let render_entry win s font v ~selected ~x ~border_x ~y ~w =
      if selected then
        Renderer.draw_rect win ~x:(x+3) ~y:(v.y + y - 1) ~w:(w-4) ~h:(v.h-1) ~fill:true ~color:Ega.bcyan;

      let prefix =
        match v.fire with
        | Checkbox(_, fn) when fn s -> "^"
        | _ -> " "
      in
      let color = if v.enabled then Ega.black else Ega.dgray in
      Fonts.Font.write win font ~color (prefix^v.name) ~x:(x+border_x) ~y:(y + v.y)

    let rec render win s v =
      (* draw background *)
      let x = v.x in
      Renderer.draw_rect win ~x:(x+1) ~y:(v.y+1) ~w:v.w ~h:v.h ~color:Ega.gray ~fill:true;
      Renderer.draw_rect win ~x:(x+1) ~y:(v.y+1) ~w:v.w ~h:v.h ~color:Ega.white ~fill:false;
      Renderer.draw_rect win ~x:x ~y:v.y ~w:(v.w+2) ~h:(v.h+2) ~color:Ega.black ~fill:false;

      (* draw heading *)
      begin match v.heading with
      | Some str ->
          Fonts.Font.write win v.font ~color:Ega.white str ~x:(v.x + v.border_x) ~y:(v.y + v.border_y)
      | None -> ()
      end;

      (* draw entries and selection *)
      let selected = Option.get_or v.selected ~default:(-1) in
      List.iteri (fun i entry ->
        render_entry win s v.font ~selected:(i=selected) ~x:v.x ~border_x:v.border_x
          ~y:(v.y) ~w:v.w entry)
        v.entries;

      (* recurse to sub-msgbox *)
      match v.selected with
      | Some selected ->
          let entry = List.nth v.entries selected in
          begin match entry.fire with
          | MsgBox(true, box) -> render win s box
          | _ -> ()
          end
      | _ -> ()

end

module Title = struct

  (* menu in the upper bar *)
  type ('a, 'b) t = {
    x: int;
    y: int;
    w: int; (* for clicking only *)
    h: int;
    name: string;
    msgbox: ('a, 'b) MsgBox.t;
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

    (* Draw titles only *)
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

  let close_menu v =
    let msgbox = MsgBox.close v.msgbox in
    {v with msgbox}

  let render_msgbox win s v =
    MsgBox.render win s v.msgbox

  let do_open_menu s v =
    let msgbox = MsgBox.do_open_menu s v.msgbox in
    {v with msgbox}

end

module Global = struct

  type ('a, 'b) t = {
    menu_h: int;
    open_menu: int option;
    menus: ('a, 'b) Title.t list;
    indexes: (char, int) Hashtbl.t; (* for speed of search *)
  }

  let make ~menu_h menus =
    let indexes = Hashtbl.create 10 in
    List.iteri (fun i title ->
      match get_active_char title.Title.name with
      | Some c -> Hashtbl.replace indexes c i
      | None -> Hashtbl.replace indexes title.name.[0] i)
    menus;
  {
    menu_h;
    open_menu=None;
    menus;
    indexes;
  }

  let is_not_clicked v ~x ~y =
    let _x = x in
    y > v.menu_h && Option.is_none v.open_menu

  let is_menu_open v = Option.is_none v.open_menu

  let handle_click s v ~x ~y = 
    (* Check for closed menu *)
      if is_not_clicked v ~x ~y && not @@ is_menu_open v then
        (v, NoAction)
      else (
        (* Handle a top menu click first *)
        let menus = v.menus in
        let clicked_top_menu = List.find_idx (Title.is_title_clicked ~x ~y) menus in
        match clicked_top_menu, v.open_menu with
        | Some (i, _), Some mopen when i = mopen ->
            (* clicked top menu, same menu is open *)
            let menus = Utils.List.modify_at_idx mopen Title.close_menu menus in
            {v with menus; open_menu = None}, CloseMenu
        | Some (i, _), Some mopen ->
            (* clicked top menu, some other is open *)
            let menus =
              Utils.List.modify_at_idx mopen Title.close_menu menus
              |> Utils.List.modify_at_idx i (Title.do_open_menu s)
            in
            {v with menus; open_menu = Some i}, OpenMenu
        | Some (i, _), None ->
            (* clicked top menu, none are open *)
            let menus = Utils.List.modify_at_idx i (Title.do_open_menu s) menus in
            {v with menus; open_menu = Some i}, OpenMenu
        | None, (Some mopen as sopen) ->
            (* clicked elsewhere with open top menu *)
            let menus, action = 
              (* check menu itself *)
              Utils.List.modify_make_at_idx mopen (Title.handle_click s ~x ~y) menus
            in
            let action = action |> Option.get_exn_or "error" in
            (* Close the menu if it's a random click *)
            let open_menu, action, menus =
              match action with
              | NoAction ->
                  let menus = Utils.List.modify_at_idx mopen Title.close_menu menus in
                  None, CloseMenu, menus
              | _ ->
                  sopen, action, menus
            in
            ({v with menus; open_menu}, action)
        | None, None ->
            (* no menu open *)
            v, NoAction
      )

  let handle_key s v key = v, NoAction

  let update s v (event:Event.t) =
    match event with
    | MouseButton {down=true; x; y; _} ->
        handle_click s v ~x ~y
    | Key {down=true; key } ->
        handle_key s v key (* TODO *)
    | _ -> v, NoAction

  let render win s fonts v =
    (* Render menu titles *)
    List.iter (Title.render win ~fonts) v.menus;
    match v.open_menu with
    | None -> ()
    | Some i ->
        Title.render_msgbox win s (List.nth v.menus i)

end




