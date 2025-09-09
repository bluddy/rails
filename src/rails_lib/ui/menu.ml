open Containers

(* Menu is specialized to 2 type parameters:
  1. The type of the messages it sends
  2. The type of the data it reads into its checkbox and visibility lambdas
*)

module L = Utils.List
module CharMap = Utils.CharMap
open Utils.Infix

let menu_font = `Caps
let max_width = 320

  type 'a action =
    | On of 'a
    | Off of 'a
    | Selected of 'a
    | Handled
    | OpenMsgBox
    | CloseMsgBox
    | ClickInMsgBox (* a click but no action *)
    | KeyInMsgBox (* Not a selection, but noop *)
    | OpenMenu
    | CloseMenu
    | NoAction
    [@@deriving show]

  let is_action = function
    | NoAction -> false
    | _ -> true

    (* Get the active char for the menu item *)
let get_active_char str =
  String.fold (fun (amp, letter) c ->
    match c with
    | '&' -> (true, None)
    | c when amp -> (false, Some (Char.uppercase_ascii c))
    | _ -> (amp, letter)
  )
  (false, None)
  str
  |> snd

module MsgBox = struct

  type ('msg, 'state) fire =
    | Action of 'msg
    | Checkbox of 'msg * ('state -> bool) (* function to check checkbox status *)
    | MsgBox of bool * ('msg, 'state) t (* whether a further msgbox/menu is open open *)

  and ('msg, 'state) kind =
    | Static of {
      color: int * int * int * int;
    }
    | Interactive of {
      fire: ('msg, 'state) fire;
      test_enabled: ('state -> bool) option;
      select_action: 'msg option;
      enabled: bool;
    }

  and ('msg, 'state) entry = {
    y: int;
    h: int;
    name: string;
    kind: ('msg, 'state) kind;
  }

  and ('msg, 'state) t =
    { 
      x: int; y: int;
      w: int; h: int;
      border_x: int; border_y: int;
      heading: string option;
      entries: ('msg, 'state) entry list;
      selected: int option;
      font: Fonts.Font.t;
      index: int CharMap.t; (* for keyboard shortcuts *)
      draw_bg: bool;
      select_color: Ega.color;
      use_prefix: bool; (* entry prefix for checked v space *)
    }

  let get_entry_w_h font v =
    Fonts.Font.get_str_w_h ~skip_amp:true font @@ " "^v.name

  let static_entry name ~color =
    { y=0; h=0; name; kind=Static {color} }

  let make_entry ?test_enabled ?select_action name fire =
    (* test_enabled: entry can be disabled under some circumstances *)
    let fire = match fire with
      | `MsgBox m -> MsgBox(false, m)
      | `Action a -> Action a
      | `Checkbox (b, fn) -> Checkbox(b, fn)
    in
    { y=0; h=0; name;
      kind=Interactive {
        fire;
        enabled=true;
        test_enabled;
        select_action;
      }
    }

    (* Compute menu size dynamically. Must be called. *)
  let do_open_menu ?(x=0) ?(y=0) ?wh ?(selected=Some 0) s v =
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
        let w, h = get_entry_w_h v.font entry in
        let kind = match entry.kind with
          | Interactive e ->
            let enabled = match e.test_enabled with
              | Some f -> f s
              | None -> true
            in
            Interactive {e with enabled}
          | Static _ as k -> k
        in
        let entry = {entry with y=max_h; h; kind} in
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
    let w, h = Option.get_or ~default:(w, h) wh in
    {v with entries; w; h; x; y; selected}

  let make ?heading ?(x=0) ?(y=0) ?(font_idx=menu_font) ?(select_color=Ega.bcyan) ?(draw_bg=true) ?(use_prefix=true)
      ?(border_x=8) ?(border_y=6) ~fonts entries =
    let font=Fonts.get_font font_idx fonts in
    let index =
      List.foldi (fun acc i entry ->
        match get_active_char entry.name with
        | Some c -> CharMap.add c i acc
        | None -> CharMap.add entry.name.[0] i acc)
      CharMap.empty
      entries
    in
    {
      border_x; border_y;
      x; y; w=0; h=0;
      entries;
      selected=None;
      font;
      heading;
      index;
      draw_bg;
      use_prefix;
      select_color;
    }

  let get_entry_selection_action v = match v.kind with
    | Interactive {select_action; _} -> select_action
    | _ -> None

  let is_entry_mouse_shallow v ~y =
    match v.kind with
    | Static _ -> false
    | Interactive {enabled=false; _} -> false
    | _ -> y < v.y + v.h

  let is_entry_open v = match v.kind with
    | Interactive {fire=MsgBox(true, _);_} -> true | _ -> false

  let is_entry_enabled v = match v.kind with
    | Interactive {enabled; _} -> enabled | _ -> true

  let is_entry_msgbox v = match v.kind with
    | Interactive {fire=MsgBox _; _} -> true | _ -> false

  let is_entry_static v = match v.kind with
    | Static _ -> true | _ -> false

  let find_mouse_entry_shallow v ~y =
    List.find_idx (is_entry_mouse_shallow ~y) v.entries

  let mouse_check_shallow v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

    (* Do not recurse deeply *)
  let handle_entry_activate_shallow ~full s ~x v =
    (* Assume we were clicked. Only handle shallow events
       full: an actual activation vs just msgbox open/close
     *)
    match v.kind with
    | Interactive e as e_in ->
        let e, action = match e.fire with
          | MsgBox(false, box) ->
              let box = do_open_menu s ~x ~y:(v.y) box in
              Interactive {e with fire=MsgBox(true, box)}, OpenMsgBox
          | MsgBox(true, box) ->
              Interactive {e with fire=MsgBox(false, box)}, CloseMsgBox
          | Action action when full ->
              e_in, On(action)
          | Checkbox(action, fn) when full && fn s ->
              e_in, Off(action)
          | Checkbox(action, _) when full ->
              e_in, On(action)
          | _ -> e_in, NoAction
        in
        [%up {v with kind=e}], action
    | Static _ -> v, NoAction

  let rec close_entry v = match v.kind with
    | Interactive ({fire=MsgBox(true, box); _} as e) ->
        let box = close box in
        {v with kind=Interactive {e with fire=MsgBox(false, box)}}
    | _ -> v

  and close v = match v.selected with
    | Some i ->
        let entries = L.modify_at_idx i close_entry v.entries in
        {v with entries; selected=None}
    | None -> v

    (* Only search depth-first *)
  let rec handle_entry_mouse_deep ~click s v ~x ~y =
    match v.kind with
    | Interactive ({fire=MsgBox(true, box); _} as e) ->
        (* msgbox is open so recurse *)
        let box', action = handle_mouse ~click s box ~x ~y in
        if box' === box then v, action
        else
          {v with kind=Interactive {e with fire=MsgBox(true, box')}}, action
    | _ ->
        v, NoAction

  and handle_mouse ~click s (v:('a, 'b) t) ~x ~y =
    let entries, action =
      match v.selected with
      | Some idx ->
          (* deep search first *)
          let a, b =
            L.modify_make_at_idx idx (handle_entry_mouse_deep ~click s ~x ~y) v.entries
          in
          a, b |> Option.get_exn_or "error"
      | None ->
          (* Nothing selected, we're done *)
          v.entries, NoAction
    in
    let entries, action, selected =
      match action with
      | NoAction when mouse_check_shallow v ~x ~y ->
          (* Didn't find in deep search, do shallow search in this msgbox *)
          begin match find_mouse_entry_shallow v ~y:(y-v.y), v.selected with
          | None, None when click ->
              (* clicked in msgbox but not an option *)
              entries, ClickInMsgBox, v.selected
          | None, Some entry_idx ->
              (* clear selection *)
              let entries = L.modify_at_idx entry_idx close_entry v.entries in
              entries, action, None
          | Some (entry_idx, _), _ ->
              (* clicked an entry, handle and switch selection *)
              let entries, action =
                L.modify_make_at_idx entry_idx
                (handle_entry_activate_shallow ~full:click ~x:v.x s) v.entries
              in
              entries, (action |> Option.get_exn_or "bad state"), Some entry_idx

          | _ -> entries, action, None
          end

      | _ ->
          (* Got an action, pass it on *)
          entries, action, v.selected
    in
    [%up {v with entries; selected}], action

    let rec handle_entry_key_deep s v ~key =
      match v.kind with
      | Interactive ({fire=MsgBox(true, box);_} as e) ->
          (* open msgbox -> recurse *)
          let box, action = handle_key s box ~key in
          {v with kind=Interactive {e with fire=MsgBox(true, box)}}, action
      | _ ->
          v, NoAction

    and handle_key s v ~key =
      let entries, action =
        match v.selected with
        | Some idx ->
          (* deep search first *)
          L.modify_make_at_idx idx (handle_entry_key_deep s ~key) v.entries
          |> Utils.snd_option
        | None ->
            (* Nothing selected, we're done *)
            v.entries, NoAction
      in
      let entries, action, selected =
        let handle_selection_change old_idx new_idx =
          let entries = match old_idx with
            | Some old_idx -> L.modify_at_idx old_idx close_entry entries
            | None -> entries
          in
          let select_action = List.nth entries new_idx |> get_entry_selection_action in
          let action = match select_action with
            | Some action -> Selected action
            | None -> KeyInMsgBox
          in
          entries, action, Some new_idx
        in
        match action with
        | NoAction -> (* nothing from deep *)
            let menu_choice =
              if Event.is_letter key then
                CharMap.find_opt (Event.char_of_key ~shift:true key) v.index
              else
                None
            in
            begin match menu_choice, v.selected, key with
            | Some new_idx, entry_idx, _ ->
                handle_selection_change entry_idx new_idx
            | None, (Some idx as sidx), Enter ->
                let entries, action =
                  L.modify_make_at_idx idx (handle_entry_activate_shallow ~full:true s ~x:v.x) entries
                  |> Utils.snd_option
                in
                entries, action, sidx
            | None, None, Down ->
                handle_selection_change None 0
            | None, None, Up ->
                handle_selection_change None (List.length entries - 1)
            | None, (Some idx as sidx), Down when idx < List.length entries - 1 ->
                handle_selection_change sidx (idx + 1)
            | None, (Some idx as sidx), Up when idx > 0 ->
                handle_selection_change sidx (idx - 1)
            | None, Some idx, Escape when is_entry_open (List.nth entries idx) ->
                let entries = L.modify_at_idx idx close_entry entries in
                entries, KeyInMsgBox, Some idx
            | None, _, _ when Event.is_letter key ->
                (* nothing matches but still a letter: don't leak back to previous menu *)
                entries, KeyInMsgBox, v.selected
            | None, _, _ ->
                (* nothing matches: leak other things *)
                entries, NoAction, v.selected
            end
        | _ ->
            entries, action, v.selected
      in
      {v with entries; selected}, action

    let handle_event s v (event:Event.t) =
      (* Returns new v and action *)
      let v, action =
        match event with
        | MouseButton {down=true; x; y; _} ->
            handle_mouse ~click:true s v ~x ~y
        | Key {down=true; key; _ } ->
            handle_key s v ~key
        | _ -> v, NoAction
      in
      let v = match action with
        | On _ | Off _ -> close v
        | _ -> v
      in
      v, action

    let render_entry win s font v ~bg_color ~use_prefix ~selected ~x ~border_x ~y ~w =
      if selected && not @@ is_entry_static v then (
        let x = if use_prefix then x + 3 else x in
        Renderer.draw_rect win ~x ~y:(v.y + y - 1) ~w:(w-4) ~h:(v.h-1) ~fill:true ~color:bg_color
      );

      let prefix = match v.kind with
        | Interactive {fire=Checkbox(_, fn);_} when fn s -> "^"
        | Interactive _ -> " "
        | Static _ -> ""
      in
      let color, active_color = match v.kind with
        | Interactive {enabled=true;_} -> Ega.black, Ega.blue
        | Interactive _ -> Ega.dgray, Ega.dgray
        | Static {color;_} -> color, color
      in
      let name = if use_prefix then prefix^v.name else v.name in
      Fonts.Font.write win font ~color name ~x:(x+border_x) ~y:(y + v.y) ~active_color ~tag_color:Ega.bred

    let render_box ?(color=Ega.gray) win x y w h =
      Renderer.draw_rect win ~x:(x+1) ~y:(y+1) ~w ~h ~color ~fill:true;
      Renderer.draw_rect win ~x:(x+1) ~y:(y+1) ~w ~h ~color:Ega.white ~fill:false;
      Renderer.draw_rect win ~x:x ~y ~w:(w+2) ~h:(h+2) ~color:Ega.black ~fill:false

    let rec render win s v =
      (* draw background *)
      if v.draw_bg then (
        render_box win v.x v.y v.w v.h
      );

      (* draw heading *)
      begin match v.heading with
      | Some str ->
          Fonts.Font.write win v.font ~color:Ega.white str ~x:(v.x + v.border_x) ~y:(v.y + v.border_y)
      | None -> ()
      end;

      (* draw entries and selection *)
      let selected = Option.get_or v.selected ~default:(-1) in
      List.iteri (fun i entry ->
        render_entry win s v.font ~bg_color:v.select_color ~use_prefix:v.use_prefix ~selected:(i=selected)
          ~x:v.x ~border_x:v.border_x ~y:(v.y) ~w:v.w entry)
        v.entries;

      (* recurse to sub-msgbox *)
      match v.selected with
      | Some selected ->
          let entry = List.nth v.entries selected in
          begin match entry.kind with
          | Interactive {fire=MsgBox(true, box);_} -> render win s box
          | _ -> ()
          end
      | _ -> ()

    let make_basic ?x ?y ?wh ?heading ~fonts s text =
      (* Easy to use msgbox with just text *)
      let y = Option.get_or ~default:80 y in
      let x = match x with
        | Some x -> x
        | None ->
          let len = String.index_opt text '\n'
            |> Option.get_or ~default:(String.length text)
          in
          150 - 5 * len / 2 
      in
      let entry_color = if Option.is_some heading then Ega.black else Ega.white in 
      let entry = static_entry ~color:entry_color text in
      let menu =
        make ~x ~y ?heading ~fonts [entry] ~font_idx:`Standard |> do_open_menu ?wh s
      in
      menu
end

module Title = struct

  (* menu name in the upper bar. Forwards to actual menus if open. *)
  type ('msg, 'state) t = {
    x: int;
    y: int;
    w: int; (* for clicking only *)
    h: int;
    name: string;
    msgbox: ('msg, 'state) MsgBox.t;
    test_enabled: ('state -> bool) option;
  }

  let make ?test_enabled ~fonts ~x ~y name msgbox =
    let w, h = Fonts.get_str_w_h ~skip_amp:true ~fonts ~idx:menu_font name in
    {
      x; y;
      w; h;
      name;
      msgbox;
      test_enabled;
    }

  let is_title_clicked v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

  let is_enabled s v = match v.test_enabled with
    | None -> true
    | Some f -> f s

  let handle_mouse ~click s v ~x ~y =
    let msgbox, action = MsgBox.handle_mouse ~click s v.msgbox ~x ~y in
    [%up {v with msgbox}], action

  let handle_key s v ~key =
    let msgbox, action = MsgBox.handle_key s v.msgbox ~key in
    [%up {v with msgbox}], action

    (* Draw titles only *)
  let render win s ~fonts v =
    let active_color, color = if is_enabled s v then Ega.white, Ega.bcyan else Ega.gray, Ega.gray in
    Fonts.Render.write win fonts v.name ~idx:menu_font ~x:v.x ~y:v.y ~color ~active_color

  let close_menu v =
    let msgbox = MsgBox.close v.msgbox in
    [%up {v with msgbox}]

  let render_msgbox win s v =
    MsgBox.render win s v.msgbox

  let do_open_menu s v =
    if is_enabled s v then
      let msgbox = MsgBox.do_open_menu s v.msgbox in
      [%up {v with msgbox}]
    else v

end

module Global = struct
  (* The global menu bar at the top and the attached menus *)

  type ('msg, 'state) t = {
    menu_h: int;
    open_menu: int option;
    menus: ('msg, 'state) Title.t list;
    num_menus: int;
    index: (char, int) Hashtbl.t; (* for speed of search *)
  }

  let make ~menu_h menus =
    let index = Hashtbl.create 10 in
    List.iteri (fun i title ->
      match get_active_char title.Title.name with
      | Some c -> Hashtbl.replace index c i
      | None -> ()) (* Hashtbl.replace index title.name.[0] i) *)
    menus;
  {
    menu_h;
    open_menu=None;
    menus;
    num_menus=List.length menus;
    index;
  }

  let is_not_clicked v ~x ~y =
    let _x = x in
    y > v.menu_h && Option.is_none v.open_menu

  let is_enabled s v i =
    (* Check if a menu index is enabled *)
    let menu = List.nth v.menus i in
    Title.is_enabled s menu

  let is_open v = Option.is_some v.open_menu
  let is_closed v = Option.is_none v.open_menu

  let close v =
    let menus =
      match v.open_menu with
      | Some idx -> L.modify_at_idx idx Title.close_menu v.menus
      | None -> v.menus
    in
    {v with menus; open_menu=None}

  let handle_mouse_move s v ~x ~y =
    match v.open_menu with
    | None -> v
    | Some mopen ->
        let menus = L.modify_at_idx mopen
          (Title.handle_mouse ~click:false s v ~x ~y) v.menus
        in
        [%up {v with menus}]

  let handle_mouse_click s v ~x ~y = 
    (* Check for closed menu *)
      if is_not_clicked v ~x ~y && is_closed v then
        v, NoAction
      else (
        (* Handle a top menu click first *)
        let menus = v.menus in
        let clicked_top_menu = List.find_idx (Title.is_title_clicked ~x ~y) menus in
        match clicked_top_menu, v.open_menu with
        | Some (i, _), _ when not @@ is_enabled s v i ->
            (* Non-enabled menu *)
            v, NoAction
        | Some (i, _), Some mopen when i = mopen ->
            (* clicked top menu, same menu is open *)
            let menus = L.modify_at_idx mopen Title.close_menu menus in
            {v with menus; open_menu = None}, CloseMenu
        | Some (i, _), Some mopen ->
            (* clicked top menu, some other is open *)
            let menus =
              L.modify_at_idx mopen Title.close_menu menus
              |> L.modify_at_idx i (Title.do_open_menu s)
            in
            {v with menus; open_menu = Some i}, OpenMenu
        | Some (i, _), None ->
            (* clicked top menu, none are open *)
            let menus = L.modify_at_idx i (Title.do_open_menu s) menus in
            {v with menus; open_menu = Some i}, OpenMenu
        | None, (Some mopen as sopen) ->
            (* clicked elsewhere with open top menu *)
            let menus, action = 
              (* check menu itself *)
              L.modify_make_at_idx mopen (Title.handle_mouse ~click:true s ~x ~y) menus
            in
            let action = action |> Option.get_exn_or "error" in
            (* Close the menu if it's a random click *)
            let open_menu, action, menus =
              match action with
              | NoAction ->
                  let menus = L.modify_at_idx mopen Title.close_menu menus in
                  None, CloseMenu, menus
              | _ ->
                  sopen, action, menus
            in
            [%up {v with menus; open_menu}], action
        | None, None ->
            (* no menu open *)
            v, NoAction
      )

  let handle_key s v ~key =
      let get_char () =
        if Event.is_letter key then
          Hashtbl.find_opt v.index (Event.char_of_key ~shift:true key)
        else None
      in
      match v.open_menu with
      | None ->
          begin match get_char () with
          | None -> v, NoAction
          | Some idx as sidx ->
            let menus = L.modify_at_idx idx (Title.do_open_menu s) v.menus in
            {v with open_menu=sidx; menus}, OpenMenu
          end
      | (Some open_menu as some_menu) ->
          (* Open menu -> send it on *)
          let menus, action =
            L.modify_make_at_idx open_menu (Title.handle_key s ~key) v.menus
            |> Utils.snd_option
          in
          let menus, open_menu, action =
            match action, key with
            | NoAction, Event.Escape ->
                menus, None, KeyInMsgBox
            | NoAction, Event.Left when open_menu > 0 ->
                let menus =
                  L.modify_at_idx open_menu Title.close_menu menus
                  |> L.modify_at_idx (open_menu - 1) (Title.do_open_menu s)
                in
                menus, Some(open_menu - 1), KeyInMsgBox
            | NoAction, Event.Right when open_menu < v.num_menus - 1 ->
                let menus =
                  L.modify_at_idx open_menu Title.close_menu menus
                  |> L.modify_at_idx (open_menu + 1) (Title.do_open_menu s)
                in
                menus, Some(open_menu + 1), KeyInMsgBox
            (* Avoid events leaking out when menu is open *)
            | NoAction, _ -> menus, some_menu, KeyInMsgBox
            | _ -> menus, some_menu, action
          in
          {v with menus; open_menu}, action

  let handle_event s v (event:Event.t) =
    (* Returns new v and the action derived from the menu *)
    let v, action = match event with
      | MouseMotion {x; y; _} when is_open v ->
          handle_mouse_move s v ~x ~y
      | MouseButton {down=true; x; y; _} ->
          handle_mouse_click s v ~x ~y
      | Key {down=true; key; _ } ->
          handle_key s v ~key
      | _ -> v, NoAction
    in
    (* TODO: add activation animation, blue and white *)
    let v = match action with
      | On _ | Off _ -> close v
      | _ -> v
    in
    (* Cancel events we handled *)
    let event = match action with
      | NoAction -> event
      | _ -> NoEvent
    in
    v, action, event

  let render win s fonts v ~w ~h =
    Renderer.draw_rect win ~x:0 ~y:0 ~w ~h ~color:Ega.cyan ~fill:true;
    (* Render menu titles *)
    List.iter (Title.render win s ~fonts) v.menus;
    match v.open_menu with
    | None -> ()
    | Some i -> Title.render_msgbox win s (List.nth v.menus i)

end

let modal_handle_event = fun (type a) ?(is_msgbox=false) s (menu: (a, 'state) MsgBox.t) event ->
  (* Handle all events for modal msgboxes/menus *)
  let menu, action = MsgBox.handle_event s menu event in
  match action with
  | NoAction when Event.pressed_esc event -> `Exit
  | NoAction when is_msgbox && Event.key_modal_dismiss event -> `Exit
  | ClickInMsgBox when is_msgbox -> `Exit
  | On(choice) -> `Activate choice
  | NoAction -> `Stay menu
  | _ -> `Stay menu

