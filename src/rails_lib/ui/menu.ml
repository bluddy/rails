open Containers

(* Menu is specialized to 2 type parameters:
  1. The type of the messages it sends
  2. The type of the data it reads into its checkbox and visibility lambdas
*)

module C = Constants
module L = Utils.List
module CharMap = Utils.CharMap
open Utils.Infix

let src = Logs.Src.create "menu" ~doc:"Menu"
module Log = (val Logs.src_log src: Logs.LOG)

let menu_font = `Caps
let max_width = 320

type 'a action =
  | On of 'a
  | Off of 'a
  | Selected of 'a (* special action that happens when we select e.g. for main menu *)
  | HandledEvent (* handled event somewhere, but no action results *)
  | NoAction
  [@@deriving show]

let _is_action = function
  | NoAction | HandledEvent -> false
  | _ -> true

    (* Get the active char for the menu item *)
let _get_active_char str =
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

  let _get_entry_w_h font v =
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
        let w, h = _get_entry_w_h v.font entry in
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
        match _get_active_char entry.name with
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

  let _get_entry_selection_action v = match v.kind with
    | Interactive {select_action; _} -> select_action
    | _ -> None

  let _is_entry_mouse_shallow v ~y =
    match v.kind with
    | Static _ -> false
    | Interactive {enabled=false; _} -> false
    | _ -> y < v.y + v.h

  let is_entry_open_msgbox v = match v.kind with
    | Interactive {fire=MsgBox(true, _);_} -> true | _ -> false

  let is_entry_enabled v = match v.kind with
    | Interactive {enabled; _} -> enabled | _ -> true

  let is_entry_msgbox v = match v.kind with
    | Interactive {fire=MsgBox _; _} -> true | _ -> false

  let _is_entry_static v = match v.kind with
    | Static _ -> true | _ -> false

  let _find_mouse_entry_shallow v ~y =
    List.find_idx (_is_entry_mouse_shallow ~y) v.entries

  let _mouse_check_shallow v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

    (* Only search depth-first *)
  let rec handle_entry_hover_deep v ~x ~y =
    match v.kind with
    | Interactive ({fire=MsgBox(true, box); _} as e) ->
        (* msgbox is open so recurse *)
        let box' = _handle_hover box ~x ~y in
        if box' === box then v, `NoDeep
        else
          {v with kind=Interactive {e with fire=MsgBox(true, box')}}, `Deep
    | _ ->
        v, `NoDeep

  and _handle_hover (v:('a, 'b) t) ~x ~y =
    let entries, deep =
      match v.selected with
      | Some idx ->
          (* selection exists, deep search first *)
          L.modify_make_at_idx idx (handle_entry_hover_deep ~x ~y) v.entries
      | None ->
          (* Nothing selected, we're done *)
          v.entries, Some `NoDeep
    in
    let entries, selected =
      let default = entries, v.selected in
      match deep with
      | (None | Some `NoDeep) when _mouse_check_shallow v ~x ~y ->
          (* Didn't find in deep search, passed shallow search in this msgbox *)
          begin match _find_mouse_entry_shallow v ~y:(y-v.y), v.selected with
          | Some (entry_idx, _), Some cur_select when entry_idx <> cur_select ->
              (* hovered over an entry, handle and switch selection if not a msgbox *)
              let entry = List.nth v.entries cur_select in
              (* don't change msgbox *)
              if is_entry_open_msgbox entry then default
              else entries, Some entry_idx
          | _ -> default
          end

      | _ -> entries, v.selected
    in
    [%up {v with entries; selected}]

    (* Do not recurse deeply *)
  let _handle_entry_activate_shallow s ~x v =
    (* Assume we were clicked. Only handle shallow events
       full: an actual activation vs just msgbox open/close
     *)
    match v.kind with
    | Interactive e as e_in ->
        let e, action = match e.fire with
          | MsgBox(false, box) ->
              let box = do_open_menu s ~x ~y:(v.y) box in
              Interactive {e with fire=MsgBox(true, box)}, HandledEvent
          | MsgBox(true, box) ->
              Interactive {e with fire=MsgBox(false, box)}, HandledEvent
          | Action action ->
              e_in, On(action)
          | Checkbox(action, fn) when fn s ->
              e_in, Off(action)
          | Checkbox(action, _) ->
              e_in, On(action)
        in
        [%up {v with kind=e}], action
    | Static _ -> v, HandledEvent

  let rec _close_entry v = match v.kind with
    | Interactive ({fire=MsgBox(true, box); _} as e) ->
        let box = close box in
        {v with kind=Interactive {e with fire=MsgBox(false, box)}}
    | _ -> v

  and close v = match v.selected with
    | Some i ->
        let entries = L.modify_at_idx i _close_entry v.entries in
        {v with entries; selected=None}
    | None -> v

    (* Only search depth-first *)
  let rec _handle_entry_click_deep s v ~x ~y =
    match v.kind with
    | Interactive ({fire=MsgBox(true, box); _} as e) ->
        (* msgbox is open so recurse *)
        let box', action = _handle_click s box ~x ~y in
        if box' === box then v, action
        else
          {v with kind=Interactive {e with fire=MsgBox(true, box')}}, action
    | _ ->
        v, NoAction

  and _handle_click s v ~x ~y =
    let entries, action =
      match v.selected with
      | Some idx ->
          (* deep search first *)
          let a, b =
            L.modify_make_at_idx idx (_handle_entry_click_deep s ~x ~y) v.entries
          in
          a, b |> Option.get_exn_or "error"
      | None ->
          (* Nothing selected, we're done *)
          v.entries, NoAction
    in
    let entries, action, selected =
      match action with
      | NoAction when _mouse_check_shallow v ~x ~y ->
          (* Didn't find in deep search, do shallow search in this msgbox *)
          begin match _find_mouse_entry_shallow v ~y:(y-v.y), v.selected with
          | None, None ->
              (* clicked in msgbox but not an option *)
              entries, HandledEvent, v.selected
          | None, Some entry_idx ->
              (* clear selection *)
              let entries = L.modify_at_idx entry_idx _close_entry v.entries in
              entries, action, None
          | Some (entry_idx, _), _ ->
              (* clicked an entry, handle and switch selection *)
              let entries, action =
                L.modify_make_at_idx entry_idx
                (_handle_entry_activate_shallow ~x:v.x s) v.entries
              in
              entries, (action |> Option.get_exn_or "bad state"), Some entry_idx
          end

      | _ ->
          (* Got an action, pass it on *)
          entries, action, v.selected
    in
    [%up {v with entries; selected}], action

    let rec _handle_entry_key_deep s v ~key =
      match v.kind with
      | Interactive ({fire=MsgBox(true, box);_} as e) ->
          (* open msgbox -> recurse *)
          let box', action = _handle_key s box ~key in
          let kind =
            if box' === box then v.kind
            else
              Interactive {e with fire=MsgBox(true, box')}
          in
          [%up {v with kind}], action
      | _ ->
          v, NoAction

    and _handle_key s v ~key =
      let entries = v.entries in
      let entries, action =
        match v.selected with
        | Some idx ->
          (* deep search first *)
          L.modify_make_at_idx idx (_handle_entry_key_deep s ~key) entries
          |> Utils.snd_option
        | None ->
            (* Nothing selected, we're done *)
            entries, NoAction
      in
      let entries, action, selected =
        let handle_selection_change old_idx new_idx =
          let entries = match old_idx with
            | Some old_idx -> L.modify_at_idx old_idx _close_entry entries
            | None -> entries
          in
          let select_action = List.nth entries new_idx |> _get_entry_selection_action in
          let action = match select_action with
            | Some action -> Selected action
            | None -> HandledEvent
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
                  L.modify_make_at_idx idx (_handle_entry_activate_shallow s ~x:v.x) entries
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
            | None, Some idx, Escape when is_entry_open_msgbox (List.nth entries idx) ->
                let entries = L.modify_at_idx idx _close_entry entries in
                entries, HandledEvent, Some idx
            | None, _, _ when Event.is_letter key ->
                (* nothing matches but still a letter: don't leak back to previous menu *)
                entries, HandledEvent, v.selected
            | None, _, _ ->
                (* nothing matches: leak other things *)
                entries, NoAction, v.selected
            end
        | _ ->
            entries, action, v.selected
      in
      {v with entries; selected}, action

    let handle_event ?(do_close=true) s v (event:Event.t) _time =
      (* Returns new v and action *)
      let v, action = match event with
        | MouseMotion {x; y; _} -> _handle_hover v ~x ~y, NoAction
        | MouseButton {down=true; x; y; _} -> _handle_click s v ~x ~y
        | Key {down=true; key; _ } -> _handle_key s v ~key
        | _ -> v, NoAction
      in
      let v = match action with
        | (On _ | Off _) when do_close -> close v
        | _ -> v
      in
      v, action

    let _render_entry win s font v ~select_color ~use_prefix ~selected ~x ~border_x ~y ~w =
      if selected && not @@ _is_entry_static v then (
        let x = if use_prefix then x + 3 else x in
        Renderer.draw_rect win ~x ~y:(v.y + y - 1) ~w:(w-4) ~h:(v.h-1) ~fill:true ~color:select_color
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

    let rec render ?final_select_color win s v =
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
      (* Get selected entry *)
      let selected_entry = match v.selected with
        | Some selected ->
            let entry = List.nth v.entries selected in
            Some entry
        | _ -> None
      in
      (* Determine color by nature of selected entry *)
      let select_color = match final_select_color, selected_entry with
        | Some color, Some entry when not @@ is_entry_open_msgbox entry -> color
        | _ -> v.select_color
      in
      List.iteri (fun i entry ->
        _render_entry win s v.font ~select_color ~use_prefix:v.use_prefix ~selected:(i=selected)
          ~x:v.x ~border_x:v.border_x ~y:(v.y) ~w:v.w entry)
        v.entries;

      (* recurse to sub-msgbox *)
      match selected_entry with
      | Some entry ->
          begin match entry.kind with
          | Interactive {fire=MsgBox(true, box);_} -> render ?final_select_color win s box
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

  let _is_title_clicked v ~x ~y =
    x >= v.x && x <= v.x + v.w && y >= v.y && y <= v.y + v.h

  let _is_enabled s v = match v.test_enabled with
    | None -> true
    | Some f -> f s

  let handle_mouse ~click s v ~x ~y =
    let msgbox, action =
      if click then MsgBox._handle_click s v.msgbox ~x ~y
      else MsgBox._handle_hover v.msgbox ~x ~y, NoAction
    in
    [%up {v with msgbox}], action

  let _handle_key s v ~key =
    let msgbox, action = MsgBox._handle_key s v.msgbox ~key in
    [%up {v with msgbox}], action

    (* Draw titles only *)
  let render win s ~font v =
    let active_color, color = if _is_enabled s v then Ega.white, Ega.bcyan else Ega.gray, Ega.gray in
    Fonts.Font.write win font v.name ~x:v.x ~y:v.y ~color ~active_color

  let close_menu v =
    let msgbox = MsgBox.close v.msgbox in
    [%up {v with msgbox}]

  let render_msgbox ?final_select_color win s v =
    MsgBox.render ?final_select_color win s v.msgbox

  let do_open_menu s v =
    if _is_enabled s v then
      let msgbox = MsgBox.do_open_menu s v.msgbox in
      [%up {v with msgbox}]
    else v

end

module Global = struct
  (* The global menu bar at the top and the attached menus *)

  type ('msg, 'state) t = {
    open_menu: int option;
    menus: ('msg, 'state) Title.t list;
    num_menus: int;
    index: (char, int) Hashtbl.t; (* for speed of search *)
    w: int;
    h: int;
    font: Fonts.Font.t;
  }

  let make fonts ?(font_idx=menu_font) menus ~w ~h =
    let font=Fonts.get_font font_idx fonts in
    let index = Hashtbl.create 10 in
    List.iteri (fun i title ->
      match _get_active_char title.Title.name with
      | Some c -> Hashtbl.replace index c i
      | None -> ()) (* Hashtbl.replace index title.name.[0] i) *)
    menus;
  {
    open_menu=None;
    menus;
    num_menus=List.length menus;
    index;
    font;
    w;
    h;
  }

  let _is_not_clicked v ~x ~y =
    let _x = x in
    y > v.h && Option.is_none v.open_menu

  let _is_enabled s v i =
    (* Check if a menu index is enabled *)
    let menu = List.nth v.menus i in
    Title._is_enabled s menu

  let is_open v = Option.is_some v.open_menu
  let is_closed v = Option.is_none v.open_menu

  let _close v =
    let menus =
      match v.open_menu with
      | Some idx -> L.modify_at_idx idx Title.close_menu v.menus
      | None -> v.menus
    in
    [%up {v with menus; open_menu=None}]

  let _handle_mouse_move s v ~x ~y =
    match v.open_menu with
    | None -> v
    | Some mopen ->
        let menus, _ = L.modify_make_at_idx mopen
          (Title.handle_mouse ~click:false s ~x ~y) v.menus
        in
        [%up {v with menus}]

  let _handle_mouse_click s v ~x ~y = 
    (* Check for closed menu *)
      if _is_not_clicked v ~x ~y && is_closed v then
        v, NoAction
      else (
        (* Handle a top menu click first *)
        let menus = v.menus in
        let clicked_top_menu = List.find_idx (Title._is_title_clicked ~x ~y) menus in
        match clicked_top_menu, v.open_menu with
        | Some (i, _), _ when not @@ _is_enabled s v i ->
            (* Non-enabled menu *)
            v, NoAction
        | Some (i, _), Some mopen when i = mopen ->
            (* clicked top menu, same menu is open *)
            let menus = L.modify_at_idx mopen Title.close_menu menus in
            {v with menus; open_menu = None}, NoAction
        | Some (i, _), Some mopen ->
            (* clicked top menu, some other is open *)
            let menus =
              L.modify_at_idx mopen Title.close_menu menus
              |> L.modify_at_idx i (Title.do_open_menu s)
            in
            {v with menus; open_menu = Some i}, NoAction
        | Some (i, _), None ->
            (* clicked top menu, none are open *)
            let menus = L.modify_at_idx i (Title.do_open_menu s) menus in
            {v with menus; open_menu = Some i}, NoAction
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
                  None, NoAction, menus
              | _ ->
                  sopen, action, menus
            in
            [%up {v with menus; open_menu}], action
        | None, None ->
            (* no menu open *)
            v, NoAction
      )

  let _handle_key s v ~key =
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
            {v with open_menu=sidx; menus}, NoAction
          end
      | Some open_menu as some_menu ->
          (* Open menu -> send it on *)
          let menus, action =
            L.modify_make_at_idx open_menu (Title._handle_key s ~key) v.menus
            |> Utils.snd_option
          in
          let menus, open_menu, action =
            match action, key with
            | NoAction, Event.Escape ->
                menus, None, NoAction
            | NoAction, Event.Left when open_menu > 0 ->
                let menus =
                  L.modify_at_idx open_menu Title.close_menu menus
                  |> L.modify_at_idx (open_menu - 1) (Title.do_open_menu s)
                in
                menus, Some(open_menu - 1), NoAction
            | NoAction, Event.Right when open_menu < v.num_menus - 1 ->
                let menus =
                  L.modify_at_idx open_menu Title.close_menu menus
                  |> L.modify_at_idx (open_menu + 1) (Title.do_open_menu s)
                in
                menus, Some(open_menu + 1), NoAction
            (* Avoid events leaking out when menu is open *)
            | NoAction, _ -> menus, some_menu, NoAction
            | _ -> menus, some_menu, action
          in
          {v with menus; open_menu}, action

  let handle_event ?(do_close=true) s v (event:Event.t) _time =
    (* Returns new v and the action derived from the menu *)
    let v, action = match event with
      | MouseMotion {x; y; _} when is_open v ->
          _handle_mouse_move s v ~x ~y, NoAction
      | MouseButton {down=true; x; y; _} ->
          _handle_mouse_click s v ~x ~y
      | Key {down=true; key; _ } ->
          _handle_key s v ~key
      | _ -> v, NoAction
    in
    (* Cancel events if menu is open *)
    let event = if is_open v then Event.NoEvent else event in
    let v = match action with
      | (On _ | Off _) when do_close -> _close v
      | _ -> v
    in
    v, action, event

  let render ?final_select_color win s v =
    Renderer.draw_rect win ~x:0 ~y:0 ~w:v.w ~h:v.h ~color:Ega.cyan ~fill:true;
    (* Render menu titles *)
    List.iter (Title.render win s ~font:v.font) v.menus;
    match v.open_menu with
    | None -> ()
    | Some i -> Title.render_msgbox ?final_select_color win s (List.nth v.menus i)

end

module Animated = struct
  (* Caches the last message to allow animation of menu, both msgbox and global
     These need to be taken care of in handle_tick
   *)

  type ('msg, 'state) menu =
    | Global of ('msg, 'state) Global.t
    | MsgBox of ('msg, 'state) MsgBox.t

  type ('msg, 'state) t = {
    menu: ('msg, 'state) menu;
    last_msg: ('msg action * int) option; (* end_time *)
  }

  let make_global fonts ?font_idx menus ~w ~h =
    let menu = Global.make fonts ?font_idx menus ~w ~h in
    {menu=Global menu; last_msg=None}

  let make_msgbox ?heading ?x ?y ?font_idx ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries =
    let menu = MsgBox.make ?heading ?x ?y ?font_idx ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries in
    {menu=MsgBox menu; last_msg=None}

  let _close v =
    let menu = match v.menu with
      | Global g ->
          let g' = Global._close g in
          if g' === g then v.menu
          else Global g'
      | MsgBox m ->
          let m' = MsgBox.close m in
          if m' === m then v.menu
          else MsgBox m'
    in
    [%up {v with menu}]

  let is_open v = match v.menu with
    | Global g -> Global.is_open g
    | MsgBox _ -> true

  let is_closed v = not @@ is_open v

  let render win s v =
    let final_select_color = if Option.is_some v.last_msg then Ega.white else Ega.bcyan in
    match v.menu with
    | Global g -> Global.render ~final_select_color win s g
    | MsgBox m -> MsgBox.render ~final_select_color win s m

  let handle_event s v (event:Event.t) time = match v.menu with
    (* Intercept actions and save for later *)
    | Global g ->
        (* Can let events through *)
        let g', action, event = Global.handle_event ~do_close:false s g event time in
        let menu = if g' === g then v.menu else Global g' in
        let last_msg, event = if _is_action action then Some (action, time + C.Menu.exit_time), Event.NoEvent
          else v.last_msg, event
        in
        ([%up {v with menu; last_msg}], event) [@warning "-23"]
    | MsgBox m ->
        (* Always swallows up events *)
        let m', action = MsgBox.handle_event ~do_close:false s m event time in
        let menu = if m' === m then v.menu else MsgBox m' in
        let last_msg = if _is_action action then Some (action, time + C.Menu.exit_time) else v.last_msg in
        ([%up {v with menu; last_msg}], Event.NoEvent) [@warning "-23"]

  let handle_tick _s v time = match v.last_msg with
    | Some (msg, t) when time > t ->
        let v =  _close v in
        {v with last_msg=None}, msg
    | _ -> v, NoAction

end

let modal_handle_event = fun (type a) ?(is_msgbox=false) s (menu: (a, 'state) MsgBox.t) event time ->
  (* Handle all events for modal msgboxes/menus *)
  let menu, action = MsgBox.handle_event s menu event time in
  match action with
  | NoAction when Event.pressed_esc event -> `Exit
  | NoAction when is_msgbox && Event.modal_dismiss event -> `Exit
  | HandledEvent when is_msgbox -> `Exit
  | On(choice) -> `Activate choice
  | NoAction -> `Stay menu
  | _ -> `Stay menu

