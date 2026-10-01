open! Containers
module R = Engine.Renderer
module B = Backend
module C = Constants
module CS = Constants.Save
module Event = Engine.Event

let src = Logs.Src.create "savegame" ~doc:"Save_game"
module Log = (val Logs.src_log src: Logs.LOG)

include Save_game_d

open Utils.Infix

let sp = Printf.sprintf

let save_game_of_i i = sp "game%d.sav" i

let _make action (s:State.t) =
  let entries = make_entries () in
  let open Menu in
  let open MsgBox in
  let entries = List.map (fun entry ->
    let s = Option.get_or ~default:"EMPTY" entry.header in
    make_entry s @@ `Action(entry))
    entries
  in
  let menu =
    make ~fonts:s.fonts entries ~x:20 ~y:20
     |> Menu.MsgBox.do_open_menu s
  in
  {menu; action; error_modal = None}

let make_save s = _make `Save s
let make_load s = _make `Load s

let render win (s:State.t) v =
  Menu.MsgBox.render win s v.menu;
  match v.error_modal with
  | Some modal -> Menu.MsgBox.render win s modal
  | None -> ()

let _save_title (s:State.t) =
  let b = s.backend in
  let name = B.get_name C.player b in
  let difficulty = B.get_difficulty b |> B_options.show_difficulty in
  sp "%s (%s) %d" name difficulty b.params.year

let _save_game (s:State.t) slot =
  try
    let to_string = Yojson.Safe.to_string in
    let header = {save_title=_save_title s; version=CS.version} |> Header.yojson_of_t |> to_string in
    let backend = Backend.yojson_of_t s.backend |> to_string in
    let options = Main_ui_d.yojson_of_options s.ui.options |> to_string in
    let mapview = Mapview_d.yojson_of_t s.ui.view |> to_string in
    let data = String.concat "====" [header; backend; options; mapview] in
    let game_name = save_game_of_i slot in
    match Save_compression.write_save_file game_name data with
    | Ok () ->
        Log.info (fun m -> m "Saved game to %s" game_name);
        Ok ()
    | Error err ->
        Log.err (fun m -> m "Failed to write save to %s: %s" game_name err);
        Error (Printf.sprintf "Failed to write file: %s" err)
  with exn ->
    let err = Printexc.to_string exn in
    Log.err (fun m -> m "Exception while saving to slot %d: %s" slot err);
    Error err

let handle_event (s:State.t) v event time =
  match v.error_modal with
  | Some modal ->
      begin match Menu.MsgBox.modal_handle_event ~is_msgbox:true s modal event time with
      | `Exit | `Activate _ -> `Stay, {v with error_modal = None}
      | `Stay modal2 -> `Stay, {v with error_modal = Some modal2}
      end
  | None ->
      if Event.pressed_esc event then `Exit, v else
      match Menu.MsgBox.handle_event s v.menu event time with
      | menu2, Menu.On(entry) -> (* load entry *)
          let v = {v with menu=menu2} in
          let slot = entry.slot in
          begin match v.action with
          | `Save ->
              begin match _save_game s slot with
              | Ok () -> `Exit, v
              | Error err ->
                  let msg = Printf.sprintf "Could not save game to slot %d:\n%s" slot err in
                  let modal = Menu.MsgBox.make_basic ~tight:true ~fonts:s.fonts s msg in
                  `Stay, {v with error_modal = Some modal}
              end
          | _ -> assert false
          end
      | menu2, _ when menu2 === v.menu -> `Stay, v
      | menu2, _ -> `Stay, {v with menu=menu2}
  


