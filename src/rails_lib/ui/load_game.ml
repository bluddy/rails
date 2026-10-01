open! Containers
module R = Engine.Renderer
module B = Backend
module C = Constants
module CS = Constants.Save
module Event = Engine.Event

let src = Logs.Src.create "loadgame" ~doc:"Load_game"
module Log = (val Logs.src_log src: Logs.LOG)

include Save_game_d

open Utils.Infix

let sp = Printf.sprintf

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

let make s = _make `Load s

let render win (s:State.t) v =
  Menu.MsgBox.render win s v.menu;
  match v.error_modal with
  | Some modal -> Menu.MsgBox.render win s modal
  | None -> ()

(* Make state out of loaded game *)
let _load_state backend ui_options ui_view win sound =
  let resources = Resources.load_all () in
  let region =  backend.Backend_d.params.region in
  let textures = Textures.of_resources win resources in
  let map_tex = R.Texture.make win @@ Tilemap.to_img backend.map in
  let map_silhouette_tex = R.Texture.make win @@ Tilemap.to_silhouette backend.map in
  let fonts = Fonts.load (Engine.Paths.data_file "FONTS.RR") win in
  let ui = Main_ui.default ~options:ui_options ~view:ui_view win fonts region in
  {
    State.map_tex;
    map_silhouette_tex;
    mode=State.Game;
    backend;
    resources;
    textures;
    fonts;
    ui;
    win;
    random = Random.get_state ();
    sound;
  }

let load_game slot win sound : (State.t, string) result =
  let game_name = save_game_of_i slot in
  match Save_compression.read_save_file game_name with
  | Error err -> Error (Printf.sprintf "Failed to read %s: %s" game_name err)
  | Ok content ->
      let lst = String.split content ~by:"====" in
      match lst with
      | [header_str; backend_str; options_str; view_str] ->
          begin match Header.of_string header_str with
          | Error err -> Error (Printf.sprintf "Invalid save header: %s" err)
          | Ok header ->
              if header.version > CS.version then
                Error (Printf.sprintf "Save file version %d is newer than game version %d. Please update the game." header.version CS.version)
              else
                try
                  let from_string = Yojson.Safe.from_string in
                  let backend_json = from_string backend_str in
                  let options_json = from_string options_str in
                  let view_json = from_string view_str in
                  let sections = Save_migrations.{
                    backend = backend_json;
                    options = options_json;
                    view = view_json;
                  } in
                  match Save_migrations.migrate ~from_version:header.version ~target_version:CS.version sections with
                  | Error err -> Error (Printf.sprintf "Migration error: %s" err)
                  | Ok migrated ->
                      let backend = Backend.t_of_yojson migrated.backend in
                      let backend = {backend with pause = false} in
                      Backend.reset_tick backend;
                      let ui_options = Main_ui_d.options_of_yojson migrated.options in
                      let ui_view = Mapview_d.t_of_yojson migrated.view in
                      Ok (_load_state backend ui_options ui_view win sound)
                with
                | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
                | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
                    Error (Printf.sprintf "Save data schema mismatch: %s" (Printexc.to_string exn))
                | exn -> Error (Printf.sprintf "Error loading save: %s" (Printexc.to_string exn))
          end
      | _ -> Error "Corrupted save file: invalid section format (expected 4 sections)"

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
          | `Load ->
              begin match load_game slot s.win s.sound with
              | Ok loaded_state -> `LoadGame loaded_state, v
              | Error err ->
                  Log.err (fun m -> m "Failed to load slot %d: %s" slot err);
                  let msg = Printf.sprintf "Could not load save slot %d:\n%s" slot err in
                  let modal = Menu.MsgBox.make_basic ~tight:true ~fonts:s.fonts s msg in
                  `Stay, {v with error_modal = Some modal}
              end
          | _ -> assert false
          end
      | menu2, _ when menu2 === v.menu -> `Stay, v
      | menu2, _ -> `Stay, {v with menu=menu2}
  

