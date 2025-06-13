open Containers

(* UI for 1st delivery/pickup of a good notice *)

module R = Renderer
module B = Backend
module C = Constants

include Name_rr_d

let init stationmap cities player_idx backend =
  let name, handle = B.get_player player_idx backend |> Player.get_name_and_handle stationmap cities in
  let name = Text_entry.make name ~x:24 ~y:80 ~chars:24 in
  let handle = Text_entry.make handle ~x:24 ~y:120 ~chars:3 in
  {
    active=`First;
    name;
    handle;
  }

let nobaction = Backend.Action.NoAction

let handle_event v event =
  match v.active with
  | `First ->
      begin match Text_entry.handle_event v.name event with
      | name, `Return _ ->
        let name = Text_entry.set_writeable false name in
        [%up {v with name; active=`Second}], `Stay, nobaction
      | name, `Stay -> [%up {v with name}], `Stay, nobaction
      end
  | `Second ->
      begin match Text_entry.handle_event v.handle event with
      | handle, `Return _ -> [%up {v with handle}], `Exit, 
          B.Action.NameRR{
            player_idx=C.player;
            name=Text_entry.get_text v.name;
            handle=Text_entry.get_text handle}
      | handle, `Stay -> [%up {v with handle}], `Stay, nobaction
      end

let render win (s:State.t) v =
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  let write = Fonts.Render.write win s.fonts ~color:Ega.black ~idx:4 in
  write "Type Railroad Name..." ~x:24 ~y:64;
  Text_entry.render win s.fonts v.name;
  match v.active with
  | `Second ->
    write "Type RailRoad Handle..." ~x:24 ~y:104;
    Text_entry.render win s.fonts v.handle
  | _ -> ()
