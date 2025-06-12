open! Containers
module C = Constants
module R = Renderer
module B = Backend

type loc = Utils.loc

type t = {
  speed: int;
  src: loc;
  dst: loc;
  train_idx: Train.Id.t;
  entry: Text_entry.t;
}

let make speed ~src ~dst train_idx trains stations cities = 
  let names = ["Zephyr"; "Cannonball"; "Rocket"; "Special"] in
  let name_idx = Train.Id.to_int train_idx mod List.length names in
  let super_name = List.nth names name_idx in
  let train = Trainmap.get train_idx trains in
  let city_name =
    let loc = Train.get_dest train in
    let station = Station_map.get_exn loc stations in
    let x, y = Station.get_city station |> Option.get_exn_or "missing city of station" in
    Cities.get_name x y cities in
  let name = city_name ^ " " ^ super_name in
  let entry = Text_entry.make name ~x:80 ~y:112 ~chars:24 in
  {
    speed;
    src;
    dst;
    train_idx;
    entry;
  }

let handle_event v event =
  match Text_entry.handle_event v.entry event with
  | entry, `Return name ->
    [%up {v with entry}], `Exit, B.Action.NameTrain{player_idx=C.player; train=v.train_idx; name}
  | entry, `Stay ->
    [%up {v with entry}], `Stay, B.Action.NoAction

let station_name loc s = (B.get_station loc s.State.backend |> Option.get_exn_or "oops "|> Station.get_name)

let render win (s:State.t) v =
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  Fonts.Render.write_shadow win s.fonts ~idx:2 ~color:Ega.bred ~x:24 ~y:8 "New Speed Record!";
  let text =
    Printf.sprintf
    "%s to %s\n\
     at %d miles per hour."
    (station_name v.src s)
    (station_name v.dst s)
    v.speed
  in
  (* TODO: side train *)
  let train = B.get_train v.train_idx C.player s.backend in
  Fonts.Render.write win s.fonts ~idx:4 ~color:Ega.black ~x:24 ~y:40 text;
  Fonts.Render.write win s.fonts ~idx:4 ~color:Ega.black ~x:80 ~y:100 "Train Name?";
  Text_entry.render win s.fonts v.entry

