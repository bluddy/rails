open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

include History_d

let sp = Printf.sprintf

let company_colors = [|Ega.yellow; Ega.bcyan; Ega.red; Ega.bblue|]

let create (s:State.t) = 
  let player_idx = C.player in
  let b = s.backend in
  let params = b.params in
  let player = B.get_player player_idx s.backend in
  let player_track_history = Player.get_track_pieces_history player |> List.rev |> Array.of_list in
  let ai_route_history = Ai.get_route_history b.ai |> List.rev |> Array.of_list in
  {
    last_tick=0;
    map_tex=s.map_silhouette_tex;
    year=params.year_start;
    phase=Player {end_=false};
    player_track_history;
    ai_route_history;
    player_track_idx=0;
    ai_route_idx=0;
    ai_track_idx=0;
  }

let render win v (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  let player = B.get_player player_idx b in
  let params = b.params in
  R.draw_rect win ~x:0 ~y:0 ~w:256 ~h:8 ~fill:true ~color:Ega.white;
  R.draw_rect win ~x:256 ~y:0 ~w:64 ~h:200 ~fill:true ~color:Ega.yellow;
  R.Texture.render win v.map_tex ~x:0 ~y:8;

  let write = Fonts.Render.write ~idx:`Standard win fonts in
  let write = write ~color:Ega.black in
  let write_caps = Fonts.Render.write ~idx:`Caps win fonts ~color:Ega.black in

  let _draw_year =
    R.draw_rect win ~x:111 ~y:191 ~w:29 ~h:9 ~fill:true ~color:Ega.white;
    write ~x:116 ~y:192 @@ string_of_int params.year;
  in

  let owners = B.players_and_ai b in
  let colors =
    Iter.foldi (fun acc i owner -> Owner.Map.add owner company_colors.(i) acc)
      Owner.Map.empty
      owners
  in
  (* We need to draw everything from start_year to this year *)

  let _draw_player_track =
    Iter.iter (fun idx ->
      let x, y = Player.get_track_loc idx player in
      let y = y + 8 in
      R.draw_point win ~color:Ega.white ~x ~y;
    )
    Iter.(0 -- v.player_track_idx)
  in

  let _draw_player_stations =
    let is_end_player_or_ai = match v.phase with Player {end_=true} | Ai -> true | _ -> false in
    let comp = if is_end_player_or_ai then (<=) else (<) in
    Station_map.iter (fun station ->
      if comp (Station.get_year_built station) v.year then (
        let x, y = Station.get_loc station in
        let y = y + 8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color:Ega.yellow ~fill:true
      )
    )
    b.stations
  in

  let _draw_ai_track_and_stations =
    Array.iter (fun idx ->
      let src, dst = route.Ai.src, route.Ai.dst in
      let ai = Ai.ai_of_city src b.ai |> Option.get_exn_or "ai_of_city" in
      let color = Owner.Map.find ai colors in
      let route = Ai.get_route idx b.ai in
      if idx = v.ai_route_idx then (
        Iter.iter (fun idx ->
          let x, y = route.track.(idx) in
          let y = y + 8 in
          R.draw_point win ~color:Ega.black ~x ~y
        ) Iter.(0 -- (v.ai_track_idx - 1));
        (* Draw current point *)
        let x, y = route.track.(v.ai_track_idx) in
        let y = y + 8 in
        R.draw_point win ~color ~x ~y;
        (* draw src only while drawing route *)
        let x, y = src in
        let y = y + 8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
      ) else (
        Array.iter (fun (x, y) ->
          let y = y + 8 in
          R.draw_point win ~color:Ega.black ~x ~y
        ) route.track;
        let x, y = src in
        let y = y + 8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
        let x, y = dst in
        let y = y + 8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
      );
    )
    Iter.(0 -- v.ai_route_idx)
  in
  let heading = match v.phase with
  | Player _ -> sp "a history of the %s." (B.get_name player_idx b)
  | Ai {owner} ->
        let route = Ai.get_route idx b.ai in
        let src, dst = route.Ai.src, route.Ai.dst in
        let src_s = Cities.name_of_loc src b.cities in
        let dst_s = Cities.name_of_loc dst b.cities in
        sp "%s connects %s to %s" (B.get_name owner b) src_s dst_s
  in
  write_caps ~x:8 ~y:1 heading;
  ()

let tick_delta = 200 (* ms *)

let handle_tick s v cur_time =
  let next_time = v.last_tick + tick_delta  in
  if cur_time < next_time then v else
  let last_tick = cur_time in
  let b = s.backend in
  let params = b.params in
  let player_idx = C.player in
  let player = B.get_player player_idx b in
  let age = v.year - v.year_start in
  match v.phase with
  | Player {end_=false} ->
    let year_track_idx = v.player_track_history.(age) in
    if v.player_track_idx < year_track_idx then
      {v with player_track_idx=v.player_track_idx + 1; last_tick}
    else
      {v with phase=Player {end_=true}; last_tick}
  | Player _ -> Ai
  | Ai ->
    let route = Ai.get_route v.ai_route_idx b.ai in
    let len = Array.length route.track in
    if v.ai_route_idx < len - 1 then
      {v with ai_route_idx=ai_route_idx + 1; last_tick}
    else
      let year_route_idx = v.ai_route_history.(age) in
      if v.ai_route_idx < year_route_idx then
        {v with ai_route_idx=ai_route_idx + 1; last_tick}
      else if v.year < params.year then
        {v with year=v.year + 1; phase=Player{end_=false}; last_tick}
      else
        {v with phase=Done; last_tick}
  | Done -> {v with last_tick}



