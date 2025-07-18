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
  let ranked_owners = Stock_market.rank_owners_last_history b.stocks |> List.map snd in
  {
    last_tick=0;
    map_tex=s.map_silhouette_tex;
    year=params.year_start;
    phase=Player {end_=false};
    player_track_history;
    ai_route_history;
    player_track_idx=0;
    ai_route_idx=0;
    ranked_owners;
  }

let incr_y_8 (x ,y) = x, y + 8

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
      let x, y = Player.get_track_loc idx player |> incr_y_8 in
      let color = if idx = v.player_track_idx then Ega.yellow else Ega.white in
      R.draw_point win ~color ~x ~y;
    )
    Iter.(0 -- (v.player_track_idx - 1))
  in

  let _draw_player_stations =
    let is_end_player_or_ai = match v.phase with Done | Player {end_=true} | Ai _ -> true | _ -> false in
    let comp = if is_end_player_or_ai then (<=) else (<) in
    Station_map.iter (fun station ->
      if comp (Station.get_year_built station) v.year then (
        let x, y = Station.get_loc station |> incr_y_8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color:Ega.yellow ~fill:true
      )
    )
    b.stations
  in

  let _draw_ai_track_and_stations =
    if Ai.num_routes b.ai <= 0 then () else
    Iter.iter (fun route_idx ->
      let route = Ai.get_route route_idx b.ai in
      let src, dst = route.Ai.src, route.Ai.dst in
      let ai = Ai.ai_of_city src b.ai |> Option.get_exn_or "ai_of_city" in
      let color = Owner.Map.find ai colors in
      match v.phase with
      | Ai {track_idx} when route_idx = v.ai_route_idx ->
        Iter.iter (fun track_idx2 ->
          let x, y = route.track.(track_idx2) |> incr_y_8 in
          let color = if track_idx = track_idx2 then color else Ega.black in
          R.draw_point win ~color ~x ~y
        ) Iter.(0 -- track_idx);

        (* draw src only while drawing route *)
        let x, y = src |> incr_y_8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true
      | _ ->
        Array.iter (fun loc ->
          let x, y = incr_y_8 loc in
          R.draw_point win ~color:Ega.black ~x ~y
        ) route.track;
        let x, y = src |> incr_y_8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
        let x, y = dst |> incr_y_8 in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
    )
    Iter.(0 -- v.ai_route_idx)
  in
  let heading = match v.phase with
  | Player _ ->
      sp "a history of the %s." (B.get_name player_idx b)
  | Ai _ ->
      if Ai.num_routes b.ai <= 0 then "" else
      let route = Ai.get_route v.ai_route_idx b.ai in
      let owner = route.owner in
      let src, dst = route.Ai.src, route.dst in
      let src_s = Cities.name_of_loc src b.cities in
      let dst_s = Cities.name_of_loc dst b.cities in
      sp "%s connects %s to %s" (B.get_name owner b) src_s dst_s
  | Done ->
      "History Complete (Press key)"
  in
  write_caps ~x:8 ~y:1 heading;

  (* Draw ranked portraits *)
  begin match v.phase with
  | Done -> Fiscal_period_end.draw_owner_portraits win s (List.to_iter v.ranked_owners)
  | _ -> ()
  end

let tick_delta = 200 (* ms *)

let handle_tick (s:State.t) cur_time v =
  let next_time = v.last_tick + tick_delta  in
  if cur_time < next_time then v else
  let last_tick = cur_time in
  let b = s.backend in
  let player_idx = C.player in
  let player = B.get_player player_idx b in
  let params = b.params in
  let age = v.year - params.year_start in
  match v.phase with
  | Player {end_=false} when v.year < params.year ->
    let year_track_idx = v.player_track_history.(age) in
    if v.player_track_idx < year_track_idx then
      {v with player_track_idx=v.player_track_idx + 1; last_tick}
    else
      {v with phase=Player {end_=true}; last_tick}

  | Player {end_=false} ->
    (* last year *)
    if v.player_track_idx < (Player.get_num_track_pieces player) - 1 then
      {v with player_track_idx=v.player_track_idx + 1; last_tick}
    else
      {v with phase=Player {end_=true}; last_tick}

  | Player {end_=true} -> {v with phase=Ai {track_idx=0}; last_tick}
    (* Just to complete stations *)

  | Ai {track_idx} ->
    if Ai.num_routes b.ai <= 0 then
        if v.year < params.year then {v with phase=Player {end_=false}; last_tick}
        else {v with phase=Done; last_tick}
    else
    let route = Ai.get_route v.ai_route_idx b.ai in
    let len = Array.length route.track in
    if track_idx < len - 1 then
      (* Same track *)
      {v with phase=Ai {track_idx=track_idx + 1}; last_tick}
    else
      let year_route_idx = v.ai_route_history.(age) in
      if v.ai_route_idx < year_route_idx then
        (* Next route before year *)
        {v with ai_route_idx=v.ai_route_idx + 1; phase=Ai{track_idx=0}; last_tick}
      else if v.year < params.year then
        (* Advance year *)
        {v with year=v.year + 1; phase=Player{end_=false}; last_tick}
      else
        {v with phase=Done; last_tick}
  | Done -> {v with last_tick}

let handle_event v (s:State.t) (event:Event.t) =
  let player_idx = C.player in
  let b = s.backend in
  let player = B.get_player player_idx b in
  let exit_event = match event with
    | MouseButton {down=true; _}
    | Key {down=true; _} -> true
    | _ -> false
  in
  if not exit_event then `None, v else
  let params = b.params in
  match v.phase with
    | Done -> `Exit, v
    | _ ->
    (* Jump to end *)
    let ai_route_idx = Ai.num_routes b.ai - 1 in
    let player_track_idx = Player.get_num_track_pieces player - 1 in
    `None, {v with phase=Done; year=params.year; ai_route_idx; player_track_idx}

