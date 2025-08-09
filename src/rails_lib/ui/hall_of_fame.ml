open Containers
module C = Constants.HallOfFame
module List = Utils.List
module M = Money
module R = Renderer
module B = Backend

open Utils.Infix

let sp = Printf.sprintf

let player_idx = Constants.player

include Hall_of_fame_d

let hof_file = "hof.dat"

let make ?(bonus=M.zero) ~fired () =
  let text_entry () = EnterName(Text_entry.make "" ~x:80 ~y:112 ~chars:24) in
  if IO.File.exists hof_file then
    let s = IO.File.read_exn hof_file in
    let entries = Yojson.Safe.from_string s |> entries_of_yojson in
    match List.find_idx (fun entry -> M.(bonus > entry.bonus)) entries with
    | Some (i, _) ->
        {mode=text_entry (); entries; idx=Some i; fired}
    | None when List.length entries < C.max_entries ->
        {mode=text_entry (); entries; idx=List.length entries |> Option.some; fired}
    | None ->
        {mode=Display; entries; idx=None; fired}
  else
    {mode=text_entry (); entries=[]; idx=Some 0; fired}

let render win (s:State.t) v =
  let b = s.backend in
  let region = Backend.get_region b in
  let fonts = s.fonts in
  let write = Fonts.Render.write win fonts in
  let write_b = write ~color:Ega.black in

  match v.mode with
  | EnterName text_entry ->
      Ui_common.render_full_screen_frame win s.textures s.ui.dims;
      let write = write_b ~idx:`Standard in
      let write_lg = write_b ~idx:`Large in
      write ~x:80 ~y:16 "You have qualified for the";
      write_lg ~x:80 ~y:24 "Hall of Fame!";
      write ~x:80 ~y:104 "Your name?";
      Text_entry.render win fonts text_entry

  | Display ->
      R.clear_screen win;
      R.paint_screen win ~color:Ega.white;
      R.draw_rect win ~x:2 ~y:2 ~w:316 ~h:196 ~color:Ega.dgray ~fill:false;
      let write_lg = write_b ~idx:`Large in
      write_lg ~x:64 ~y:5 "RailRoad Tycoon";
      write_lg ~x:80 ~y:22 "HALL OF FAME";
      let write_cy = write ~idx:`Standard ~color:Ega.cyan in
      let write = write_b ~idx:`Standard in

      List.fold_left (fun (i, y) entry ->
        let s = sp "%d. %s's     %s" (i + 1) entry.player entry.rr_name in
        write ~x:16 ~y s;
        let s = sp 
          "%s, %s\n\
          %s/%s (%d-%d)"
          (Jobs.show entry.job)
          (Money.print ~region entry.bonus)
          (B_options.show_difficulty entry.difficulty)
          (Region.show entry.region)
          entry.year_start
          entry.year
        in
        write_cy ~x:64 ~y:(y+8) s;

        (match v.idx with
        | Some j when i = j ->
            R.draw_rect win ~x:8 ~y:(y-2) ~w:303 ~h:28 ~color:Ega.bred ~fill:false
        | _ -> ());

        (i + 1, y + 26))
      (0, 41)
      v.entries
      |> ignore

let handle_event event (s:State.t) v =
  let b = s.backend in
  let player = B.get_player player_idx b in
  let region = B.get_region b in
  let p = b.params in
  let create_entry name entries =
    let rr_name = B.get_name player_idx b in
    let job, bonus, _ = Player.job_bonus_diff_factor ~fired:v.fired b.stocks b.params player in
    let difficulty, year, year_start = p.options.difficulty, p.year, p.year_start in
    let entry = {player=name; rr_name; job; bonus; region; difficulty; year; year_start} in
    let entries = match v.idx with
      | Some i when List.length v.entries > i -> List.modify_at_idx i (fun _ -> entry) entries
      | Some _ -> entries @ [entry]
      | _ -> v.entries
    in
    entries
  in
  match v.mode with
  | EnterName text ->
      begin match Text_entry.handle_event text event with
      | text2, (`Stay | `Exit) when text2 === text ->  `Stay, v
      | text2, (`Stay | `Exit) -> `Stay, {v with mode=EnterName text2}
      | _, `Return name ->
          let entries = create_entry name v.entries in
          let entries_s = yojson_of_entries entries |> Yojson.Safe.to_string in
          ignore @@ IO.File.write hof_file entries_s;
          `Stay, {v with mode=Display; entries}
      end
  | Display when Event.key_modal_dismiss event -> `Exit, v
  | _ -> `Stay, v

