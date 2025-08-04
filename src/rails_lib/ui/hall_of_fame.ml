open Containers
module C = Constants.HallOfFame
module List = Utils.List
module M = Money
module R = Renderer

let sp = Printf.sprintf

let player_idx = Constants.player

include Hall_of_fame_d

let hof_file = "hof.dat"

let make ?(bonus=M.zero) () =
  let text_entry () = EnterName(Text_entry.make "Your Name?" ~x:80 ~y:112 ~chars:24) in
  if IO.File.exists hof_file then
    let s = IO.File.read_exn hof_file in
    let entries = Yojson.Safe.from_string s |> entries_of_yojson in
    match List.find_idx (fun entry -> M.(bonus > entry.bonus)) entries with
    | Some (i, _) ->
        {mode=text_entry (); entries; idx=Some i}
    | None when List.length entries < C.max_entries ->
        {mode=text_entry (); entries; idx=List.length entries |> Option.some}
    | None ->
        {mode=Display; entries; idx=None}
  else
    {mode=text_entry (); entries=[]; idx=Some 0}

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
      Text_entry.render win fonts text_entry

  | Display ->
      R.clear_screen win;
      R.paint_screen win ~color:Ega.white;
      R.draw_rect win ~x:2 ~y:2 ~w:316 ~h:196 ~color:Ega.dgray ~fill:false;
      let write_lg = write_b ~idx:`Large in
      write_lg ~x:64 ~y:5 "RailRoad Tycoon";
      write_lg ~x:80 ~y:22 "HALL OF FAME";
      let write_cy = write ~idx:`Standard ~color:Ega.bcyan in
      let write = write_b ~idx:`Standard in

      List.fold_left (fun (i, y) entry ->
        let s = sp "%d. %s's     %s" i entry.player entry.rr_name in
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
        write_cy ~x:64 ~y:101 s;

        (match v.idx with
        | Some j when i = j ->
            R.draw_rect win ~x:8 ~y:(y-2) ~w:303 ~h:25 ~color:Ega.bred ~fill:false
        | _ -> ());

        (i + 1, y + 26))
      (1, 41)
      v.entries
      |> ignore

