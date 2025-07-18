(* Drawing of newspaper, both simple and fancy *)

open! Containers
module R = Renderer
module C = Constants
module Random = Utils.Random

include Newspaper_d


let make_simple (s:State.t) kind ?heading text opponent = Simple {
    opponent;
    kind;
    msgbox=Menu.MsgBox.make_basic ~x:58 ~y:98 ?heading ~fonts:s.fonts s text;
}

let day_of_year time = ((time * 3) / 17) mod 360

let make_fancy (s:State.t) text params =
  let year, time = params.Params.year, params.time in
  let cost = ((year - C.newspaper_cost_ref_year) / 40) * 5 in
  (* Note: for some reason, the day computation here isn't the same as it is in the rest of the game. Why? *)
  let day_of_year = day_of_year time in
  let source = match day_of_year mod 4 with
    | 0 -> Fancy.RailNewsWeekly
    | 1 -> DailyTattler
    | 2 when Region.is_us params.region -> NewYorkTimes
    | 2 -> LondonTimes
    | 3 when Region.is_us params.region -> WallStreetJournal
    | 3 -> HeraldTribune
    | _ -> assert false
  in
  let date_s =
    let month_s = (day_of_year / 30) mod 12 |> Utils.str_of_month in
    let day_of_month = (day_of_year mod 30) + 1 in
    let year = day_of_year / 360 + year in
    Printf.sprintf "%s %d, %d" month_s day_of_month year
  in
  let tear_vals =
    let rec loop x acc =
      if x >= C.screen_width then acc else
      let vals = (Random.int 2 s.random, Random.int 2 s.random) in
      loop (x + 20) @@ vals::acc
    in loop 0 []
  in
  Fancy {
    source;
    text;
    msgbox=Menu.MsgBox.make_basic ~x:58 ~y:98 ~fonts:s.fonts s "Press any key to continue";
    cost_s=Printf.sprintf "%d cents" cost;
    date_s=date_s;
    tear_vals;
  }

let render_simple win (s:State.t) v =
  R.draw_rect win ~x:56 ~y:46 ~w:183 ~h:107 ~color:Ega.white ~fill:true;
  R.draw_rect win ~x:56 ~y:46 ~w:183 ~h:107 ~color:Ega.black ~fill:false;
  Fonts.Render.write_shadow win s.fonts ~color:Ega.gray ~x:62 ~y:54 ~idx:`Large @@
    Simple.show_kind v.Simple.kind;
  let tex = match v.opponent with
    | Some opponent ->
      Hashtbl.find s.textures.opponents opponent
    | None ->
      Hashtbl.find s.textures.misc `Newspaper
  in
  R.Texture.render win ~x:194 ~y:50 tex;
  Menu.MsgBox.render win s v.msgbox;
  ()

let render_fancy win (s:State.t) v =
  let white, black = Ega.white, Ega.black in
  R.draw_rect win ~x:0 ~y:0 ~w:320 ~h:100 ~color:white ~fill:true;
  R.draw_line win ~x1:1 ~y1:1 ~x2:318 ~y2:1 ~color:black;
  let name, (x, y) = Fancy.show_source v.Fancy.source, Fancy.source_pos v.source in
  Fonts.Render.write win s.fonts ~color:black ~x ~y ~idx:`Old name;
  R.draw_line win ~x1:0 ~y1:32 ~x2:319 ~y2:32 ~color:black;
  R.draw_line win ~x1:1 ~y1:1 ~x2:1 ~y2:32 ~color:black;
  R.draw_line win ~x1:1 ~y1:1 ~x2:1 ~y2:32 ~color:black;
  R.draw_line win ~x1:318 ~y1:1 ~x2:318 ~y2:32 ~color:black;
  R.draw_line win ~x1:0 ~y1:97 ~x2:319 ~y2:97 ~color:black;
  let paper_tear_tex = List.map (fun name -> Hashtbl.find s.textures.misc name) [`PaperTear1; `PaperTear2] in
  let _draw_tear =
    ignore @@
    List.fold_left (fun x (i, j) ->
      let tex = List.nth paper_tear_tex i in
      let y = 100 - j in
      R.Texture.render win ~x ~y tex;
      (x + 20))
    0
    v.tear_vals
  in
  let txt1, txt2, txt3 = v.text in
  Fonts.Render.write win s.fonts ~color:black ~x:16 ~y:40 ~idx:`Large txt1;
  Fonts.Render.write win s.fonts ~color:black ~x:16 ~y:57 ~idx:`Large txt2;
  Fonts.Render.write win s.fonts ~color:black ~x:16 ~y:74 ~idx:`Large txt3;
  Fonts.Render.write win s.fonts ~color:black ~x:8 ~y:24 ~idx:`Standard v.date_s;
  Fonts.Render.write win s.fonts ~color:black ~x:272 ~y:24 ~idx:`Standard v.cost_s;
  Menu.MsgBox.render win s v.msgbox;
  ()

let render win (s:State.t) v =
  match v with
  | Simple x -> render_simple win s x
  | Fancy x -> render_fancy win s x

let handle_event s v event =
  let msgbox = match v with Fancy x -> x.msgbox | Simple x -> x.msgbox in
  begin match Menu.modal_handle_event ~is_msgbox:true s msgbox event with
  | `Exit -> `Exit
  | _ -> `Stay
  end

