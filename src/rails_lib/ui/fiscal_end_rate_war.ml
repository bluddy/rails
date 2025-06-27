open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

let sp = Printf.sprintf

let render msg win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:2 ~w:(320-4) ~h:(200-4) ~color:Ega.black ~fill:false;
  Fonts.Render.write win ~idx:2 ~x:108 ~y:4 ~color:Ega.black fonts "Rate War!";

  let write = Fonts.Render.write win ~idx:4 fonts in
  let write_b = write ~color:Ega.blue in
  let write_r = write ~color:Ega.red in
  let write = write ~color:Ega.black in

  let city_s = Cities.name_of_loc msg.Ui_msg.city b.cities in
  write ~x:120 ~y:20 @@ sp "in %s" city_s;

  let name = B.get_name player_idx b in
  let ai_name = B.get_name msg.ai_idx b in
  write ~x:17 ~y:27 @@ sp "%s vs %s" name ai_name;

  let y = 40 in
  write ~x:8 ~y "Traffic Picked Up...";
  let handle = B.get_handle player_idx b in
  write ~x:160 ~y handle;
  let ai_handle = B.get_handle msg.ai_idx b in
  write ~x:240 ~y ai_handle;

  let _write_picked_up =
    Array.fold (fun y freight ->
      write ~x:16 ~y @@ Freight.show freight;
      let scores = Freight.Map.get_or freight msg.pickup_scores ~default:(0, 0) in
      write_b ~x:140 ~y @@ sp "(%d)" @@ fst scores;
      let picked_up = Freight.Map.get_or freight msg.picked_up ~default:0 in
      write ~x:160 ~y @@ sp "%d tons" picked_up;
      write_r ~x:220 ~y @@ sp "(%d)" @@ snd scores;
      let ai_picked_up = Freight.Map.get_or freight msg.ai_picked_up ~default:0 in
      write ~x:240 ~y @@ sp "%d tons" ai_picked_up;
      y + 8)
      48
      Freight.all_freight
      |> ignore
  in
  if B_options.complex_economy b.params.options then (
    write ~x:8 ~y:96 "Traffic Delivered...";

    let _write_delivered =
    List.fold_left (fun y good ->
      let delivered = Goods.Set.mem good msg.delivered in
      let ai_delivered = Goods.Set.mem good msg.ai_delivered in
      if not (ai_delivered || delivered) then y + 8 else (
        write ~x:16 ~y @@ Goods.show good;
        let scores = Goods.Map.get_or good msg.delivery_scores ~default:(0, 0) in
        write_b ~x:140 ~y @@ sp "(%d)" @@ fst scores;
        write ~x:160 ~y @@ if delivered then "Delivered" else "";
        write_r ~x:220 ~y @@ sp "(%d)" @@ snd scores;
        write ~x:240 ~y @@ if ai_delivered then "Delivered" else "";
        y + 8))
      104
      Goods.order
      |> ignore
    in ()
  );

  let _write_votes =
    let score, ai_score = msg.final_scores in
    let text =
      if score = ai_score then
        sp "%s Council votes %d to %d (tied)."
        city_s (max score ai_score) (min score ai_score) 
      else
        let leader_idx = if score > ai_score then player_idx else msg.ai_idx in
        let handle = B.get_handle leader_idx b in
        sp "%s Council votes %d to %d in favor of %s"
        city_s (max score ai_score) (min score ai_score) handle
    in
    write ~x:8 ~y:168 text;
  in
  ()

  (* This should only be called once one side has won *)
let render_council msg win (s:State.t) =
  let b = s.backend in
  let fonts = s.fonts in
  let player_idx = C.player in
  let write = Fonts.Render.write win ~idx:4 fonts in
  let write_g = write ~color:Ega.gray ~tag_color:Ega.black in
  let write = write ~color:Ega.black in

  let bg_tex = Hashtbl.find s.textures.misc `Council in
  R.Texture.render win ~x:0 ~y:0 bg_tex;

  let city_s = Cities.name_of_loc msg.Ui_msg.city b.cities in
  write ~x:95 ~y:8 "Annual Meeting of the";
  write ~x:95 ~y:20 @@ city_s ^ " Town Council.";

  let score, ai_score = msg.final_scores in
  let max_score, min_score = if score > ai_score then score, ai_score else ai_score, score in
  let name =
    let winner_idx = match msg.winner with
      | `Player -> player_idx
      | `Ai -> msg.ai_idx
      | `None -> assert false
    in
    B.get_handle winner_idx b in
  let text = sp
    "The council vote\n\
    is |%d to %d|\n\
    in favor of the\n\
    |%s RailRoad.|\n\
     All other RRs\n\
    must leave town\n\
    |IMMEDIATELY!!!!|"
    max_score
    min_score
    name
  in
  write_g ~x:57 ~y:74 text

