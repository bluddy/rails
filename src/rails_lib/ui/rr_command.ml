open Containers

module R = Renderer

(* Called by Stock_broker *)

type t = {
  src: Text_entry.t;
  dst: Text_entry.t;
  focus: [`First | `Second of Utils.loc];
}

let default =
  let x, chars = 66, 24 in
  let src = Text_entry.make "" ~x ~y:89 ~chars in
  let dst = Text_entry.make "" ~x ~y:107 ~chars ~editable:false in
  {src; dst; focus=`First}

let render win fonts v =
  (* Draw other ui first *)
  let x, y, w, h = 64, 78, 224, 26 in
  R.draw_rect win ~x ~y ~w ~h ~color:Ega.white ~fill:true;
  R.draw_rect win ~x ~y ~w ~h ~color:Ega.black ~fill:false;
  let x = 66 in
  Fonts.Render.write win fonts ~x ~y:80 ~idx:`Standard ~color:Ega.black "From city";
  Text_entry.render win fonts v.src;
  Fonts.Render.write win fonts ~x ~y:98 ~idx:`Standard ~color:Ega.black "To city";
  Text_entry.render win fonts v.dst

let handle_event event cities v =
  let textbox = match v.focus with
    | `First -> v.src
    | `Second _ -> v.dst
  in
  let process_box box =
    let box, status = Text_entry.handle_event box event in
    let status = match status with
      | `Return text ->
          if String.is_empty text then `Fail else
          begin match Cities.find_by_substr text cities with
          | None -> `Fail
          | Some (x, y, _) -> `Return (x, y)
          end
      | _ -> `Stay
    in
    box, status
  in
  let textbox, status = process_box textbox in
  let v, status = match v.focus, status with
    | `First, `Return loc -> {v with src=textbox; focus=`Second loc}, `Stay
    | `First, _ -> {v with src=textbox}, status
    | `Second loc1, `Return loc2 -> {v with dst=textbox}, `Loc (loc1, loc2)
    | `Second _, _ -> {v with dst=textbox}, `Stay
  in
  v, status


