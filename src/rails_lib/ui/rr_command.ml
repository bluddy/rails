open Containers

module R = Renderer

(* Called by Stock_broker *)

type t = {
  src: Text_entry.t;
  dst: Text_entry.t;
  focus: [`First | `Second];
}

let init () =
  let src = Text_entry.make "" ~x:66 ~y:89 ~chars:24 in
  let dst = Text_entry.make "" ~x:66 ~y:100 ~chars:24 ~editable:false in
  {src; dst; focus=`First}

let render win fonts v =
  (* Draw mainui first *)
  R.draw_rect win ~x:64 ~y:78 ~w:224 ~h:26 ~color:Ega.white ~fill:true;
  R.draw_rect win ~x:64 ~y:78 ~w:224 ~h:26 ~color:Ega.black ~fill:false;
  Fonts.Render.write win fonts ~x:66 ~y:80 ~idx:`Standard ~color:Ega.black "From city";
  Text_entry.render win fonts v.src;
  Fonts.Render.write win fonts ~x:66 ~y:80 ~idx:`Standard ~color:Ega.black "To city";
  Text_entry.render win fonts v.dst

let handle_event event cities v =
  let state, status = Text_entry.handle_event v event in
  let status = match status with
    | `Return text ->
        if String.is_empty text then `Fail else
        begin match Cities.find_by_substr text cities with
        | None -> `Fail
        | Some (x, y, _) -> `Return (x, y)
        end
    | _ -> `Stay
  in
  state, status


