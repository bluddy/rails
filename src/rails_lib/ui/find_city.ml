open Containers

module R = Renderer

type t = Text_entry.t

let init () =
  Text_entry.make "" ~x:66 ~y:89 ~chars:24

let render win fonts v =
  (* Draw mainui first *)
  R.draw_rect win ~x:64 ~y:78 ~w:224 ~h:26 ~color:Ega.black ~fill:false;
  R.draw_rect win ~x:65 ~y:79 ~w:222 ~h:24 ~color:Ega.white ~fill:true;
  Fonts.Render.write win fonts ~x:66 ~y:80 ~idx:`Standard ~color:Ega.black "Where in the heck is ... (city name)";
  Text_entry.render win fonts v

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


