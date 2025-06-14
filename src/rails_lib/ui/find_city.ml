open Containers

module R = Renderer

type t = Text_entry.t

let init () =
  Text_entry.make "" ~x:66 ~y:89 ~chars:24

let render win fonts v =
  (* Draw mainui first *)
  R.draw_rect win ~x:64 ~y:78 ~w:224 ~h:26 ~color:Ega.white ~fill:true;
  R.draw_rect win ~x:64 ~y:78 ~w:224 ~h:26 ~color:Ega.black ~fill:false;
  Fonts.Render.write win fonts ~x:66 ~y:80 ~idx:4 ~color:Ega.black "Where in the heck is ... (city name)";
  Text_entry.render win fonts v

let _find_city text cities =
   Cities.to_list cities
    |> List.map (fun (x, y, name) -> x, y, String.lowercase_ascii name)
    |> List.sort (fun (_, _, name1) (_, _, name2) -> String.compare name1 name2)
    |> List.find_opt (fun (_, _, name) -> String.prefix ~pre:text name)

let handle_event event cities v =
  let state, status = Text_entry.handle_event v event in
  let status = match status with
    | `Return text ->
        let text = text
          |> String.lowercase_ascii
          |> String.rdrop_while (function ' ' -> true | _ -> false)
        in
        if String.length text = 0 then `Fail else
        begin match _find_city text cities with
        | None -> `Fail
        | Some (x, y, _) -> `Return (x, y)
        end
    | _ -> `Stay
  in
  state, status


