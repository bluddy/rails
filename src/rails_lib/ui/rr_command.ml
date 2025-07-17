open Containers

module R = Renderer

(* Called by Stock_broker *)

type t = {
  box: Text_entry.t;
  stage: [`Src | `Dst of Utils.loc];
  ai: Owner.t;
}

let make ai =
  let box = Text_entry.make "" ~x:66 ~y:89 ~chars:24 in
  {box; stage=`Src; ai}

let render win fonts v =
  (* Draw other ui first *)
  let x, y, w, h = 64, 78, 224, 24 in
  R.draw_rect win ~x ~y ~w ~h ~color:Ega.white ~fill:true;
  R.draw_rect win ~x ~y ~w ~h ~color:Ega.black ~fill:false;
  let x = 66 in
  let str = match v.stage with
    | `Src -> "Survey route from  ... (city name)"
    | `Dst _ -> "to connect to  ... (city name)"
  in
  Fonts.Render.write win fonts ~x ~y:98 ~idx:`Standard ~color:Ega.black str;
  Text_entry.render win fonts v.box

let handle_event event cities v =
  let box, status = 
    let box, status = Text_entry.handle_event v.box event in
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
  let v, status = match v.stage, status with
    | `Src, `Return loc ->
        let v = make v.ai in
        {v with stage=`Dst loc}, `Stay
    | `Src, `Stay -> [%up{v with box}], `Stay
    | `Src, `Fail -> {v with box}, `Exit
    | `Dst loc1, `Return loc2 -> {v with box}, `Route (v.ai, loc1, loc2)
    | `Dst _, _ -> {v with box}, `Stay
  in
  v, status

