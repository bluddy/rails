open! Containers

module R = Engine.Renderer
module Sound = Engine.Sound
module Event = Engine.Event
module Pani_render = Engine.Pani_render
module Ega = Engine.Ega

type mode =
  | Crime_start
  | Crime_step_start
  | Crime_region_info
  | Crime_first_clues
  [@@deriving yojson]

type text_src =
  | Pattern of {text: string; pattern: string}
  | Pages of string list

type t = {
  mode: mode;
  pani: Engine.Pani_render.t;
  text: text_src;
  case: Case.t;
  srv: Services.t;
}

let get_text_ (srv:Services.t) res pat =
  let plot_txt = Hashtbl.find srv.resources.text res in
  Subst_engine.get_lines ~pat plot_txt
  |> Option.map (Utils.add_newlines ~width:40)

let briefing_create ?(input=[0,4]) (s:Services.t) =
  Sound.pani_create s.sound "data/covert/BRIEFING.PAN" ~input

let next_page v = match v.text with
  | Pattern p ->
    let len = String.length p.pattern in
    let pattern = String.mapi (fun i c ->
      if i=len-1 then int_of_char c + 1 |> char_of_int
      else c)
      p.pattern in
    begin match get_text_ v.srv `Plot pattern with
    | Some text -> {v with text=Pattern {pattern; text}}, `Stay
    | None -> v, `Exit
    end
  | Pages (_::((_::_) as xs)) -> {v with text=Pages(xs)}, `Stay
  | Pages _ -> v, `Exit

let create (s:Services.t) (case:Case.t) mode =
  match mode with
  | Crime_start ->
      let pani = briefing_create s in
      let page, plot_num = 0, 9 in
      let pattern = Printf.sprintf "*PL%02d%d%d" (Crime.Id.to_int case.crime) plot_num page in
      let text = get_text_ s `Plot pattern
        |> Option.get_exn_or @@ Printf.sprintf "missing text %s" pattern in
      { case; pani; srv=s; mode; text=Pattern{text; pattern}; }
  | Crime_step_start ->
      let pani = briefing_create s in
      let letter = if Case.failed_other_steps case then 'A' else 'a' in
      let pattern = Printf.sprintf "*PL%02d%d%c" (Crime.Id.to_int case.crime) (Crime.Step.Id.to_int case.step) letter in
      let text = get_text_ s `Plot pattern
        |> Option.get_exn_or @@ Printf.sprintf "missing text %s" pattern in
      { case; pani; srv=s; mode; text=Pattern{text; pattern}; }
  | Crime_region_info ->
      let pani = briefing_create s in
      let org_name = Org.Map.find_pred (fun _ org -> org.Org.known_involved) case.orgs
          |> Option.map (fun org_id -> (Org.Map.find org_id case.orgs).name)
      in
      let text = Printf.sprintf
        "We have indications that\n\
        an operation is in preparation somehwere in %s"
          (Region.show case.region)
      in
      let text = match org_name with
      | Some org_s -> Printf.sprintf "%s\nThe %s is known to be involved." text org_s
      | None -> text
      in
      let has_double_agents = Loc.Set.not_empty case.double_agents in
      let text2 = "Your mission is to prevent this operation from succeeding\n\
                  and to capture as many of the participants as possible."
      in
      let text2 = if has_double_agents then
                    text2 ^ "\n\
                    Caution, one or more Double Agents are believed to be active\n\
                    within the CIA."
                  else
                    text2
      in
      {case; pani; srv=s; mode; text=Pages [text; text2]}
  | Crime_first_clues ->
    let pani = briefing_create ~input:[0,1] s in
    let text = "Here is some info we've picked up over the last few days.\n\
                We think it's linked to this operation..."
    in
    {case; pani; srv=s; mode; text=Pages [text]}

let render win v =
  Pani_render.render win v.pani;
  let text = match v.text with
    | Pattern {text; _} -> text
    | Pages (x::_) -> x
    | _ -> ""
  in
  Fonts.Render.write win v.srv.fonts ~tight:true ~color:Ega.white ~idx:`Large ~x:12 ~y:152 text

let handle_event event time v =
  match event with
  | Event.Tick ->
      let pani, _ = Pani_render.handle_tick time v.pani in
      [%up {v with pani}], `Stay
  | _ when Event.modal_dismiss event -> next_page v
  | _ -> v, `Stay

