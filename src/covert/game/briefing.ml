open! Containers

module R = Engine.Renderer
module Sound = Engine.Sound
module Event = Engine.Event
module Pani_render = Engine.Pani_render
module Ega = Engine.Ega

type mode =
  | Crime_start
  | Crime_step_start
  [@@deriving yojson]

type t = {
  mode: mode;
  pani: Engine.Pani_render.t;
  text: string;
  case: Case.t;
  world: World.t;
  srv: Services.t;
  plot_num: int;
  page: int; (* page of text *)
}

let get_text (srv:Services.t) res ~num ~crime ~page =
  let plot_txt = Hashtbl.find srv.resources.text res in
  let pat = Printf.sprintf "*PL%02d%d%d"
    (Crime.Id.to_int crime)
    num
    page
  in
  Subst_engine.get_lines ~pat plot_txt
  |> Option.map (Utils.add_newlines 40)

let next_page v =
  let page = v.page + 1 in
  match get_text v.srv `Plot ~num:v.plot_num ~crime:v.case.crime ~page with
  | Some text -> {v with page; text}, `Stay
  | None -> v, `Exit

let create (s:Services.t) (case:Case.t) world mode =
  match mode with
  | Crime_start ->
      let pani = Sound.pani_create s.sound "data/covert/BRIEFING.PAN" ~input:[0,4] in
      let page, plot_num = 0, 9 in
      let text = get_text s `Plot ~num:plot_num ~crime:case.crime ~page:0
        |> Option.get_exn_or "missing text"
      in
    {
      case;
      pani;
      world;
      srv=s;
      mode;
      text;
      page;
      plot_num;
    }

let render win v =
  Pani_render.render win v.pani;
  Fonts.Render.write win v.srv.fonts ~tight:true ~color:Ega.white ~idx:`Large ~x:12 ~y:152 v.text

let handle_event event time v =
  match event with
  | Event.Tick ->
      let pani, _ = Pani_render.handle_tick time v.pani in
      [%up {v with pani}], `Stay
  | _ when Event.modal_dismiss event -> next_page v
  | _ -> v, `Stay

