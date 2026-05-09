open! Containers

module R = Engine.Renderer
module Sound = Engine.Sound
module Event = Engine.Event
module Pani_render = Engine.Pani_render

type mode =
  | Case_start

type t = {
  mode: mode;
  pani: Engine.Pani_render.t;
  text: string;
  case: Case.t;
  world: World.t;
  srv: Services.t;
}

let create (s:Services.t) case world mode =
  match mode with
  | Case_start ->
      let pani = Sound.pani_create s.sound "data/covert/BRIEFING.PAN" ~input:[0,4] in
    let plot_txt = Hashtbl.find s.resources.text `Plot in
    let pat = Printf.sprintf "*PL%d90" (Crime.Id.to_int case.Case.crime_choice) in
    let text = Subst_engine.get_lines ~pat plot_txt |> Option.get_exn_or "oops" in
    {
      case;
      pani;
      world;
      srv=s;
      mode;
      text;
    }

let render win v =
  Pani_render.render win v.pani

let handle_event event time v =
  let state, status = match event with
  | Event.Tick -> Pani_render.handle_tick time v.pani
  | _ -> Pani_render.handle_event event v.pani
  in
  [%up {v with pani=state}], status

