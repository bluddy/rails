open! Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Engine.Renderer
module Random = Utils.Random
module Event = Engine.Event
module Mainloop = Engine.Mainloop
module Sound = Engine.Sound
open Utils.Infix

type module_t =
  | Intro of Intro.t
  | Start_menu of Start_menu.t
  | Briefing of Briefing.t
  | Time_pass
  (*
  | Investigate
  | Driving
  | Hacking
  | Crypto
  | Break_in
  *)

(* All state *)
type t = {
  (* saveable *)
  mode: module_t;
  next_modes: module_t list;

  (* non-saveable *)
  srv: Services.t;
}

let next_mode v = match v.next_modes with
  | x::xs -> {v with mode=x; next_modes=xs}
  | [] -> v

let set_modes v l = match l with
  | x::xs -> {v with mode=x; next_modes=xs}
  | [] -> v

let default_state win sound =
  let resources = Resources.load_all () in
  let textures = Textures.of_resources win resources in
  let fonts = Fonts.load "./data/covert/FONTS.CV" win in
  let srv = Services.{
      resources;
      textures;
      fonts;
      win;
      sound;
      random = Random.get_state ();
    }
  in
  let mode = Intro (Intro.make srv) in
  let next_modes = [Start_menu(Start_menu.create srv)] in
  {
    srv; mode; next_modes;
  }

let handle_event _win v (event:Event.t) time =
  (* Handle an input event, starting with the UI.
     (Store the win in closure so we can create the full game state when needed 
   *)
  let state, quit =
    match v.mode with
    | Intro state ->
        let state2, status = if Event.is_tick event then
          Intro.handle_tick time state
        else
          Intro.handle_event event state
        in
        let s = match status with
          | `Stay when state2 === state -> v
          | `Stay -> {v with mode=Intro state2}
          | `Exit -> next_mode v
        in
        s, `Stay
    | Start_menu state ->
        let state2, status = Start_menu.handle_event v.srv event time state in
        let v = if state2 =!= state then {v with mode=Start_menu state2} else v in
        begin match status with
        | `Activate info ->
            let world = World.default info in
            let case = Case_init.create v.srv world
              |> Case_init.step_and_recreate_if_needed v.srv world
              |> Case_init.update_events_roles_agents v.srv world
            in
            let known_org = Org.S.find_one_known (Case.G.orgs case) |> Option.map (Org.S.get_name @@ Case.G.orgs case) in
            let case = case
              |> Case_init.create_known_hqs
              |> Case_init.create_red_herrings v.srv
            in
            let modes =
              let open Briefing in
              [
                Briefing(create v.srv case Crime_start);
                Briefing(create v.srv case @@ Crime_region_info(known_org));
                Briefing(create v.srv case Crime_first_clues);
                (* Briefing(Briefing.create v.srv case world Briefing.Crime_step_start) Not done on crime start *)
              ] in
            set_modes v modes, `Stay
        | `Stay -> v, `Stay
        | `Exit -> v, `Stay
        end
    | Briefing state ->
        let state2, status = Briefing.handle_event event time state in
        let v = if state2 =!= state then {v with mode=Briefing state2} else v in
        begin match status with
        | `Stay -> v, `Stay
        | `Exit -> next_mode v, `Stay
        end

  in
  state, quit

let render win v = match v.mode with
  | Intro state -> Intro.render win state
  | Start_menu state -> Start_menu.render v.srv state
  | Briefing state -> Briefing.render win state

let run ?load ~zoom ~adjust_ar ~audio ~shader () : unit =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);

  Printf.printf "Loading resources...";
  print_newline ();

  let init_fn win =

    let sound = if audio then Sound.init () else Sound.default in
    let state = match load with
      | Some slot ->
          Printf.printf "Loading from slot %d...\n" slot;
          (* Load_game.load_game slot win sound *)
          let s = default_state win sound in
          s

      | None ->
          (* New game. Use a basic default state *)
          let s = default_state win sound in
          s
    in
    Printf.printf " done.\n";

    state, Mainloop.{
      handle_event=handle_event win;
      handle_tick=(fun v time -> handle_event win v Event.Tick time);
      render=render win;
    }
  in
  let shader_file = Printf.sprintf "shaders/%s.glsl" shader in
  Mainloop.main ~zoom ~adjust_ar init_fn ~shader_file

