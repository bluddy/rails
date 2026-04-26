open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Engine.Renderer
module Random = Utils.Random
module Event = Engine.Event
module Mainloop = Engine.Mainloop
open Utils.Infix

type module_t =
  | Intro
  | StartMenu
  | Investigate
  | Driving
  | Hacking
  | Crypto
  | Break_in

(* All state *)
type t = {
  (* saveable *)
  mode: module_t;

  (* non-saveable *)
  srv: Services.t;
}

let default_state win sound =
  let resources = Resources.load_all () in
  let textures = Textures.of_resources win resources in
  let fonts = Fonts.load "./data/covert/FONTS.CV" win in
  {
    srv=Services.{
      resources;
      textures;
      fonts;
      win;
      sound;
      random = Random.get_state ();
    };
    mode=Intro;
  }

let handle_tick win (s:State.t) time =
  let state =
    match s.mode with
    | Intro state ->
        let state2, status = Intro.handle_tick time state in
        let s =
          match status with
          | `Stay when state2 === state -> s
          | `Stay -> {s with mode = Intro state2}
          | `Exit -> {s with mode=Menu(Start_menu.default s)}
        in
        s, `Stay

  in
  state

let handle_event win (s:State.t) (event:Event.t) time =
  (* Handle an input event, starting with the UI.
     (Store the win in closure so we can create the full game state when needed 
   *)
  let state, quit =
    match s.mode with
    | Intro state ->
        begin match Intro.handle_event event state with
        | state2, `Stay when state2 === state -> s, `Stay
        | state2, `Stay -> {s with mode=Intro state2}, `Stay
        | _, `Exit -> {s with mode=Menu(Start_menu.default s)}, `Stay
        end
  in
  state, quit

let render win (s:State.t) = match s.mode with
  | Intro state -> Intro.render win state 


let run ?load ~zoom ~adjust_ar ~shader () : unit =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);

  Printf.printf "Loading resources...";
  print_newline ();

  let init_fn win =

    let sound = Sound.init () in
    let state = match load with
      | Some slot ->
          Printf.printf "Loading from slot %d...\n" slot;
          Load_game.load_game slot win sound

      | None ->
        (* New game. Use a basic default state *)
        let s = default_state win sound in
        let state = Intro.make s in
        {s with mode=Intro state}
    in
    Printf.printf " done.\n";

    state, Mainloop.{
      handle_event=handle_event win;
      handle_tick=handle_tick win;
      render=render win;
    }
  in
  let shader_file = Printf.sprintf "shaders/%s.glsl" shader in
  Mainloop.main ~zoom ~adjust_ar init_fn ~shader_file

