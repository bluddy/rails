open !Containers
module R = Renderer
module C = Constants
open Utils.Infix

(* Deals with all possible end game situations *)

let retire_menu fonts =
  let open Menu in
  let open MsgBox in
  let heading = "Are you sure you want\nto end the game?" in
  make ~heading ~fonts ~font_idx:`Standard ~x:100 ~y:60
  [
    make_entry "No, Just Kidding." @@ `Action `DontQuit;
    make_entry "Yes, bye" @@ `Action `Quit;
  ]

include Endgame_d

let is_fired = function `Fired -> true | `FinishRun | `RetireEarly -> false
let is_final = function `Fired | `FinishRun -> true | `RetireEarly -> false

let make kind s =
  let fired = is_fired kind in
  let state = Job_offer.create_retire ~fired s in
  {mode=JobOffer {state; menu=None}; kind}

let render_ad win (s:State.t) =
  let tex = Hashtbl.find s.textures.misc `Advert in
  R.Texture.render win tex ~x:0 ~y:0

let render win v (s:State.t) = match v.mode with
  | JobOffer {state; menu} ->
      Job_offer.render state win s;
      Option.iter (fun menu ->
        Menu.MsgBox.render win s menu
      ) menu
  | RetirementBonus {render_fn} -> render_fn win s
  | HallOfFame hall -> Hall_of_fame.render win s hall
  | Advert {render_fn} -> render_fn win s

let handle_event (s:State.t) v event time = match v.mode with
  | JobOffer {menu=None; _} when Event.modal_dismiss event ->
      let fired = is_fired v.kind in
      let render_fn =
        let state = Retirement_bonus.make ~fired C.player s.backend in
        Retirement_bonus.render state in
      `Stay, {v with mode=RetirementBonus {render_fn}}

  | RetirementBonus _ when Event.modal_dismiss event ->
      let fired = is_fired v.kind in
      if is_final v.kind then
        let state = Hall_of_fame.make ~fired () in
        `Stay, {v with mode=HallOfFame state}
      else
        let state = Job_offer.create_retire ~fired s in
        let menu = retire_menu s.fonts |> Menu.MsgBox.do_open_menu s in
        `Stay, {v with mode=JobOffer{state; menu=Some menu}}

  | JobOffer {menu=Some menu; state} ->
      let menu2, action = Menu.MsgBox.handle_event s menu event time in
      begin match action with
      | Menu.On(`DontQuit) ->
          `Exit, v (* exit menu but stay in game *)
      | Menu.On(`Quit) ->
          (* Go on with retirement *)
          let fired = is_fired v.kind in
          let state = Hall_of_fame.make ~fired () in
          `Stay, {v with mode=HallOfFame state}
      | _ when menu2 === menu -> `Stay, v
      | _ ->
          `Stay, {v with mode=JobOffer{menu=Some menu2; state}}
      end

  | HallOfFame state ->
      let ret, state2 = Hall_of_fame.handle_event event s state in
      begin match ret with
      | `Stay when state2 === state -> `Stay, v
      | `Stay -> `Stay, {v with mode=HallOfFame state2}
      | `Exit -> `Stay, {v with mode=Advert{render_fn=render_ad}}
      end

  | Advert _ when Event.modal_dismiss event -> `QuitGame, v

  | _ -> `Stay, v

