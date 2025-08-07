open !Containers
module R = Renderer
module C = Constants
open Utils.Infix

(* Deals with all possible end game situations *)

let retire_menu fonts =
  let open Menu in
  let open MsgBox in
  make ~fonts
  [
    make_entry "No, Just Kidding." @@ `Action `DontQuit;
    make_entry "Yes, bye" @@ `Action `Quit;
  ]

include Endgame_d

let make s =
  let state = Job_offer.create_retire s in
  {mode=JobOffer {state; menu=None}; kind=`RetireEarly}

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

let handle_event event (s:State.t) v = match v.mode with
  | JobOffer {menu=None; _} when Event.key_modal_dismiss event ->
      let render_fn =
        let state = Retirement_bonus.make ~fired:false C.player s.backend in
        Retirement_bonus.render state
      in
      print_endline "joboffer none";
      `Stay, {v with mode=RetirementBonus {render_fn}}

  | RetirementBonus _ when Event.key_modal_dismiss event ->
      let state = Job_offer.create_retire s in
      let menu = retire_menu s.fonts |> Menu.MsgBox.do_open_menu s in
      `Stay, {v with mode=JobOffer{state; menu=Some menu}}

  | JobOffer {menu=Some menu; state} ->
      let menu2, action = Menu.MsgBox.update s menu event in
      begin match action with
      | Menu.On(`DontQuit) ->
          `Exit, v (* exit menu but stay in game *)
      | Menu.On(`Quit) ->
          (* Go on with retirement *)
          let state = Hall_of_fame.make ~fired:false () in
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

  | Advert _ when Event.key_modal_dismiss event -> `QuitGame, v

  | _ -> `Stay, v

