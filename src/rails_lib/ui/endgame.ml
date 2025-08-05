open !Containers
module R = Renderer

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
  JobOffer {state; menu=None}

let render state win (s:State.t) = match state with
  | JobOffer {state; menu} ->
      Job_offer.render state win s;
      Option.iter (fun menu ->
        Menu.MsgBox.render win s menu
      ) menu
  | RetirementBonus {render_fn} -> render_fn win s
  | HallOfFame hall -> Hall_of_fame.render win s hall

let handle_event event (s:State.t) v = match v.mode with
  | JobOffer {menu=None; state} when Event.key_modal_dismiss event ->
      let menu = retire_menu s.fonts |> Menu.MsgBox.do_open_menu s in
      {v with mode=JobOffer{state; menu=Some menu}}

  | JobOffer {menu=Some menu; state} ->
      let menu2, action = Menu.MsgBox.update s menu event in
      begin match action with
      | Menu.On(Some `DontQuit) ->
          `Exit, v (* stay in game *)
      | Menu.On(Some `Quit) ->
          (* Go on with quitting game *)
          let state = Retirement_bonus.make ~fired:false C.player s.backend in
          let render_fn = Retirement_bonus.render s state in
          `Stay, {v with mode=RetirementBonus {render_fn}}
      | _ when menu2 === menu -> `Stay, v
      | _ -> `Stay, {v with mode=JobOffer{menu=menu2; state}}
      end

  | RetirementBonus _ when Event.key_modal_dismiss event ->
      {v with mode=HallOfFame Hall_of_fame.make}

  | HallOfFame state -> Hall_of_fame.handle_event event s state

