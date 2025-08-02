open !Containers

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
  JobOffer{state=Job_offer.create_retire s; menu=None}

let render state win (s:State.t) = match state with
  | JobOffer {state; menu} ->
      Job_offer.render state win s;
      Option.iter (fun menu ->
        Menu.MsgBox.render win s menu
      ) menu
  | RetirementBonus render_fn -> render_fn win s
  | HallOfFame hall -> Hall_of_fame.render win hall s

let handle_event event s v = match v.mode with
  | JobOffer {menu=None; state} when Event.key_modal_dismiss event ->
      let menu = retire_menu fonts |> Menu.MsgBox.do_open_menu s in
      {v with mode=JobOffer{state; menu=Some menu}}
  | JobOffer {menu=Some menu; state} ->
      let menu2, action = Menu.MsgBox.update s menu event in
      begin match action with
      | Menu.On(Some `DontQuit) -> `Exit, v
      | Menu.On(Some `Quit) ->
          `Stay, {v with mode=RetirementBonus}
      | _ when menu2 === menu -> `Stay, v
      | _ -> `Stay, {v with mode=JobOffer{menu=menu2; state}}
      end
  | RetirementBonus _ when Event.key_modal_dismiss event ->
      {v with HallOfFame}
  | HallOfFame state -> Hall_of_fame.handle_event event s state

