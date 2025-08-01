open !Containers

(* Deals with all possible end game situations *)

let retire_menu fonts =
  let open Menu in
  let open MsgBox in
  make ~fonts
  [
    make_entry "No, Just Kidding." @@ `Action `Stay;
    make_entry "Yes, bye" @@ `Action `Quit;
  ]

include Endgame_d

