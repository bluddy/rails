open! Containers

module Menu = Engine.Menu

let create (srv:Services.t) =
  let open Menu in
  let open MsgBox in
  make ~fonts:srv.fonts ~heading:"Do you want to..."
  [
    make_entry " Create a New Character" @@ `Action(`New_character);
    make_entry " Load a Saved Game" @@ `Action(`Load_game);
    make_entry " Practice a skill" @@ `Action(`Practice_skill);
    make_entry " Review Hall of Fame" @@ `Action(`Hall_of_fame);
  ]
  


