open! Containers

type t = {
  case: Case.t;
  world: World.t;
  srv: Services.t;
}

let create srv case world =
  {
    case;
    world;
    srv
  }

let render win s = ()

let handle_event v event time = ()
