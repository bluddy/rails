open Containers
open Tsdl

type mouse_button =
  [ `Left | `Right | `Middle | `X1 | `X2 ]
  [@@deriving enum]

module ButtonBitset = Bitset.Make(struct
  type t = mouse_button
  let of_enum = mouse_button_of_enum
  let to_enum = mouse_button_to_enum
  let last = `Middle
end)

type key =
  | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P
  | Q | R | S | T | U | V | W | X | Y | Z
  | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | Enter | Space
  | Left | Right | Up | Down

type modifier =
  [`Shift | `Ctrl | `Alt | `Lshift | `Rshift | `Lctrl | `Rctrl | `Lalt | `Ralt | `Caps]
  [@@deriving enum]

module ModifierBitset = Bitset.Make(struct
  type t = modifier
  let of_enum = modifier_of_enum
  let to_enum = modifier_to_enum
  let last = `Caps
end)

type t =
  | MouseMotion of {x: int; y: int; state: ButtonBitset.t}
  | MouseButton of
      {x: int; y: int; clicks: int; button: mouse_button; down: bool}
  | MouseWheel of {x: int; y: int}
  | Key of {repeat: int; key: key; modifiers: ModifierBitset.t; down:bool}
  | Quit
  | NoEvent

let key event (event_typ:Sdl.Event.enum) =
  let open Sdl.Event in
  let open Sdl.Kmod in
  let repeat = get event keyboard_repeat in
  let keymod = get event keyboard_keymod in
  let mod_set test v set =
    if ((test land keymod) <> 0) then ModifierBitset.add set v else set
  in
  let modifiers =
    ModifierBitset.empty
    |> mod_set lshift `Lshift
    |> mod_set rshift `Rshift
    |> mod_set lalt `Lalt
    |> mod_set ralt `Ralt
    |> mod_set lctrl `Lctrl
    |> mod_set rctrl `Rctrl
    |> mod_set ctrl `Ctrl
    |> mod_set alt `Alt
    |> mod_set shift `Shift
  in
  let down = match event_typ with
    | `Key_up -> false
    | `Key_down -> true
    | _ -> failwith "key"
  in
  let exception UnhandledKey in
  let key = Sdl.Scancode.enum (get event keyboard_keycode) in
  try
    let key =
      match key with
      | `A -> A
      | `B -> B
      | `C -> C
      | `D -> D
      | `E -> E
      | `F -> F
      | `G -> G
      | `H -> H
      | `I -> I
      | `J -> J
      | `K -> K
      | `L -> L
      | `M -> M
      | `N -> N
      | `O -> O
      | `P -> P
      | `Q -> Q
      | `R -> R
      | `S -> S
      | `T -> T
      | `U -> U
      | `V -> V
      | `W -> W
      | `X -> X
      | `Y -> Y
      | `Z -> Z
      | `F1 -> F1
      | `F2 -> F2
      | `F3 -> F3
      | `F4 -> F4
      | `F5 -> F5
      | `F6 -> F6
      | `F7 -> F7
      | `F8 -> F8
      | `F9 -> F9
      | `F10 -> F10
      | `F11 -> F11
      | `F12 -> F12
      | `Return -> Enter
      | `Space -> Space
      | `Left -> Left
      | `Right -> Right
      | `Up -> Up
      | `Down -> Down
      | _ -> raise_notrace UnhandledKey
    in
    Key {repeat; down; modifiers; key}
  with
  | UnhandledKey -> NoEvent

let mouse_motion event =
  let open Sdl.Event in
  let x = get event mouse_motion_x in
  let y = get event mouse_motion_y in
  let state = get event mouse_motion_state in
  let state =
    let mod_set test v set =
      if Int32.((test land state) <> 0l) then ButtonBitset.add set v else set
    in
    ButtonBitset.empty
    |> mod_set Sdl.Button.lmask `Left
    |> mod_set Sdl.Button.mmask `Middle
    |> mod_set Sdl.Button.rmask `Right
    |> mod_set Sdl.Button.x1mask `X1
    |> mod_set Sdl.Button.x2mask `X2
  in
  MouseMotion{x; y; state}

let mouse_button event (event_typ:Sdl.Event.enum) =
  let open Sdl.Event in
  let x = get event mouse_button_x in
  let y = get event mouse_button_y in
  let clicks = get event mouse_button_clicks in
  (* let state = get event mouse_button_state in *)
  let button = get event mouse_button_button in
  let button =
    if button = Sdl.Button.left then `Left else
    if button = Sdl.Button.right then `Right else
    if button = Sdl.Button.middle then `Middle else
    if button = Sdl.Button.x1 then `X1 else
    if button = Sdl.Button.x2 then `X2 else
    failwith "Unexpected button value"
  in
  let down = match event_typ with
  | `Mouse_button_up -> false
  | `Mouse_button_down -> true
  | _ -> failwith "Mouse_button"
  in
  MouseButton {x; y; clicks; button; down}

let mouse_wheel event =
  let open Sdl.Event in
  let x = get event mouse_wheel_x in
  let y = get event mouse_wheel_y in
  let dir = get event mouse_wheel_direction in
  let x, y =
    if Stdlib.(dir = mouse_wheel_flipped) then -x, -y else x, y
  in
  MouseWheel {x; y}

let of_sdl event =
  let t = Sdl.Event.(enum (get event typ)) in
  match t with
  | `Mouse_button_up | `Mouse_button_down ->
      mouse_button event t
  | `Mouse_motion ->
      mouse_motion event
  | `Mouse_wheel ->
      mouse_wheel event
  | `Key_down | `Key_up ->
      key event t
  | `Quit -> Quit
  | _ -> NoEvent


