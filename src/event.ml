open Containers
open Tsdl

let typ event =
  Sdl.Event.(enum (get event typ))

type key =
  Otherkey | Escape | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 |
  Enter

let key event =
  let key = Sdl.Event.(get event keyboard_keycode) in
  if key = Sdl.K.escape then Escape else
  if key = Sdl.K.f1 then F1 else
  if key = Sdl.K.f2 then F2 else
  if key = Sdl.K.f3 then F3 else
  if key = Sdl.K.f4 then F4 else
  if key = Sdl.K.f5 then F5 else
  if key = Sdl.K.f6 then F6 else
  if key = Sdl.K.f7 then F7 else
  if key = Sdl.K.f8 then F8 else
  if key = Sdl.K.f9 then F9 else
  if key = Sdl.K.f10 then F10 else
  if key = Sdl.K.return then Enter else
  Otherkey

