open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

  type window = {
    zoom: float;
    inner_w: int;
    inner_h: int;
    renderer: Sdl.renderer;
    window: Sdl.window;
  }

  let create w h ~zoom =
    let out_w = Int.of_float @@ zoom *. Float.of_int w in
    let out_h = Int.of_float @@ zoom *. Float.of_int h in
    let window, renderer =
      match Sdl.init Sdl.Init.video with
      | Error(`Msg e) -> Sdl.log "Init error: %s" e; exit 1
      | Ok () ->
        match Sdl.create_window_and_renderer ~w:out_w ~h:out_h Sdl.Window.opengl with
        | Error(`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok (w,r) -> w,r
    in
    { inner_w=w; inner_h=h; renderer; window; zoom }

  let zoom win x =
    win.zoom *. Float.of_int x |> Int.of_float

module Texture = struct
  type t = {
    h: int;
    w: int;
    texture: Sdl.texture;
    dst: Sdl.rect;
    mutable dirty_rect: bool;
  }

  let make win (arr:Pic.ndarray) =
    let h, w = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
    let open Result.Infix in
    let res =
      Sdl.create_rgb_surface_with_format_from (Bigarray.reshape_1 arr (w*h*4))
        ~w ~h ~depth:32 ~pitch:(w*4) Sdl.Pixel.format_abgr8888
      >>= fun surf ->
      Sdl.create_texture_from_surface win.renderer surf
    in
    let texture =
      match res with
      | Error(`Msg e) -> Sdl.log "Couldn't create texture from surface: %s" e; exit 1
      | Ok t -> t
    in
    let w' = zoom win w in
    let h' = zoom win h in
    let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
    { w; h; texture; dst; dirty_rect=true}

  let destroy tex =
    Sdl.destroy_texture tex.texture
end

let render ?(x=0) ?(y=0) win tex =
  let open Texture in
  Sdl.Rect.set_x tex.dst @@ zoom win x;
  Sdl.Rect.set_y tex.dst @@ zoom win y;
  Sdl.render_copy win.renderer tex.texture ~dst:tex.dst

let clear_screen win = Sdl.render_clear win.renderer

let error_handle = function
  | Ok x -> x
  | Error(`Msg s) -> failwith s

