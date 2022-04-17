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

let get_exn = function
  | Ok x -> x
  | Error(`Msg s) -> failwith s


module Texture = struct
  type t = {
    h: int;
    w: int;
    texture: Sdl.texture;
    surface: Sdl.surface;
    mutable ndarray: (int, Bigarray.int8_unsigned_elt) Sdl.bigarray;
    dst: Sdl.rect;
    mutable dirty_rect: bool;
  }

  let make win (arr:Pic.ndarray) =
    let h, w = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
    let ndarray = Bigarray.reshape_1 arr (w*h*4) in
    let surface =
      Sdl.create_rgb_surface_with_format_from ndarray
        ~w ~h ~depth:32 ~pitch:(w*4) Sdl.Pixel.format_abgr8888
      |> get_exn
    in
    let texture = Sdl.create_texture_from_surface win.renderer surface
      |> get_exn
    in
    let w' = zoom win w in
    let h' = zoom win h in
    let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
    { w; h; ndarray; texture; surface; dst; dirty_rect=true}

  let destroy tex =
    Sdl.destroy_texture tex.texture

    (* slowish *)
  let update (tex:t) (ndarray:Pic.ndarray) =
    let h, w = Ndarray.nth_dim ndarray 0, Ndarray.nth_dim ndarray 1 in
    let ndarray = (Bigarray.reshape_1 ndarray (w*h*4)) in
    Sdl.update_texture tex.texture None ndarray (tex.w * 4)
      |> get_exn

end

open Result.Infix

let render ?(x=0) ?(y=0) ?color win tex =
  let open Texture in
  Sdl.Rect.set_x tex.dst @@ zoom win x;
  Sdl.Rect.set_y tex.dst @@ zoom win y;
  let () = match color with
    | Some (r,g,b) ->
        Sdl.set_texture_color_mod tex.texture r g b |> get_exn
    | _ -> ()
  in
  Sdl.render_copy win.renderer tex.texture ~dst:tex.dst |> get_exn

let clear_screen win =
  Sdl.render_clear win.renderer |> get_exn

