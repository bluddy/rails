open Tsdl
open Containers

module R = Renderer
module C = Constants
module List = Utils.List

type state = [`Timeout | `Done | `Error]

type t = {
  mutable state: state;
  interp: Pani_interp.t;
  mutable last_time: int;
  mutable textures: R.Texture.t option array;
}

let create ?input filename =
  let stream = Pani.stream_of_file filename in
  let interp = Pani.of_stream ?input stream in
  let textures = [||] in
  let state = `Timeout in
  let last_time = 0 in
  {state; interp; last_time; textures}


let render win v =
  let no_textures = Array.length v.textures = 0 in
  if no_textures then (
    let textures = Array.map (Option.map (R.Texture.make win)) v.interp.pics in
    v.textures <- textures
  );

  R.clear_screen win;

  (* Draw all backgrounds in correct order *)
  let () = List.rev_iter (fun Pani_interp.{x; y; pic_idx} ->
      match v.textures.(pic_idx) with
      | None -> failwith @@ Printf.sprintf "No texture %d" pic_idx
      | Some tex -> ignore(R.Texture.render win ~x ~y tex)
    )
    v.interp.backgrounds
  in

  (* Draw all textures as required by interpreter *)
  Iter.iter (fun i ->
    Pani_interp.anim_get_pic v.interp i
    |> Option.iter (fun pic_idx ->
       match v.textures.(pic_idx) with
       | None -> failwith @@ Printf.sprintf "No pic_idx %d" pic_idx
       | Some pic_tex ->
         let x, y = Pani_interp.calc_anim_xy v.interp i in
         R.Texture.render win ~x ~y pic_tex
  ))
  Iter.(0 -- C.Pani.max_num_animations)

let handle_tick time v =
  let () = match v.state with
    | `Done | `Error -> ()
    | _ ->
        if time - v.last_time > C.Pani.update_delta
        then (
          v.last_time <- time;
          v.state <- Pani_interp.step v.interp;
        )
  in
  v
  
let standalone win ~filename =
  let stream = Pani.stream_of_file filename in
  let pani_v = Pani.of_stream stream in
  let pics_tex = pani_v.pics |>
    Array.map (function
      | None -> None
      | Some ndarray -> Some (R.Texture.make win ndarray))
  in

  let last_state = ref `Timeout in
  let last_time = ref @@ Sdl.get_ticks () in
  let update_delta = 10l in

  let handle_tick () _ =
    begin match !last_state with
    | `Done | `Error -> ()
    | _ ->
        let time = Sdl.get_ticks () in
        let open Int32 in
        if time - !last_time > update_delta
        then (
          last_time := time;
          last_state := Pani_interp.step pani_v;
        )
    end;
    ()
  in

  let render () =
    (* let open Result.Infix in *)
    let () = ignore(Sdl.render_clear win.R.renderer) in

    (* Draw backgrounds *)
    let () =
      List.fold_right (fun Pani_interp.{x;y;pic_idx} _ ->
        match pics_tex.(pic_idx) with
        | None -> failwith @@ Printf.sprintf "No texture %d" pic_idx
        | Some tex ->
            ignore(R.Texture.render win ~x ~y tex)
      )
      pani_v.backgrounds
      ()
    in
    (* Draw all pictures *)
    Iter.fold (fun _acc i ->
      match Pani_interp.anim_get_pic pani_v i with
      | None -> ()
      | Some pic_idx ->
          match pics_tex.(pic_idx) with
          | None -> failwith @@ Printf.sprintf "No pic_idx %d" pic_idx
          | Some pic_tex ->
            let x, y = Pani_interp.calc_anim_xy pani_v i in
            ignore(R.Texture.render win ~x ~y pic_tex)
    )
    ()
    Iter.(0 -- 50)

  in 
  let handle_event () _ = (), false in
  ((), Mainloop.{handle_tick; render; handle_event})

