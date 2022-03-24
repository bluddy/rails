open Containers

let image_dir = "./pics/"

let load all () =
  let load_ndarray s = Pic.ndarray_of_file @@ image_dir ^ s ^ ".PIC"
  in

  let ndarrays_of_filenames filenames =
    List.map (fun name ->
      Pic.ndarray_of_file @@ image_dir ^ name)
      filenames
  in

  let load_ndarray_range s (range:int) = 
    let filenames = List.Infix.(0 -- range)
      |> List.map (fun i -> Printf.sprintf "%s%d.PIC" s i)
    in
    ndarrays_of_filenames filenames
  in

  let load_ndarrays l =
    let filenames =
      List.map (fun s -> Printf.sprintf "%s.PIC" s) l
    in
    ndarrays_of_filenames filenames
  in

  let sprites = load_ndarray "SPRITES" in

  let tracks = load_ndarray "TRACKS" in

  let station = load_ndarray "STATION" in

  let jobs = load_ndarray_range "PAGE" 9 in

  let faces = load_ndarray_range "FACES" in

  let trains = (load_ndarray_range "ELOC" 3) @ (load_ndarray_range "LOCOS" 2) in

  let small_trains = load_ndarrays ["LOCOS"; "LOCOSM"] in

  let title = load_ndarray "TITLE" in

  let logos = load_ndarrays ["LOGO"; "LABS"; "TITLE"; "CREDITS2"; "ADVERT"] in

  let menu = load_ndarrays ["DIFFS"; "DIFFSP"] in

  let council = load_ndarray "COUNCIL" in

  ()


  


