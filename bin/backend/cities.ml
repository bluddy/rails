
  type t = {
    map: (int, string * int) Utils.Hashtbl.t; (* random offset for suffix *)
    width: int;
    height: int;
  } [@@deriving yojson]

  let make map width height = {map; width; height}

  let iter f v =
    Hashtbl.iter (fun offset city_s ->
      let x, y = Utils.x_y_of_offset v.width offset in
      f x y city_s)
    v.map

  let find_exn v x y = Hashtbl.find v.map (Utils.calc_offset v.width x y)

  let find v x y = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

  let to_list v = CCHashtbl.to_list v.map
    |> List.map (fun (i, (city, _)) ->
      let x, y = Utils.x_y_of_offset v.width i in
      x, y, city)

  let find_close v x y ~range =
    Utils.scan ~range ~x ~y ~width:v.width ~height:v.height
      ~f:(fun x y -> match find v x y with Some _ -> true | None -> false)

