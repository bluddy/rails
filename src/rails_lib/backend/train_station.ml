open Containers
module Hashtbl = Utils.Hashtbl
module T = Train
module S = Station
module C = Constants

(* Functions concerning combination of trains and stations *)

let dump_unused_cars_to_station v station_supply =
  (* dump unused goods at the station at this stage *)
  (* return time for changing cars *)
  (* TODO: clear priority route cars *)
  let stop = T.get_stop v in
  match stop.cars with
  | None -> (* No adjustment *)
      0, 0, v.cars
  | Some stop_cars ->
      let train_cars_by_good = 
        let h = Hashtbl.create 10 in
        List.iter (fun car -> Hashtbl.add_list h (T.Car.get_good car) car)
        v.cars;
        h
      in
      (* Create the new cars for the train using old cars if possible *)
      let added_cars, train_cars =
        List.fold_map (fun cnt good ->
          match Hashtbl.get train_cars_by_good good with
          | Some (car::cars) ->
              Hashtbl.replace train_cars_by_good good cars;
              cnt, car
          | _ ->
              let car = T.Car.make good in
              cnt + 1, car)
        0
        stop_cars
      in
      (* Remaining cars are added to the station supply *)
      let removed_cars =
        Hashtbl.fold (fun good cars cnt ->
          List.fold_left (fun cnt car ->
            Hashtbl.incr ~by:(T.Car.get_amount car) station_supply good;
            cnt + 1)
          cnt
          cars)
        train_cars_by_good
        0
      in
      let expense = (removed_cars - added_cars) * C.car_cost in
      let work_done = removed_cars + added_cars in
      work_done, expense, train_cars
                           

let fill_train_and_empty_station cars source cycle station_supply =
  let pickup_amount = 
    List.map (fun car ->
      let car_amount, good = T.Car.get_amount car, T.Car.get_good car in
      let station_amount = Hashtbl.get_or station_supply good ~default:0 in
      Utils.clip station_amount ~min:0 ~max:(C.car_amount - car_amount)
    )
    cars
  in
  let cars =
    List.map2 (fun (car:T.Car.t) add_amount ->
      match car.load with
      | Some load ->
        let amount = load.amount + add_amount in
        if load.amount < add_amount then
          let load = Some {T.Car.amount; source; cycle} in
          {car with load}
        else
          {car with load=Some {load with amount}}
      | None ->
          {car with load=Some {amount=add_amount; source; cycle}})
    cars
    pickup_amount
  in
  let time_pickup =
    List.fold_left2 (fun time amt car ->
      let freight = T.Car.get_freight car |> Goods.freight_to_enum in
      time + (amt * freight / 32))
      0
      pickup_amount
      cars
  in
  let () =
    List.iter2 (fun car amount ->
      Hashtbl.decr station_supply (T.Car.get_good car) ~by:amount)
    cars
    pickup_amount
  in
  time_pickup, cars
