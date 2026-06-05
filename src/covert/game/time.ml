open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers

type t = {
  minutes: int;
  hour_in_day: int;
  day_in_month: int;
  event_tick: int;
  months: int;
} [@@deriving yojson]

let default = {
  minutes=0;
  day_in_month=0;
  hour_in_day=0;
  event_tick= -1;
  months=0;
}

let hours_of_min minutes = minutes / 60

let hour_in_day minutes =
  let hours = hours_of_min minutes in
  hours mod 24

let should_do_tick v v' =
  (v.hour_in_day < 12 && v'.hour_in_day >= 12) || (v.day_in_month <> v'.day_in_month)

let do_tick v = {v with event_tick=v.event_tick + 1}

let update minutes v =
  let minutes = v.minutes + minutes in
  let hour_in_day = hour_in_day minutes in
  let hours = hours_of_min minutes in
  let day_in_month = hours / 24 + 1 in
  {v with minutes; hour_in_day; day_in_month}

let time_date_print v =
  let mins_in_hour = v.minutes mod 60 in
  let hours = hours_of_min v.minutes in
  let hours_in_day = v.hour_in_day in
  let hours_am_pm = ((hours_in_day + 11) mod 12) + 1 in
  let am_pm = if hours_in_day < 12 then 'A' else 'P' in
  let month_of_year = v.months mod 12 |> Utils.str_of_month in
  let day_of_month = hours / 24 + 1 in
  Printf.sprintf "%02d:%02d %cM %s %02d"
    hours_am_pm mins_in_hour am_pm month_of_year day_of_month

