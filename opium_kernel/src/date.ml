(* This module is adapted from ocaml-cookie
  (https://github.com/ulrikstrid/ocaml-cookie/blob/master/lib/date.ml)
  to work with Httpaf headers. 

  Copyright (c) 2020, Ulrik Strid
  All rights reserved. *)

type date_time = Ptime.date * Ptime.time

let int_of_month month =
  match String.lowercase_ascii month with
  | "jan" -> 1
  | "feb" -> 2
  | "mar" -> 3
  | "apr" -> 4
  | "may" -> 5
  | "jun" -> 6
  | "jul" -> 7
  | "aug" -> 8
  | "sep" -> 9
  | "oct" -> 10
  | "nov" -> 11
  | "dec" -> 12
  | _ -> raise (Invalid_argument "The month is not valid")
;;

let month_of_int = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> raise (Invalid_argument "The month must be an integer between 1 and 12")
;;

type time_zone =
  | GMT
  | PST
  | PDT
  | MDT
  | CDT
  | EDT

let time_zone_of_string = function
  | "" | "Z" | "GMT" | "UTC" | "UT" -> GMT
  | "PST" -> PST
  | "MST" | "PDT" -> PDT
  | "CST" | "MDT" -> MDT
  | "EST" | "CDT" -> CDT
  | "EDT" -> EDT
  | _ -> GMT
;;

let int_of_time_zone = function
  | GMT -> 0
  | PST -> -480
  | PDT -> -420
  | MDT | CDT -> -300
  | EDT -> -240
;;

let parse_exn s =
  try
    Scanf.sscanf
      s
      "%3s, %d %s %4d %d:%d:%d %s"
      (fun _wday day month year hour minute second time_zone ->
        let month = int_of_month month in
        let time_zone = time_zone_of_string time_zone |> int_of_time_zone in
        let date = year, month, day in
        let time = (hour, minute, second), time_zone in
        date, time)
  with
  | Scanf.Scan_failure e -> raise (Invalid_argument e)
  | Not_found -> raise (Invalid_argument "Invalid date string")
;;

let parse s =
  try Some (parse_exn s) with
  | Invalid_argument _ -> None
;;

type weekday =
  [ `Fri
  | `Mon
  | `Sat
  | `Sun
  | `Thu
  | `Tue
  | `Wed
  ]

let string_of_weekday (weekday : weekday) =
  match weekday with
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"
  | `Sun -> "Sun"
;;

let serialize date_time =
  let ptime = Ptime.of_date_time date_time |> Option.get in
  let weekday = Ptime.weekday ptime |> string_of_weekday in
  let (year, month, day), ((hour, minute, second), _) = date_time in
  Printf.sprintf
    "%3s, %d %s %4d %d:%d:%d GMT"
    weekday
    day
    (month_of_int month)
    year
    hour
    minute
    second
;;
