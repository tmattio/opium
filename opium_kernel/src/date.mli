(* This module is adapted from ocaml-cookie
  (https://github.com/ulrikstrid/ocaml-cookie/blob/master/lib/date.mli)
  to work with Httpaf headers. 

  Copyright (c) 2020, Ulrik Strid
  All rights reserved. *)

(** Parsing and serialization of dates that respect RFC1123 *)

type date_time = Ptime.date * Ptime.time

val parse_exn : string -> date_time
val parse : string -> date_time option
val serialize : date_time -> string
