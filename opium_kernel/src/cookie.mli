(* This module is adapted from ocaml-cookie
  (https://github.com/ulrikstrid/ocaml-cookie/blob/master/lib/cookie.mli)
  to work with Httpaf headers. 

  Copyright (c) 2020, Ulrik Strid
  All rights reserved. *)

type header = Httpaf.Headers.name * Httpaf.Headers.value

(** parses ["Cookie: foo=bar"] into [("Cookie", "foo=bar")] *)
val header_of_string : string -> header option

(** [expires] describes when a cookie will expire.
- [`Session] - nothing will be set
- [`Max_age] - Max-Age will be set with the number
- [`Date] - Expires will be set with a date
*)
type expires =
  [ `Session
  | `Max_age of int64
  | `Date of Ptime.t
  ]

type same_site =
  [ `None
  | `Strict
  | `Lax
  ]

(** The [cookie] type is a tuple of [(name, value)] *)
type cookie = string * string

type t =
  { expires : expires
  ; scope : Uri.t
  ; same_site : same_site
  ; secure : bool
  ; http_only : bool
  ; value : string * string
  }

(** [make] creates a cookie, it will default to the following values:
- {!type:expires} - `Session
- {!type:scope} - None
- {!type:same_site} - `Lax
- [secure] - false
- [http_only] - true *)
val make
  :  ?expires:expires
  -> ?scope:Uri.t
  -> ?same_site:same_site
  -> ?secure:bool
  -> ?http_only:bool
  -> cookie
  -> t

val of_set_cookie_header : ?origin:string -> header -> t option
val to_set_cookie_header : t -> header
val to_cookie_header : ?now:Ptime.t -> ?elapsed:int64 -> ?scope:Uri.t -> t list -> header
val cookies_of_header : header -> cookie list
val values_of_string : string -> (string * string) list
