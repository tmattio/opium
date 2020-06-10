(* This module is adapted from ocaml-cookie
  (https://github.com/ulrikstrid/ocaml-cookie/blob/master/lib/cookie.ml)
  to remove dependencies and specialize it to Httpaf headers.

  Copyright (c) 2020, Ulrik Strid
  All rights reserved. *)

module Cookie_map = struct
  module Ordered_cookie = struct
    type t = int * string

    let compare (c1, s1) (c2, s2) = if String.equal s1 s2 then 0 else Int.compare c1 c2
  end

  include Map.Make (Ordered_cookie)

  let filter_value (fn : 'a -> bool) (map : 'a t) = filter (fun _key v -> fn v) map
end

module Attribute_map = struct
  include Map.Make (String)

  let key_exists ~key map = exists (fun k _ -> k = key) map
  let force_set v _ = Some v

  let keep_numbers s =
    Std.String.filter
      (fun c ->
        let code = Char.code c in
        if code = 45 || (code >= 48 && code <= 57) then true else false)
      s
  ;;

  let is_invalid_char c = c = ';' || c = '"'
  let is_valid_char c = not (is_invalid_char c)

  let set_attributes amap attr =
    match attr with
    | [] | [ ""; _ ] | [ "" ] | "" :: _ | "version" :: _ -> amap
    | [ key ] when String.lowercase_ascii key |> String.trim = "httponly" ->
      update "http_only" (force_set "") amap
    | key :: _ when String.lowercase_ascii key |> String.trim = "httponly" ->
      update "http_only" (force_set "") amap
    | [ key ] when String.lowercase_ascii key |> String.trim = "secure" ->
      update "secure" (force_set "") amap
    | key :: _ when String.lowercase_ascii key |> String.trim = "secure" ->
      update "secure" (force_set "") amap
    | key :: value when String.lowercase_ascii key |> String.trim = "path" ->
      update
        "path"
        (force_set
           (String.concat "" value |> String.trim |> Std.String.filter is_valid_char))
        amap
    | key :: value when String.lowercase_ascii key |> String.trim = "domain" ->
      let domain =
        value
        |> String.concat ""
        |> String.trim
        |> Std.String.drop ~max:1 ~sat:(( = ) '.')
        |> String.lowercase_ascii
      in
      if domain = ""
         || Std.String.is_suffix domain ~affix:"."
         || Std.String.is_prefix domain ~affix:"."
      then amap
      else update "domain" (force_set domain) amap
    | key :: value when String.lowercase_ascii key |> String.trim = "expires" ->
      let expires = String.concat "" value |> String.trim in
      update "expires" (force_set expires) amap
    | [ key; value ] when String.lowercase_ascii key = "max-age" ->
      update "max-age" (force_set (keep_numbers value)) amap
    | _ -> amap
  ;;

  let of_list attrs =
    let amap : string t = empty in
    attrs |> List.map (String.split_on_char '=') |> List.fold_left set_attributes amap
  ;;
end

type header = Rock.Headers.name * Rock.Headers.value

let header_of_string str =
  let len = String.length str in
  (* Check if the string is longer than "Cookie: "*)
  if len > 8 && String.sub str 0 8 |> String.lowercase_ascii = "cookie: "
  then
    Some ("Cookie", String.sub str 8 (len - 8))
    (* Check if the string is longer than "Set-Cookie: "*)
  else if len > 12 && String.sub str 0 12 |> String.lowercase_ascii = "set-cookie: "
  then Some ("Set-Cookie", String.sub str 12 (len - 12))
  else None
;;

type expires =
  [ `Session
  | `Max_age of int64
  | `Date of Ptime.t
  ]

let expires_of_tuple (key, value) =
  String.lowercase_ascii key
  |> function
  | "max-age" -> Some (`Max_age (Int64.of_string value))
  | "expires" ->
    Option.bind (Date.parse value) Ptime.of_date_time |> Option.map (fun e -> `Date e)
  | _ -> None
;;

type same_site =
  [ `None
  | `Strict
  | `Lax
  ]

type cookie = string * string

type t =
  { expires : expires
  ; scope : Uri.t
  ; same_site : same_site
  ; secure : bool
  ; http_only : bool
  ; value : cookie
  }

let make
    ?(expires = `Session)
    ?(scope = Uri.empty)
    ?(same_site = `Lax)
    ?(secure = false)
    ?(http_only = true)
    value
  =
  { expires; scope; same_site; secure; http_only; value }
;;

let of_set_cookie_header ?origin:_ ((_, value) : header) =
  match Std.String.cut ~sep:";" value with
  | None ->
    Option.bind (Std.String.cut value ~sep:"=") (fun (k, v) ->
        if String.trim k = "" then None else Some (make (String.trim k, String.trim v)))
  | Some (cookie, attrs) ->
    Option.bind (Std.String.cut cookie ~sep:"=") (fun (k, v) ->
        if k = ""
        then None
        else (
          let value = String.trim k, String.trim v in
          let attrs =
            String.split_on_char ';' attrs
            |> List.map String.trim
            |> Attribute_map.of_list
          in
          let expires =
            let expire_attr_opt =
              Attribute_map.find_opt "expires" attrs |> Option.map (fun v -> "expires", v)
            in
            let max_age_attr_opt =
              Attribute_map.find_opt "max-age" attrs |> Option.map (fun v -> "max-age", v)
            in
            Option.bind
              (match expire_attr_opt with
              | Some _ -> expire_attr_opt
              | None -> max_age_attr_opt)
              (fun a -> expires_of_tuple a)
          in
          let secure = Attribute_map.key_exists ~key:"secure" attrs in
          let http_only = Attribute_map.key_exists ~key:"http_only" attrs in
          let domain : string option = Attribute_map.find_opt "domain" attrs in
          let path = Attribute_map.find_opt "path" attrs in
          let scope =
            Uri.empty
            |> fun uri ->
            Uri.with_host uri domain
            |> fun uri -> Option.map (Uri.with_path uri) path |> Option.value ~default:uri
          in
          Some (make ?expires ~scope ~secure ~http_only value)))
;;

let to_set_cookie_header t =
  let v = Printf.sprintf "%s=%s" (fst t.value) (snd t.value) in
  let v =
    match Uri.path t.scope with
    | "" -> v
    | path -> Printf.sprintf "%s; Path=%s" v path
  in
  let v =
    match Uri.host t.scope with
    | None -> v
    | Some domain -> Printf.sprintf "%s; Domain=%s" v domain
  in
  let v =
    match t.expires with
    | `Date ptime ->
      Printf.sprintf "%s; Expires=%s" v (Ptime.to_date_time ptime |> Date.serialize)
    | `Max_age max -> Printf.sprintf "%s; Max-Age=%s" v (Int64.to_string max)
    | `Session -> v
  in
  let v = if t.secure then Printf.sprintf "%s; Secure" v else v in
  let v = if t.http_only then Printf.sprintf "%s; HttpOnly" v else v in
  "Set-Cookie", v
;;

let is_expired ?now t =
  match now with
  | None -> false
  | Some than ->
    (match t.expires with
    | `Date e -> Ptime.is_earlier ~than e
    | _ -> false)
;;

let is_not_expired ?now t = not (is_expired ?now t)

let is_too_old ?(elapsed = 0L) t =
  match t.expires with
  | `Max_age max_age -> if max_age <= elapsed then true else false
  | _ -> false
;;

let is_not_too_old ?(elapsed = 0L) t = not (is_too_old ~elapsed t)

let has_matching_domain ~scope t =
  match Uri.host scope, Uri.host t.scope with
  | Some domain, Some cookie_domain ->
    if String.contains cookie_domain '.'
       && (Std.String.is_suffix domain ~affix:cookie_domain || domain = cookie_domain)
    then true
    else false
  | _ -> true
;;

let has_matching_path ~scope t =
  let cookie_path = Uri.path t.scope in
  if cookie_path = "/"
  then true
  else (
    let path = Uri.path scope in
    Std.String.is_prefix ~affix:cookie_path path || cookie_path = path)
;;

let is_secure ~scope t =
  match Uri.scheme scope with
  | Some "http" -> not t.secure
  | Some "https" -> true
  | _ -> not t.secure
;;

let to_cookie_header ?now ?(elapsed = 0L) ?(scope = Uri.of_string "/") tl =
  if List.length tl = 0
  then "", ""
  else (
    let idx = ref 0 in
    let cookie_map : string Cookie_map.t =
      tl
      |> List.filter (fun c ->
             is_not_expired ?now c
             && has_matching_domain ~scope c
             && has_matching_path ~scope c
             && is_secure ~scope c)
      |> List.fold_left
           (fun m c ->
             idx := !idx + 1;
             let key, _value = c.value in
             Cookie_map.update (!idx, key) (fun _ -> Some c) m)
           Cookie_map.empty
      |> Cookie_map.filter_value (is_not_too_old ~elapsed)
      |> Cookie_map.map (fun c -> snd c.value)
    in
    if Cookie_map.is_empty cookie_map
    then "", ""
    else
      ( "Cookie"
      , Cookie_map.fold (fun (_idx, key) value l -> (key, value) :: l) cookie_map []
        |> List.rev
        |> List.map (fun (key, value) -> Printf.sprintf "%s=%s" key value)
        |> String.concat "; " ))
;;

let cookies_of_header (key, value) =
  match key with
  | "Cookie" | "cookie" ->
    String.split_on_char ';' value
    |> List.map (String.split_on_char '=')
    |> List.filter_map (function
           | [ key; value ] -> Some (String.trim key, String.trim value)
           | _ -> None)
  | _ -> []
;;
