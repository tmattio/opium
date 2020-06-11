module Signer = struct
  type t =
    { secret : string
    ; salt : string
    }

  let make ?(salt = "salt.signer") secret = { secret; salt }

  let constant_time_compare' a b init =
    let len = String.length a in
    let result = ref init in
    for i = 0 to len - 1 do
      result := !result lor Char.(compare a.[i] b.[i])
    done;
    !result = 0
  ;;

  let constant_time_compare a b =
    if String.length a <> String.length b
    then constant_time_compare' b b 1
    else constant_time_compare' a b 0
  ;;

  let derive_key t =
    Nocrypto.Hash.mac `SHA1 ~key:(Cstruct.of_string t.secret) (Cstruct.of_string t.salt)
  ;;

  let get_signature t value =
    value
    |> Cstruct.of_string
    |> Nocrypto.Hash.mac `SHA1 ~key:(derive_key t)
    |> Nocrypto.Base64.encode
    |> Cstruct.to_string
  ;;

  let sign t data = String.concat "." [ data; get_signature t data ]

  let verified t value signature =
    if constant_time_compare signature (get_signature t value) then Some value else None
  ;;

  let unsign t data =
    match Std.String.cut ~rev:true ~sep:"." data with
    | Some (value, signature) -> verified t value signature
    | None -> None
  ;;
end

type key = string
type value = string
type period = int64

type session =
  { value : value option
  ; expiry : period
  }

type t =
  { request : Rock.Request.t
  ; store : (key, session) Hashtbl.t
  ; mutable default_period : period
  }

let gensym () = Cstruct.to_string Nocrypto.(Base64.encode (Rng.generate 30))

let create request =
  { request
  ; store =
      Hashtbl.create 10 (* One week. If this changes, change module documentation. *)
  ; default_period = Int64.of_int (60 * 60 * 24 * 7)
  }
;;

let now () = Int64.of_float (Unix.time ())
let default_period t = t.default_period
let set_default_period t period = t.default_period <- period
let clear t key = Hashtbl.remove t.store key

let get t key =
  try
    let result = Hashtbl.find t.store key in
    let period = Int64.(sub result.expiry (now ())) in
    if Int64.compare period 0L < 0
    then Error Session.S.Not_found
    else (
      match result.value with
      | None -> Error Session.S.Not_set
      | Some value -> Ok (value, period))
  with
  | Not_found -> Error Session.S.Not_found
;;

let _set ?expiry ?value t key =
  let expiry =
    match expiry with
    | None -> Int64.(add (now ()) (default_period t))
    | Some expiry -> Int64.(add (now ()) expiry)
  in
  let session = { expiry; value } in
  Hashtbl.replace t.store key session
;;

let set ?expiry t key value = _set ?expiry ~value t key

let generate ?expiry ?value t =
  let key = gensym () in
  _set ?expiry ?value t key;
  key
;;
