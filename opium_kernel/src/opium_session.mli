(*----------------------------------------------------------------------------
    Copyright (c) 2015 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

(** The signature for a cookie-compatible backend. *)
module type Backend =
  Session.S.Future with type key = string and type value = string and type period = int64

(** The signature for a cookie session manager. *)
module type S = sig
  (** The type of a handle on the store. *)
  type backend

  (** The type of a session key. *)
  type key

  (** The type of a session value. *)
  type value

  (** The type of a session expiry period. *)
  type period

  (** The session type.
      This type is marked private, so the record fields can be accessed and
      pattern-matched as usual. However, values of this type cannot be
      constructed directly. To create a new session, use {!generate}. To
      retrieve an existing [session], use {!of_key} or {!of_header}. To modify
      {!field:t.expiry_period} or {!field:t.value}, use {!set}. Finally,
      use {!to_cookie_hdrs} to smartly generate [Set-Cookie] and related
      headers. *)
  type t = private
    { key : key (** They key for the session in the backend and in cookies. *)
    ; mutable value : value (** The value for the session stored in the backend. *)
    ; mutable expiry_period : period
          (** The period from now in seconds that the session will expire. *)
    ; mutable modified : bool (** Whether the session data or expiry have been modified *)
    }

  (** [of_key backend key] fetches the session associated with [key] from the
      backend, if present and unexpired. *)
  val of_key : backend -> key -> (t, Session.S.error) result Lwt.t

  (** [of_header backend cookie_key header] retrieves the session key from
      the cookies in [header]. If [cookie_key] is not present in any cookies in
      [header], then this function will return [None]. If a session key is
      found, it will call [{!val:of_key} backend key]. If both lookups were
      successful, then this function will return [Some session]. If no key was
      found in [header], it will return [None]. *)
  val of_header
    :  backend
    -> string
    -> Cookie.header
    -> (t option, Session.S.error) result Lwt.t

  (** [of_header_or_create ?expiry backend cookie_key default header] retrieves
      the session key from the cookies in [header]. If [cookie_key] is not
      present in any cookies in the [header] or if the session is not a valid
      one, a new session will be using [expiry] for the expiration period and
      [default] as the value. *)
  val of_header_or_create
    :  ?expiry:period
    -> backend
    -> string
    -> value
    -> Cookie.header
    -> t Lwt.t

  (** [to_cookie_hdrs cookie_key session] will generate response
      headers to communicate session changes to the client. This function takes
      into account the {!field:t.modified} field of the {{!type:t}session}
      type, and will not generate headers if they are not needed. *)
  val to_cookie_hdrs
    :  ?discard:bool
    -> ?path:string
    -> ?domain:string
    -> ?secure:bool
    -> ?http_only:bool
    -> string
    -> t
    -> (string * string) list

  (** [clear_hdrs cookie_key] will generate response headers to
      communicate that the client should evict the session with key
      [cookie_key]. *)
  val clear_hdrs : ?path:string -> ?domain:string -> string -> (string * string) list

  (** [generate ?expiry backend value] will allocate a new session in the backend
      [backend]. The session will expire [expiry] seconds from now, defaulting to
      [default_period backend] if one is not explicitly specified. *)
  val generate : ?expiry:period -> backend -> value -> t Lwt.t

  (** [clear backend session] removes [session] from [backend]. The backend
      may choose to persist the session value beyond this call, but any
      subsequent operations involving [key] should behave as if [key] is
      not present in the backend.
      The {!value} and {!field:t.expiry_period} of [session] will be zero'd
      out, and the {!field:t.modified} flag will be set. Calling
      {!to_cookie_hdrs} on a cleared session will generate the appropriate
      headers directing the client to clear the associated cookie. *)
  val clear : backend -> t -> unit Lwt.t

  (** [set ?expiry ?value backend session] sets the [value] for the session
      associated [key] in [backend] and sets the session to expire [expiry]
      seconds from now. If [expiry] is not provided, the expiry period reported
      by [default_period backend] will be used instead. If no value is provided, then
      only the expiry will be updated. *)
  val set : ?expiry:period -> ?value:value -> backend -> t -> unit Lwt.t
end

(** Create a cookie session manager given an appropriate backend. *)
module Make (B : Backend with type +'a io = 'a Lwt.t) :
  S
    with type backend = B.t
     and type key = B.key
     and type value = B.value
     and type period = B.period

(** In-memory session backend using the {!S.Now} signature.

    The default expiry period is one week. *)
module Memory : sig
  include S with type key = string and type value = string and type period = int64

  (** [create ()] returns the handle on a new in-memory store. *)
  val create : unit -> backend

  (** [set_default_period t period] sets the default expiry period of [t]. This
      will only affect future operations. *)
  val set_default_period : backend -> period -> unit
end

(** Session backend that stores the sessions in cookies.

  The cookies are signed and encrypted to ensure they cannot be tampered with.

  The [Signed_cookie] session uses the request to access the session values and stores the values in memory.
  Users must call [to_cookie_hdrs] to generate the response cookies. *)
module Cookie : sig
  include S with type key = string and type value = string and type period = int64

  (** [create ()] returns the handle on a new in-memory store. *)
  val create : Rock.Request.t -> backend

  (** [set_default_period t period] sets the default expiry period of [t]. This
          will only affect future operations. *)
  val set_default_period : backend -> period -> unit
end
