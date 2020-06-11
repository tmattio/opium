open Opium.Std
open Opium_kernel

let cookie = "__counter_session"

module Session = struct
  include Session.Memory

  let increment t session =
    let value = string_of_int (1 + int_of_string session.value) in
    set t ~value session
  ;;
end

let store = Session.create ()

let print_session =
  get "/" (fun req ->
      let open Lwt.Syntax in
      let cookie_header = Request.get_header req "cookie" |> Option.value ~default:"" in
      let* session =
        Session.of_header_or_create store cookie "0" ("Cookie", cookie_header)
      in
      let* () = Session.increment store session in
      let headers = Session.to_cookie_hdrs cookie session |> Rock.Headers.of_list in
      let response =
        Response.of_string ~headers @@ Printf.sprintf "Hello, %s\n" session.value
      in
      Lwt.return response)
;;

let _ =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Nocrypto_entropy_unix.initialize ();
  App.empty |> print_session |> App.run_command
;;
