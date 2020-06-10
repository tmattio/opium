open Alcotest
open Opium_kernel

let test_parse_simple () =
  let (year, month, day), ((hour, minute, second), offset) =
    Date.parse_exn "Tue, 9 Jun 2020 11:49:28 UTC"
  in
  (check int) "year" 2020 year;
  (check int) "month" 2 month;
  (check int) "day" 27 day;
  (check int) "hour" 8 hour;
  (check int) "minute" 4 minute;
  (check int) "second" 19 second;
  (check int) "offset" 0 offset
;;

let test_serialize_simple () =
  let s = "Tue, 9 Jun 2020 11:49:28 UTC" in
  let r = s |> Date.parse_exn |> Date.serialize in
  (check string) "same string" s r
;;

let () =
  Alcotest.run
    "Date"
    [ "parse", [ "parse simple", `Quick, test_parse_simple ]
    ; "serialize", [ "serialize simple", `Quick, test_serialize_simple ]
    ]
;;
