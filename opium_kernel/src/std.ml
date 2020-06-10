include Stdlib

module String = struct
  include String

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let is_prefix ~affix s =
    let len_a = length affix in
    let len_s = length s in
    if len_a > len_s
    then false
    else (
      let max_idx_a = len_a - 1 in
      let rec loop i =
        if i > max_idx_a
        then true
        else if unsafe_get affix i <> unsafe_get s i
        then false
        else loop (i + 1)
      in
      loop 0)
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let is_suffix ~affix s =
    let max_idx_a = length affix - 1 in
    let max_idx_s = length s - 1 in
    if max_idx_a > max_idx_s
    then false
    else (
      let rec loop i =
        if i > max_idx_a
        then true
        else if unsafe_get affix (max_idx_a - i) <> unsafe_get s (max_idx_s - i)
        then false
        else loop (i + 1)
      in
      loop 0)
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let bytes_unsafe_blit_string s sfirst d dfirst len =
    Bytes.(unsafe_blit (unsafe_of_string s) sfirst d dfirst len)
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let unsafe_string_sub s first len =
    let b = Bytes.create len in
    bytes_unsafe_blit_string s first b 0 len;
    Bytes.unsafe_to_string b
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let fcut ~sep s =
    let sep_len = length sep in
    if sep_len = 0
    then invalid_arg "~sep is an empty string"
    else (
      let s_len = length s in
      let max_sep_idx = sep_len - 1 in
      let max_s_idx = s_len - sep_len in
      let rec check_sep i k =
        if k > max_sep_idx
        then (
          let r_start = i + sep_len in
          Some (unsafe_string_sub s 0 i, unsafe_string_sub s r_start (s_len - r_start)))
        else if unsafe_get s (i + k) = unsafe_get sep k
        then check_sep i (k + 1)
        else scan (i + 1)
      and scan i =
        if i > max_s_idx
        then None
        else if unsafe_get s i = unsafe_get sep 0
        then check_sep i 1
        else scan (i + 1)
      in
      scan 0)
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let rcut ~sep s =
    let sep_len = length sep in
    if sep_len = 0
    then invalid_arg "~sep is an empty string"
    else (
      let s_len = length s in
      let max_sep_idx = sep_len - 1 in
      let max_s_idx = s_len - 1 in
      let rec check_sep i k =
        if k > max_sep_idx
        then (
          let r_start = i + sep_len in
          Some (unsafe_string_sub s 0 i, unsafe_string_sub s r_start (s_len - r_start)))
        else if unsafe_get s (i + k) = unsafe_get sep k
        then check_sep i (k + 1)
        else rscan (i - 1)
      and rscan i =
        if i < 0
        then None
        else if unsafe_get s i = unsafe_get sep 0
        then check_sep i 1
        else rscan (i - 1)
      in
      rscan (max_s_idx - max_sep_idx))
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let cut ?(rev = false) ~sep s = if rev then rcut ~sep s else fcut ~sep s

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let filter sat s =
    let max_idx = length s - 1 in
    let rec with_buf b k i =
      (* k is the write index in b *)
      if i > max_idx
      then Bytes.sub_string b 0 k
      else (
        let c = unsafe_get s i in
        if sat c
        then (
          Bytes.unsafe_set b k c;
          with_buf b (k + 1) (i + 1))
        else with_buf b k (i + 1))
    in
    let rec try_no_alloc i =
      if i > max_idx
      then s
      else if sat (unsafe_get s i)
      then try_no_alloc (i + 1)
      else if i = max_idx
      then unsafe_string_sub s 0 i
      else (
        let b = Bytes.of_string s in
        (* copy and overwrite starting from i *)
        with_buf b i (i + 1))
    in
    try_no_alloc 0
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let fdrop ?(min = 0) ?(max = max_int) ?(sat = fun _ -> true) s =
    if min < 0
    then invalid_arg (Format.asprintf "negative ~min (%d)" min)
    else if max < 0
    then invalid_arg (Format.asprintf "negative ~max (%d)" max)
    else if min > max || max = 0
    then s
    else (
      let len = length s in
      let max_idx = len - 1 in
      let max_idx =
        let k = max - 1 in
        if k > max_idx then max_idx else k
      in
      let need_idx = min in
      let rec loop i =
        if i <= max_idx && sat (unsafe_get s i)
        then loop (i + 1)
        else if i < need_idx || i = 0
        then s
        else if i = len
        then ""
        else unsafe_string_sub s i (len - i)
      in
      loop 0)
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let rdrop ?(min = 0) ?(max = max_int) ?(sat = fun _ -> true) s =
    if min < 0
    then invalid_arg (Format.asprintf "negative ~min (%d)" min)
    else if max < 0
    then invalid_arg (Format.asprintf "negative ~max (%d)" max)
    else if min > max || max = 0
    then s
    else (
      let len = length s in
      let max_idx = len - 1 in
      let min_idx =
        let k = len - max in
        if k < 0 then 0 else k
      in
      let need_idx = max_idx - min in
      let rec loop i =
        if i >= min_idx && sat (unsafe_get s i)
        then loop (i - 1)
        else if i > need_idx || i = max_idx
        then s
        else if i = -1
        then ""
        else (
          let cut = i + 1 in
          unsafe_string_sub s 0 cut)
      in
      loop max_idx)
  ;;

  (* From https://github.com/dbuenzli/astring
     Copyright (c) 2016 Daniel C. Bünzli *)
  let drop ?(rev = false) ?min ?max ?sat s =
    match rev with
    | true -> rdrop ?min ?max ?sat s
    | false -> fdrop ?min ?max ?sat s
  ;;
end
