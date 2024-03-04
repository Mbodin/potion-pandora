(* Create a ml file incorporating the file all.png. *)

let png = "all.png"

let prefix = "image"

(* Convert a number into a string using only [a-z]. *)
let rec num_to_str n =
  if n = 0 then ""
  else (
    let nb_letters = 26 in
    let a, b = n / nb_letters, n mod nb_letters in
    let b = Char.chr (Char.code 'a' + b) in
    let a = num_to_str a in
    Printf.sprintf "%s%c" a b
  )

let () =
  let str =
    let channel = open_in_bin png in
    let len = in_channel_length channel in
    really_input_string channel len in
  (* The string is meant to be enclosed in quoted OCaml strings {id|str|id}, but for that
    we need to make sure that the string "|id}" doesn't appear in str.
    More generally, we shall find an id that won't appear in the string. *)
  let rec protect n =
    let id = prefix ^ num_to_str n in
    let regexp = Str.regexp id in
    if Str.string_match regexp str 0 then
      protect (n + 1)
    else id in
  let id = protect 0 in
  print_endline (Printf.sprintf "let content = {%s|%s|%s}" id str id) ;
  print_endline "let chunk = ImageUtil.chunk_reader_of_string content" ;
  print_endline "let image = ImageLib.PNG.parsefile chunk"

