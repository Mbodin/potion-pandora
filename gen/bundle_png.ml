(* Merge all the images of the images/ folder into a single png file.
  Also outputs an ml file containing the location of each bits within this giant image. *)

let image_folder = "images"

let regexp_name = Str.regexp {|\([^.]*\).png|}
let regexp_forbidden_names = Str.regexp {|\(\)|}

let all_images =
  let all_files = Array.to_list (Sys.readdir image_folder) in
  let all_filenames =
    List.filter_map (fun file ->
      if Str.string_match regexp_name file 0 then (
        let file = Str.matched_group 1 file in
        if Str.string_match regexp_forbidden_names file 0 then
          Printf.eprintf "Warning: unexpected file name %s.png." file ;
        Some file
      ) else (
        Printf.eprintf "Warning: file %s not a png file." file ;
        None
      )) all_files in
  List.map (fun file ->
    let file = Printf.sprintf "%s/%s.png" image_folder file in
    let channel = open_in_bin file in
    let len = in_channel_length channel in
    let content = really_input_string channel len in
    let chunk = ImageUtil.chunk_reader_of_string content in
    ImageLib.PNG.parsefile chunk) all_filenames

let () =
  print_endline {|[@@@warning "-32"]|} ;
  () (* TODO *)

