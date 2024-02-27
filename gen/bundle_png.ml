(* Merge all the images of the images/ folder into a single png file.
  Also outputs an ml file containing the location of each bits within this giant image. *)

let image_folder = "images"
let png_output = "all.png"

let regexp_name = Str.regexp {|^\([a-z][A-Za-z0-9]*\).png$|}
let regexp_forbidden_names = Str.regexp {|^\(\)$|}

let all_images =
  let all_files = Array.to_list (Sys.readdir image_folder) in
  let all_filenames =
    List.filter_map (fun file ->
      if Str.string_match regexp_name file 0 then (
        let name = Str.matched_group 1 file in
        if Str.string_match regexp_forbidden_names file 0 then
          Printf.eprintf "Warning: unexpected file name %s.\n" file ;
        Some (name, image_folder ^ "/" ^ file)
      ) else (
        Printf.eprintf "Warning: unexpected file name %s.\n" file ;
        None
      )) all_files in
  let all_images =
    List.map (fun (name, file) ->
      let channel = open_in_bin file in
      let len = in_channel_length channel in
      let content = really_input_string channel len in
      let chunk = ImageUtil.chunk_reader_of_string content in
      (name, ImageLib.PNG.parsefile chunk)) all_filenames in
  List.sort (fun (_name1, i1) (_name2, i2) ->
    let c = compare i1.Image.width i2.Image.width in
    if c = 0 then
      - compare i1.Image.height i2.Image.height
    else -c) all_images

let max_width =
  match all_images with
  | [] -> 1
  | (_name, i) :: _ -> i.Image.width

let () =
  assert (List.for_all (fun (_name, i) -> i.Image.width <= max_width) all_images)

module SMap = Map.Make (String)

let coordinates =
  let all_images = Array.of_list all_images in
  let rec build_level index coordinates level_y =
    if index = Array.length all_images then coordinates
    else (
      let (first_name, first_image) = all_images.(index) in
      if SMap.mem first_name coordinates then
        (* The current image has actually already been placed. *)
        build_level (index + 1) coordinates level_y
      else (
        (* We create a new level by filling in the first available image (i.e., the widest left). *)
        let coordinates = SMap.add first_name (0, level_y) coordinates in
        (* We are left with a partially completed level, and we will try to fill it. *)
        let level_height = first_image.Image.height in
        let rec fill_level coordinates x =
          let rest_x = max_width - x in
          (* We want to fetch the largest image fitting a rectangle of level_height by rest_x. *)
          if rest_x = 0 || index + 1 = Array.length all_images then
            coordinates
          else (
            (* First, fetching the minimum index in the array all_images from which all images are
              at least rest_x wide. *)
            let fit index =
              let (_name, image) = all_images.(index) in
              image.Image.width <= rest_x in
            let min_index =
              let rec aux min max =
                if min >= max then min
                else (
                  let middle = (min + max) / 2 in
                  assert (middle <> max) ;
                  if fit middle then
                    aux min middle
                  else aux (middle + 1) max
                ) in
              aux (index + 1) (Array.length all_images - 1) in
            if not (fit min_index) then
              coordinates
            else (
              (* We then iterate over each non-yet-placed images, taking the first that could fit. *)
              let rec aux index =
                if index = Array.length all_images then
                  (* None were found: we leave the coordinates as-is, and let the rest of the function
                    create a new level. *)
                  coordinates
                else (
                  let (name, image) = all_images.(index) in
                  assert (image.Image.width <= rest_x) ;
                  if SMap.mem name coordinates then
                    (* This image has already been placed. *)
                    aux (index + 1)
                  else if image.Image.height <= level_height then (
                    assert (x + image.Image.width <= max_width) ;
                    (* We found a fitting candidate. *)
                    let coordinates = SMap.add name (x, level_y) coordinates in
                    fill_level coordinates (x + image.Image.width)
                  ) else aux (index + 1)
                ) in
              aux min_index)
          ) in
        let coordinates = fill_level coordinates first_image.Image.width in
        build_level (index + 1) coordinates (level_y + level_height)
    )) in
  build_level 0 SMap.empty 0

let () =
  assert (List.for_all (fun (name, i) ->
    match SMap.find_opt name coordinates with
    | None -> assert false
    | Some (x, _y) -> x + i.Image.width <= max_width) all_images)

let max_height =
  List.fold_left (fun v (name, i) ->
    let v' =
      match SMap.find_opt name coordinates with
      | None -> assert false
      | Some (_x, y) -> y + i.Image.height in
    max v v') 1 all_images

(* Copy an image into a buffer image at a given position.
  The buffer image must be large enough for the copied image to fit. *)
let copy_into image buffer pos_x pos_y =
  for x = 0 to image.Image.width - 1 do
    for y = 0 to image.Image.height - 1 do
      Image.read_rgba image x y (fun r g b a ->
        Image.write_rgba buffer (pos_x + x) (pos_y + y) r g b a)
    done
  done

let full_image =
  let image = Image.create_rgb ~alpha:true max_width max_height in
  Image.fill_rgb ~alpha:0 image 0 0 0 ;
  List.iter (fun (name, i) ->
    let (x, y) =
      match SMap.find_opt name coordinates with
      | None -> assert false
      | Some xy -> xy in
    copy_into i image x y) all_images ;
  image

(* Creating the PNG output. *)
let () =
  let image = ImageLib.PNG.bytes_of_png full_image in
  let channel = open_out_bin png_output in
  output_bytes channel image ;
  close_out channel

(* Creating the ML output. *)
let () =
  print_endline {|[@@@warning "-32"]|} ;
  SMap.iter (fun name (x, y) ->
    print_endline (Printf.sprintf "let %s = (%i, %i)" name x y)) coordinates ;
  ()

