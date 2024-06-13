(* Merge all the images of the images/ folder into a single png file.
  Also outputs an ml file containing the location of each bits within this giant image. *)

let image_folder = "../images"
let png_output = "all.png"

(* Declaration of some regexpes. *)
let regexp_name = Str.regexp {|^\([a-z][A-Za-z0-9_]*\).png$|}
let regexp_forbidden_names = Str.regexp {|^\(make\|coords\)$|}

module SMap = Map.Make (String)

(* Copy an image into a buffer image at a given position.
  The buffer image must be large enough for the copied image to fit. *)
let copy_into image buffer pos_x pos_y =
  for x = 0 to image.Image.width - 1 do
    for y = 0 to image.Image.height - 1 do
      Image.read_rgba image x y (fun r g b a ->
        Image.write_rgba buffer (pos_x + x) (pos_y + y) r g b a)
    done
  done

(* Given an image, split it into several images along its white lines. *)
let split ?name image =
  assert (image.Image.height > 0) ;
  let check r g b a =
    List.for_all ((=) image.Image.max_val) [r; g; b; a] in
  let extract_image x_begin x_end =
    let buffer = Image.create_rgb ~alpha:true (x_end - x_begin + 1) image.Image.height in
    for x = x_begin to x_end do
      for y = 0 to image.Image.height - 1 do
        Image.read_rgba image x y (fun r g b a ->
          Image.write_rgba buffer (x - x_begin) y r g b a)
      done
    done ;
    buffer in
  let rec scan_x previous_x x =
    if x = image.Image.width then (
      if x = previous_x then []
      else [extract_image previous_x (x - 1)]
    ) else (
      let print_assert msg b =
        if not b then prerr_endline msg ;
        assert b in
      let line =
        Image.read_rgba image x 0 (fun r g b a ->
          if check r g b a then (
            for y = 0 to image.Image.height - 1 do
              print_assert
                (Printf.sprintf "Uncomplete white ligne%s."
                  (Option.fold ~none:"" ~some:(fun n -> " in " ^ n) name))
                (Image.read_rgba image x y check)
            done ;
            true
          ) else false) in
      if line then (
        let img = extract_image previous_x (x - 1) in
        img :: scan_x (x + 1) (x + 1)
      ) else scan_x previous_x (x + 1)
    ) in
  scan_x 0 0

(* Area of an image. *)
let area i = i.Image.width * i.Image.height

(* List of all individual images paired with their name and index, as well
  as the a map of images for each name. *)
let all_images, image_map =
  let all_files = Array.to_list (Sys.readdir image_folder) in
  let all_filenames =
    List.filter_map (fun file ->
      if Str.string_match regexp_name file 0 then (
        let name = Str.matched_group 1 file in
        if Str.string_match regexp_forbidden_names name 0 then
          Printf.eprintf "Warning: unexpected name %s.\n" file ;
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
  let all_images =
    List.map (fun (name, i) -> (name, split ~name i)) all_images in
  let image_map =
    List.fold_left (fun image_map (name, l) ->
      assert (not (SMap.mem name image_map)) ;
      SMap.add name l image_map) SMap.empty all_images in
  let all_images =
    List.concat_map (fun (name, l) ->
      List.mapi (fun index i -> ((name, index), i)) l) all_images in
  let all_images =
    List.sort (fun (_id1, i1) (_id2, i2) ->
      - compare (area i1) (area i2)) all_images in
  all_images, image_map

let max_width =
  List.fold_left (fun w (_id, i) -> max w i.Image.width) 1 all_images

(* Set and get coordinates for a specific name and index (which paired forms their identifier). *)
let set_coordinates, get_coordinates =
  let coordinates =
    SMap.map (fun l -> Array.make (List.length l) None) (image_map) in
  let set (name, index) coords =
    match SMap.find_opt name coordinates with
    | None -> assert false
    | Some a ->
      assert (a.(index) = None) ;
      a.(index) <- Some coords in
  let get (name, index) =
    match SMap.find_opt name coordinates with
    | None -> assert false
    | Some a -> a.(index) in
  set, get

(* Set up the coordinates of all images into a large single image. *)
let () =
  let all_images = Array.of_list all_images in
  (* Fill-in all the images from the provided index that can be filled-in,
    starting from position (angle_x, angle_y) and using at most max_y of height. *)
  let rec build_from index angle_x angle_y max_height =
    if index <> Array.length all_images then (
      let (id, image) = all_images.(index) in
      let index = index + 1 in
      if get_coordinates id <> None
         || image.Image.height > max_height
         || angle_x + image.Image.width > max_width then
        (* The current image has actually already been placed or it is too big. *)
        build_from index angle_x angle_y max_height
      else (
        (* We place the current image. *)
        set_coordinates id (angle_x, angle_y) ;
        (* We then recurse over both left-over lines. *)
        let height = image.Image.height in
        build_from index (angle_x + image.Image.width) angle_y height ;
        build_from index angle_x (angle_y + height) (max_height - height)
      )
    ) in
  (* Fill in all images from the provided index, creating new levels on the way. *)
  let rec build_level index level_y =
    if index <> Array.length all_images then (
      let (id, image) = all_images.(index) in
      let index = index + 1 in
      if get_coordinates id <> None then
        build_level index level_y
      else (
        (* This is the first non-yet-placed image. *)
        set_coordinates id (0, level_y) ;
        build_from index image.Image.width level_y image.Image.height ;
        build_level index (level_y + image.Image.height)
      )
    ) in
  build_level 0 0

let () =
  assert (List.for_all (fun (id, i) ->
    match get_coordinates id with
    | None -> false
    | Some (x, _y) -> x + i.Image.width <= max_width) all_images)

let max_height =
  List.fold_left (fun v (id, i) ->
    let v' =
      match get_coordinates id with
      | None -> assert false
      | Some (_x, y) -> y + i.Image.height in
    max v v') 1 all_images

let full_image =
  let image = Image.create_rgb ~alpha:true max_width max_height in
  Image.fill_rgb ~alpha:0 image 0 0 0 ;
  List.iter (fun (id, i) ->
    let (x, y) =
      match get_coordinates id with
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
  print_endline {|type coords = (int * int) * (int * int)|} ;
  print_endline {|let make width height (x, y) : coords = ((width, height), (x, y))|} ;
  SMap.iter (fun name images ->
    print_endline (Printf.sprintf "let %s = [" name) ;
    List.iteri (fun index i ->
      let (w, h) = (i.Image.width, i.Image.height) in
      let (x, y) =
        match get_coordinates (name, index) with
        | None -> assert false
        | Some xy -> xy in
      print_endline (Printf.sprintf "\tmake %i %i (%i, %i) ;" w h x y)) images ;
    print_endline "]") image_map ;
  ()

