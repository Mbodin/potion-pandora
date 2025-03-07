
let transparent =
  let img = Image.create_rgb ~alpha:true 1 1 in
  Image.fill_rgb ~alpha:0 img 0 0 0 ;
  Subimage.from_image img

(* Fill a pattern on an image along the provided width and height from the provided coordinates.
  The opaque parameter states whether it should overwrite opaque pixels when the pattern is
  transparent. *)
let fill_pattern opaque pattern image width height (x, y) =
  let (pattern_width, pattern_height) = Subimage.dimensions pattern in
  for x = x to min (x + width - 1) (image.Image.width - 1) do
    for y = y to min (y + height - 1) (image.Image.height - 1) do
      let (r, g, b, a) =
        Subimage.read pattern (x mod pattern_width, y mod pattern_height) in
      if a <> 0 || opaque then
        Image.write_rgba image x y r g b a
    done
  done

let rectangle pattern (width, height) =
  let img = Image.create_rgb ~alpha:true (max 1 width) (max 1 height) in
  fill_pattern true pattern img (max 1 width) (max 1 height) (0, 0) ;
  Subimage.from_image img

let decolor ~pattern original =
  let (width, height) = Subimage.dimensions original in
  let destination = Image.create_rgb ~alpha:true width height in
  let (pattern_width, pattern_height) = Subimage.dimensions pattern in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let (pr, pg, pb, pa) = Subimage.read pattern (x mod pattern_width, y mod pattern_height) in
      let (_r, _g, _b, a) = Subimage.read original (x, y) in
      Image.write_rgba destination x y pr pg pb (min a pa)
    done
  done ;
  Subimage.from_image destination

let add_background pattern original =
  let (width, height) = Subimage.dimensions original in
  let destination = Image.create_rgb ~alpha:true width height in
  let (pattern_width, pattern_height) = Subimage.dimensions pattern in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let (r, g, b, a) = Subimage.read original (x, y) in
      let (pr, pg, pb, pa) = Subimage.read pattern (x mod pattern_width, y mod pattern_height) in
      let interp c pc = (c * a + pc * (255 - a)) / 255 in
      Image.write_rgba destination x y (interp r pr) (interp g pg) (interp b pb) (max a pa)
    done
  done ;
  Subimage.from_image destination

let curve ifl (width, height) =
  let img = Image.create_rgb ~alpha:true width height in
  Image.fill_rgb ~alpha:0 img 0 0 0 ;
  for x = 0 to width - 1 do
    let rec aux y = function
      | [] -> ()
      | (pattern, f) :: l ->
        let y' = f x in
        assert (y' >= y) ;
        fill_pattern true pattern img 1 (y' - y) (x, y) ;
        aux y' l in
    aux 0 ifl
  done ;
  Subimage.from_image img

let triangle_lower_left img size =
  curve [
    (transparent, (fun x -> size - 1 - x)) ;
    (img, (fun _x -> size))
  ] (size, size)

let triangle_lower_right img size =
  curve [
    (transparent, (fun x -> x)) ;
    (img, (fun _x -> size)) ;
  ] (size, size)

let shimmer ?(quantity = 50) ?(amplitude = 5) ?(duration = 10) ?(direction = (0., 1.)) img =
  let (width, height) = Subimage.dimensions img in
  let bundle = Image.create_rgb ~alpha:true (width * duration) height in
  (* First, copying the image as many times as the duration. *)
  for t = 0 to duration - 1 do
    let shift = width * t in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        let (r, g, b, a) = Subimage.read img (x, y) in
        Image.write_rgba bundle (shift + x) y r g b a
      done
    done
  done ;
  (* Second, Adding the waves. *)
  let square x = x *. x in
  let power4 x = square (square x) in
  for _ = 0 to quantity - 1 do
    let start_t = Random.int duration in
    let pos_x = Random.int (width + 2 * amplitude) - amplitude in
    let pos_y = Random.int (height + 2 * amplitude) - amplitude in
    let max_t = max 1 (2 * amplitude) in
    for dt = 0 to max_t do
      let t = (start_t + dt) mod duration in
      let shift = width * t in
      let len =
        let rel = Float.of_int dt /. Float.of_int max_t in
        Float.to_int (Float.of_int amplitude *. (1. -. power4 rel)) in
      assert (len >= 0) ;
      assert (len <= amplitude) ;
      let pos_x = pos_x + Float.to_int (0.5 +. fst direction *. Float.of_int dt) in
      let pos_y = pos_y + Float.to_int (0.5 +. snd direction *. Float.of_int dt) in
      for x = pos_x to pos_x + len - 1 do
        let dy =
          let rel = Float.of_int (x - pos_x) /. Float.of_int len in
          let ampl = Float.of_int amplitude *. (1. -. square rel) in
          let relt = Float.of_int dt /. Float.of_int max_t in
          Float.to_int (ampl *. sin (relt *. 2. *. Float.pi)) in
        if pos_y >= 0 && pos_y < height
          && pos_y + dy >= 0 && pos_y + dy < height
          && x >= 0 && x < width then (
            assert (shift + x >= 0) ;
            assert (shift + x < width * duration) ;
            let (_r, _g, _b, a0) = Subimage.read img (x, pos_y) in
            let (r, g, b, a) = Subimage.read img (x, pos_y + dy) in
            if a0 <> 0 && a <> 0 then Image.write_rgba bundle (shift + x) pos_y r g b a
        )
      done
    done
  done ;
  (* Finally, bundling the images. *)
  List.init duration (fun t ->
    Subimage.make ~bundle width height (width * t, 0))

(* Given a function f converting original coordinates to new coordinates, change the position
  of pixels in an image. *)
let change_coordinates new_width new_height f img =
  let (width, height) = Subimage.dimensions img in
  let modified = Image.create_rgb ~alpha:true new_width new_height in
  Image.fill_rgb ~alpha:0 modified 0 0 0 ;
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let (r, g, b, a) = Subimage.read img (x, y) in
      let (x', y') = f x y in
      Image.write_rgba modified x' y' r g b a
    done
  done ;
  Subimage.from_image modified

let flip_horizontally img =
  let (width, height) = Subimage.dimensions img in
  change_coordinates width height (fun x y -> (width - 1 - x, y)) img

let flip_vertically img =
  let (width, height) = Subimage.dimensions img in
  change_coordinates width height (fun x y -> (x, height - 1 - y)) img

let flip_diagonally img =
  let (width, height) = Subimage.dimensions img in
  change_coordinates height width (fun x y -> (y, x)) img

module ColorMap =
  Map.Make (struct
    type t = int * int * int
    let compare = compare
  end)

(* Provided a function and an image, return the non-transparent pixel maximizing the function. *)
let find_best_color f img =
  let best = ref None in
  let (width, height) = Subimage.dimensions img in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let (r, g, b, a) = Subimage.read img (x, y) in
      if a <> 0 then
        match !best with
        | None -> best := Some (r, g, b)
        | Some rgb0 ->
          if f rgb0 < f (r, g, b) then
            best := Some (r, g, b)
    done
  done ;
  match !best with
  | None -> assert false (* All the pixels of the image were transparent. *)
  | Some r -> r

let brightest = find_best_color (fun (r, g, b) -> r + g + b)
let darkest = find_best_color (fun (r, g, b) -> - (r + g + b))

let invert ?palette img =
  let palette =
    match palette with
    | Some palette -> palette
    | None -> img in
  let color_map = ref ColorMap.empty in
  let get rgb =
    match ColorMap.find_opt rgb !color_map with
    | Some c -> c
    | None ->
      let c =
        (* Finding the furthest color from the provided one. *)
        let distance (r', g', b') =
          let (r, g, b) = rgb in
          abs (r' - r) + abs (g' - g) + abs (b' - b) in
        find_best_color distance palette in
      color_map := ColorMap.add rgb c !color_map ;
      c in
  let (width, height) = Subimage.dimensions img in
  let img' = Image.create_rgb ~alpha:true width height in
  Image.fill_rgb ~alpha:0 img' 0 0 0 ;
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let (r, g, b, a) = Subimage.read img (x, y) in
      let (r', g', b') = get (r, g, b) in
      Image.write_rgba img' x y r' g' b' a
    done
  done ;
  Subimage.from_image img'

