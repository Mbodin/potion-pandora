
(* Fill a pattern on an image along the provided width and height from the provided coordinates.
  The opaque parameter states whether it should overwrite opaque pixels when the pattern is
  transparent. *)
let fill_pattern opaque pattern image width height (x, y) =
  let (pattern_width, pattern_height) = Animation.image_dimensions pattern in
  for x = x to min width image.Image.width - 1 do
    for y = y to min height image.Image.height - 1 do
      let (r, g, b, a) =
        Animation.read_image pattern (x mod pattern_width, y mod pattern_height) in
      if a <> 0 || opaque then
        Image.write_rgba image x y r g b a
    done
  done

let rectangle pattern (width, height) =
  let img = Image.create_rgb ~alpha:true width height in
  fill_pattern true pattern img width height (0, 0) ;
  img

let decolor ~pattern img =
  let img = Image.create_rgb ~alpha:true img.Image.width img.Image.height in
  let (pattern_width, pattern_height) = Animation.image_dimensions pattern in
  for x = 0 to img.Image.width - 1 do
    for y = 0 to img.Image.height - 1 do
      let (pr, pg, pb, pa) =
        Animation.read_image pattern (x mod pattern_width, y mod pattern_height) in
      Image.read_rgba img x y (fun r g b a ->
        Image.write_rgba img x y pr pg pb (min a pa))
    done
  done ;
  img

let curve ifl (width, height) =
  let img = Image.create_rgb ~alpha:true width height in
  Image.fill_rgb ~alpha:0 img 0 0 0 ;
  for x = 0 to img.Image.width do
    let rec aux y = function
      | [] -> ()
      | (pattern, f) :: l ->
        let y' = f x in
        fill_pattern true pattern img 1 (y' - y) (x, y) ;
        aux y' l in
    aux 0 ifl
  done ;
  img

let shimmer ?(quantity = 50) ?(amplitude = 5) ?(duration = 10) img =
  let bundle = Image.create_rgb ~alpha:true (img.Image.width * duration) img.Image.height in
  (* First, copying the image as many times as the duration. *)
  for t = 0 to duration - 1 do
    let shift = img.Image.width * t in
    for x = 0 to img.Image.width - 1 do
      for y = 0 to img.Image.height - 1 do
        Image.read_rgba img x y (fun r g b a ->
          Image.write_rgba bundle (shift + x) y r g b a)
      done
    done
  done ;
  (* Second, Adding the waves. *)
  let square x = x *. x in
  let power4 x = square (square x) in
  for _ = 0 to quantity - 1 do
    let start_t = Random.int duration in
    let pos_x = Random.int img.Image.width in
    let pos_y = Random.int img.Image.height in
    let max_t = max 1 (2 * amplitude) in
    for dt = 0 to max_t do
      let t = (start_t + dt) mod duration in
      let shift = img.Image.width * t in
      let len =
        let rel = Float.of_int dt /. Float.of_int max_t in
        Float.to_int (Float.of_int amplitude *. (1. -. power4 rel)) in
      assert (len >= 0) ;
      assert (len <= amplitude) ;
      let pos_x = pos_x - dt / 2 in
      let pos_y = pos_y - dt / 2 in
      for x = max 0 pos_x to min img.Image.width (pos_x + len - 1) do
        let dy =
          let rel = Float.of_int (x - pos_x) /. Float.of_int len in
          let ampl = Float.of_int amplitude *. (1. -. square rel) in
          let relt = Float.of_int dt /. Float.of_int max_t in
          Float.to_int (ampl *. sin (relt *. 2. *. Float.pi)) in
        Image.read_rgba img x (pos_y + dy) (fun r g b a ->
          Image.write_rgba bundle (shift + x) pos_y r g b a)
      done
    done
  done ;
  (* Finally, bundling the images. *)
  List.init duration (fun t ->
    Animation.make_subimage ~bundle img.Image.width img.Image.height (img.Image.width * duration, 0))

