(* In this program, most images are bundled up into a single large image.
  Thus, any part is actually a subpart of this image.
  This type stores the corresponding coordinates. *)
type t = {
  width : int ;
  height : int ;
  position : int * int ;
  picture : Image.image (* The larger picture from which this image is taken from. *)
}

let dimensions img =
  (img.width, img.height)

let make ?(bundle = Bundled_image.image) width height position =
  assert (fst position >= 0 && snd position >= 0) ;
  assert (fst position + width <= bundle.Image.width) ;
  assert (snd position + height <= bundle.Image.height) ;
  { width ; height ; position ; picture = bundle }

let sub t width height (x, y) =
  assert (x >= 0 && y >= 0) ;
  assert (x + width <= t.width) ;
  assert (y + height <= t.height) ;
  { width ; height ; picture = t.picture ; position = (x + fst t.position, y + snd t.position) }

let from_image img =
  make ~bundle:img img.Image.width img.Image.height (0, 0)

let read img (x, y) =
  assert (x >= 0 && y >= 0) ;
  assert (x < img.width && y < img.height) ;
  let (px, py) = img.position in
  Image.read_rgba img.picture (x + px) (y + py) (fun r g b a -> (r, g, b, a))

let enlarge img (width, height) =
  assert (img.width <= width && img.height <= height) ;
  let (basex, basey) =
    let center = (width / 2, height / 2) in
    let center_img = (img.width / 2, img.height / 2) in
    (fst center - fst center_img, snd center - snd center_img) in
  let result = Image.create_rgb ~alpha:true (max 1 width) (max 1 height) in
  Image.fill_rgb ~alpha:0 result 0 0 0 ;
  for x = 0 to img.width - 1 do
    for y = 0 to img.height - 1 do
      let (r, g, b, a) = read img (x, y) in
      Image.write_rgba result (x + basex) (y + basey) r g b a
    done
  done ;
  from_image result

(* Given a canvas size, a list of images and offsets (which must be positive), combine the images
  into a single image. *)
let combine_raw (width, height) imgl =
  let result = Image.create_rgb ~alpha:true (max 1 width) (max 1 height) in
  Image.fill_rgb ~alpha:0 result 0 0 0 ;
  List.iter (fun (img, (dx, dy)) ->
    for x = 0 to img.width - 1 do
      for y = 0 to img.height - 1 do
        let (r, g, b, a) = read img (x, y) in
        assert (x + dx >= 0 && x + dx < width) ;
        assert (y + dy >= 0 && y + dy < height) ;
        if a <> 0 then
          Image.write_rgba result (x + dx) (y + dy) r g b a
      done
    done) imgl ;
  from_image result

let combine imgl =
  (* The dimension of the final image and the difference to the offset coordinates. *)
  let (dim, d) =
    let (min_x, min_y, max_x, max_y) =
      List.fold_left (fun (min_x, min_y, max_x, max_y) (img, (dx, dy)) ->
          (min min_x dx, min min_y dy, max max_x (dx + img.width - 1), max max_y (dy + img.height - 1)))
        (0, 0, 0, 0) imgl in
    ((1 + max_x - min_x, 1 + max_y - min_y), (-min_x, -min_y)) in
  (* We apply the difference to the offsets, to make sure that they are all positive. *)
  let imgl = List.map (fun (img, (dx, dy)) -> (img, (dx + fst d, dy + snd d))) imgl in
  combine_raw dim imgl

let combine_horizontally imgl =
  (* First, we compute the total required width and height. *)
  let (min_width, width, height) =
    List.fold_left (fun (mw, w, h) (offset, img) ->
      (min mw (w + offset), w + offset + img.width, max h img.height)) (0, 0, 0) imgl in
  assert (min_width <= 0) ;
  let width = width - min_width in
  let width = max 0 width in (* There could be negative offsets. *)
  (* Then we compute the individual offsets. *)
  let (_width, imgl) =
    List.fold_left (fun (x, imgl) (offset, img) ->
      let y = (height - img.height) / 2 in
      (x + offset + img.width, (img, (x + offset, y)) :: imgl)) (-min_width, []) imgl in
  combine_raw (width, height) imgl

let horizontal_sequence offset imgl =
  combine_horizontally (List.mapi (fun i img ->
    let offset = if i = 0 then 0 else offset in
    (offset, img)) imgl)

let combine_vertically imgl =
  (* First, we compute the total required width and height. *)
  let (width, min_height, height) =
    List.fold_left (fun (w, mh, h) (offset, img) ->
      (max w img.width, min mh (h + offset), h + offset + img.height)) (0, 0, 0) imgl in
  assert (min_height <= 0) ;
  let height = height - min_height in
  let height = max 0 height in (* There could be negative offsets. *)
  (* Then we compute the individual offsets. *)
  let (_height, imgl) =
    List.fold_left (fun (y, imgl) (offset, img) ->
      let x = (width - img.width) / 2 in
      (y + offset + img.height, (img, (x, y + offset)) :: imgl)) (-min_height, []) imgl in
  combine_raw (width, height) imgl

let vertical_sequence offset imgl =
  combine_vertically (List.mapi (fun i img ->
    let offset = if i = 0 then 0 else offset in
    (offset, img)) imgl)

