
(* In this program, most images are bundled up into a single large image.
  Thus, any part is actually a subpart of this image.
  This type stores the corresponding coordinates. *)
type image = {
  width : int ;
  height : int ;
  position : int * int ;
  picture : Image.image (* The larger picture from which this image is taken from. *)
}

let image_dimensions img =
  (img.width, img.height)

let make_subimage ?(bundle = Bundled_image.image) width height position =
  assert (fst position >= 0 && snd position >= 0) ;
  assert (fst position + width <= bundle.Image.width) ;
  assert (snd position + height <= bundle.Image.height) ;
  { width ; height ; position ; picture = bundle }

let make_image img =
  make_subimage ~bundle:img img.Image.width img.Image.height (0, 0)

let read_image img (x, y) =
  assert (x >= 0 && y >= 0) ;
  assert (x < img.width && y < img.height) ;
  let (px, py) = img.position in
  Image.read_rgba img.picture (x + px) (y + py) (fun r g b a -> (r, g, b, a))


(* Create an image from the provided image of the provided dimensions.
  These dimensions must be larger than the image.
  The image will be centered. *)
let enlarge img (width, height) =
  assert (img.width <= width && img.height <= height) ;
  let (basex, basey) =
    let center = (width / 2, height / 2) in
    let center_img = (img.width / 2, img.height / 2) in
    (fst center - fst center_img, snd center - snd center_img) in
  let result = Image.create_rgb ~alpha:true width height in
  Image.fill_rgb ~alpha:0 result 0 0 0 ;
  for x = 0 to img.width - 1 do
    for y = 0 to img.height - 1 do
      let (r, g, b, a) = read_image img (x, y) in
      Image.write_rgba result (x + basex) (y + basey) r g b a
    done
  done ;
  make_image result


module IMap = Map.Make (Integer)
module ISet = Set.Make (Integer)
module EMap = Map.Make (Event)
module ESet = Set.Make (Event)
module ImageMap =
  Map.Make (struct
    type t = image
    let compare i1 i2 =
      let tuple i = (i.width, i.height, i.position) in
      compare (tuple i1) (tuple i2)
  end)

(* Number of frames per second. *)
let frames_per_second = 30

(* A type to count time, as a number of cycles.
  The type is abstracted away to avoid mixing with the state. *)
module Time : sig

  type t
  val zero : t
  val infinity : t
  val incr : t -> t
  val (<) : t -> t -> bool

  (* This converts a time in seconds into a time in number of frames. *)
  val of_seconds : float -> t

  (* For debug purposes: print the number of frames. *)
  val print : t -> string

end = struct

  type t = int
  let zero = 0
  let infinity = max_int
  let incr t =
    if t = max_int then max_int
    else t + 1
  let (<) = (<)

  let of_seconds s =
    let f = Float.round (s *. Float.of_int frames_per_second) in
    if f >= Float.of_int infinity then infinity
    else Float.to_int f

  let print = string_of_int

end
type time = Time.t

(* The representation of an object state. *)
type state = int

(* The internal automaton of an object.
  Each state is an array index.
  It then stores the associated image, as well as the next step for each event.
  The next state is itself associated *)
type automaton = (image * (time * state) Event.map) array

type t = {
  state : state (* The current state of the automaton. *) ;
  time : time (* The number of cycles that were spent on this state without taking any transition. *) ;
  delta : automaton (* How this state responds to events. *)
}

let image t =
  let (image, _next) = t.delta.(t.state) in
  image

let check_size t =
  let dim =
    let image = image t in
    (image.width, image.height) in
  Array.for_all (fun (image, _next) ->
    dim = (image.width, image.height)) t.delta

let send t e =
  let (_image, next) = t.delta.(t.state) in
  let (time, st) = Event.fetch next e in
  if e = Tau then
    if Time.(t.time < time) then
      { t with time = Time.incr t.time }
    else { t with state = st ; time = Time.zero }
  else
    if Time.(t.time < time) then t
    else {t with state = st }

let listen t e =
  let (_image, next) = t.delta.(t.state) in
  let (time, st) = Event.fetch next e in
  (st <> t.state && not Time.(t.time < time))

let print t =
  let name_of_state =
    let images = ref ImageMap.empty in
    fun st ->
      let (image, _next) = t.delta.(st) in
      let nimage =
        match ImageMap.find_opt image !images with
        | Some n -> n
        | None ->
          let n = Char.chr (Char.code 'A' + ImageMap.cardinal !images) in
          images := ImageMap.add image n !images ;
          n in
      Printf.sprintf "%c%i" nimage st in
  let aux st =
    let current = name_of_state st in
    let (_image, next) = t.delta.(st) in
    let l =
      List.map (fun e ->
          let (time, st') =  Event.fetch next e in
          let label = Printf.sprintf "%s, %s" (Time.print time) (Event.print e) in
          Printf.sprintf "%s -> %s [label = \"%s\"]\n" current (name_of_state st') label)
        Event.all in
    String.concat "" l in
  Printf.sprintf "digraph {\n%s}\n"
    (String.concat "" (List.init (Array.length t.delta) aux))

type sequence = (image * float) list

let loop s =
  let len = List.length s in
  assert (len > 0) ;
  let automaton =
    Array.of_list (List.mapi (fun index (image, time) ->
      (image, Event.create_map (function
        | Event.Tau -> (Time.of_seconds time, (index + 1) mod len)
        | _ -> (Time.infinity, index)))) s) in {
    delta = automaton ;
    time = Time.zero ;
    state = 0
  }

let static i = loop [(i, 1.)]

(* Create an automaton part encoding a sequence [s].
  The shift represents the first state of the sequence (it is meant to be concatenated at
  the end of the automaton and thus starts its indexes there).
  The set [restart] is a set of event that restart the sequence.
  [state] represents the state targeted after the sequence ended, and the last argument is
  [t.delta.(state)]: it is useful to tune the behaviour of [skip]. *)
let create_sequence shift skip restart s state (_image, action) =
  let len = List.length s in
  Array.of_list (List.mapi (fun index (image, time) ->
    let self = shift + index in
    let next =
      if index = len - 1 then state
      else self + 1 in
    (image, Event.create_map (function
      | Event.Tau -> (Time.of_seconds time, next)
      | e ->
        if ESet.mem e restart then (Time.zero, shift)
        else
          (* Reaction of the state after the sequence to this event. *)
          let reaction =
            if skip then
              let (time, state') = Event.fetch action e in
              if time = Time.zero && state' <> state (* FIXME: This looks doubtful. Shouldn't it be index instead? *) then
                Some state'
              else None
            else None in
          match reaction with
          | None -> (Time.infinity, self)
          | Some next -> (Time.zero, next)))) s)

let react t es ?(skip = false) ?(restart = false) s =
  let len = List.length s in
  if len = 0 then t
  else
    let es = ESet.of_list es in
    let shift = Array.length t.delta in
    let sequence =
      let restart = if restart then es else ESet.empty in
      create_sequence shift skip restart s t.state t.delta.(t.state) in
    let automaton =
      Array.append
        (Array.map (fun (image, next) ->
          (image, Event.create_map (fun e ->
            if ESet.mem e es then (Time.zero, shift)
            else Event.fetch next e))) t.delta)
        sequence in
    { t with delta = automaton }

let switch t1 es1 ?(skip = false) s1 =
  let skip1 = skip in fun t2 es2 ?(skip = false) s2 ->
  let skip2 = skip in
  let es1 = ESet.of_list es1 in
  let es2 = ESet.of_list es2 in
  (* There are four portions of states in the new automaton:
      - The states of t1,
      - The states of t2 (shifted by [Array.length t1.delta]),
      - The states to encode the sequence s1,
      - The states to encore the sequence s2. *)
  let shift2 = Array.length t1.delta in
  let shift_s1 = shift2 + Array.length t2.delta in
  let shift_s2 = shift_s1 + List.length s1 in
  let delta1 =
    Array.map (fun (image, f) ->
      (image, Event.create_map (fun e ->
        if ESet.mem e es1 then
          (Time.zero, shift_s1)
        else Event.fetch f e))) t1.delta in
  let delta2 =
    Array.map (fun (image, f) ->
      (image, Event.create_map (fun e ->
        if ESet.mem e es2 then
          (Time.zero, shift_s2)
        else
          let (time, state) = Event.fetch f e in
          (time, state + shift2)))) t2.delta in
  let delta_s1 = create_sequence shift_s1 skip1 ESet.empty s1 t2.state t2.delta.(t2.state) in
  let delta_s2 = create_sequence shift_s2 skip2 ESet.empty s2 t1.state t1.delta.(t1.state) in
  let automaton =
    Array.concat [
      delta1 ;
      delta2 ;
      delta_s1 ;
      delta_s2
    ] in
  { t1 with delta = automaton }

let force_same_size t =
  let size =
    Array.fold_left (fun (sizex, sizey) (image, _next) ->
      (max sizex image.width, max sizey image.height)) (0, 0) t.delta in
  let delta =
    Array.map (fun (image, next) ->
      let image =
        if (image.width, image.height) = size then image
        else (
          assert (image.width <= fst size && image.height <= snd size) ;
          enlarge image size
        ) in
      (image, next)) t.delta in
  { t with delta }

(* Given a canvas size, a list of images and offsets (which must be positive), combine the images
  into a single image. *)
let combine_images (width, height) imgl =
  let result = Image.create_rgb ~alpha:true width height in
  Image.fill_rgb ~alpha:0 result 0 0 0 ;
  List.iter (fun (img, (dx, dy)) ->
    for x = 0 to img.width - 1 do
      for y = 0 to img.height - 1 do
        let (r, g, b, a) = read_image img (x, y) in
        Image.write_rgba result (x + dx) (y + dy) r g b a
      done
    done) imgl ;
  make_image result

let combine tl =
  assert (List.for_all (fun (t, _dxy) -> check_size t) tl) ;
  (* The dimension of a single automaton. *)
  let dim_single t =
    let image = image t in
    (image.width, image.height) in
  (* The dimension of the final image and the difference to the offset coordinates. *)
  let (dim, d) =
    let (min_x, min_y, max_x, max_y) =
      List.fold_left (fun (min_x, min_y, max_x, max_y) (t, (dx, dy)) ->
          let (width, height) = dim_single t in
          (min min_x dx, min min_y dy, max max_x (dx + width), max max_y (dy + height)))
        (0, 0, 0, 0) tl in
    ((max_x - min_x, max_y - min_y), (- min_x, - min_y)) in
  (* We apply the difference to the offsets. *)
  let tl = List.map (fun (t, (dx, dy)) -> (t, (dx + fst d, dy + snd d))) tl in
  (* The number of states in the new automaton. *)
  let num_states = List.fold_left (fun n (t, _) -> n * Array.length t.delta) 1 tl in
  (* Given in which state all the automatons in t are, return the corresponding state in the
    resulting automaton. *)
  let state_in sl =
    fst (List.fold_left2 (fun (current, size) (t, _) s ->
      let len = Array.length t.delta in
      assert (s < len) ;
      (current + s * size, size * len)) (0, 1) tl sl) in
  (* The reciprocal of state_in. *)
  let state_from i =
    let (sl, i, size) =
      List.fold_left (fun (sl, i, size) (t, _) ->
        let len = Array.length t.delta in
        assert (size mod len = 0) ;
        let size = size / len in
        let s = i / size in
        assert (s < len) ;
        let i = i mod size in
        (s :: sl, i, size)) ([], i, num_states) (List.rev tl) in
    assert (i = 0) ;
    assert (size = 1) ;
    sl in
  let delta =
    Array.init num_states (fun i ->
      let sl = state_from i in
      let image =
        combine_images dim (List.map2 (fun (t, d) s ->
          let (image, _next) = t.delta.(s) in
          (image, d)) tl sl) in
      let fetch_event e =
        let state =
          state_in (List.map2 (fun (t, _) s ->
            let (_image, next) = t.delta.(s) in
            let (_time, st) = Event.fetch next e in
            st) tl sl) in
        (* TODO: It is really not clear which time we should associate to the combined state. *)
        let time =
          List.fold_left2 (fun n (t, _) s ->
            let (_image, next) = t.delta.(s) in
            let (time, _st) = Event.fetch next e in
            if time = Time.infinity then n
            else max n time) Time.zero tl sl in
        (time, state) in
      (image, Event.create_map fetch_event)) in {
    state = state_in (List.map (fun (t, _) -> t.state) tl) ;
    time = List.fold_left (fun n (t, _) -> max n t.time) Time.zero tl ;
    delta
  }

