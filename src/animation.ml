
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
  assert (fst position + width < bundle.Image.width) ;
  assert (snd position + height < bundle.Image.height) ;
  { width ; height ; position ; picture = bundle }

let read_image img (x, y) =
  assert (x >= 0 && y >= 0) ;
  assert (x < img.width && y < img.height) ;
  let (px, py) = img.position in
  Image.read_rgba img.picture (x + px) (y + py) (fun r g b a -> (r, g, b, a))


module EMap = Map.Make (Event)
module ESet = Set.Make (Event)

(* The representation of an object state. *)
type state = int

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

end = struct

  type t = int
  let zero = 0
  let infinity = max_int
  let incr t =
    if t = max_int then max_int
    else t + 1
  let (<) = (<)

  let of_seconds s =
    Float.to_int (Float.round (s *. Float.of_int frames_per_second))

end
type time = Time.t

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
  if e = Tau && Time.(t.time < time) then
    { t with time = Time.incr t.time }
  else { t with state = st ; time = Time.zero }

let listen t e =
  let (_image, next) = t.delta.(t.state) in
  let (time, st) = Event.fetch next e in
  (st <> t.state && not Time.(t.time < time))

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
              if time = Time.zero && state' <> state then
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

