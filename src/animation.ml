
type subimage = {
  width : int ;
  height : int ;
  position : int * int ;
}

let make_subimage width height position = { width ; height ; position }

type event =
  | MoveLeft
  | MoveRight
  | LookDown
  | LookUp
  | LookBehind
  | Touch
  | Wind
  | RandomFlicker
  | RandomFrequent
  | RandomNormal
  | RandomRare
  | Tau

(* A module for events for the Map module. *)
module Event = struct
  type t = event
  let compare = compare
end

module EMap = Map.Make (Event)
module ESet = Set.Make (Event)

(* The representation of an object state. *)
type state = int

(* A type to count time, as a number of cycles.
  The type is abstracted away to avoid mixing with the state. *)
module Time : sig
  type t
  val zero : t
  val incr : t -> t
  val of_int : int -> t
  val compare : t -> t -> int
  val (<) : t -> t -> bool

  (* This converts a time in seconds into a time in number of frames. *)
  val of_seconds : float -> t

end = struct
  type t = int
  let zero = 0
  let incr t =
    if t = max_int then max_int
    else t + 1
  let of_int t = t
  let compare = compare
  let (<) = (<)

  (* Number of frame per seconds. *)
  let frame_per_seconds = 60

  let of_seconds s =
    Float.to_int (Float.round (s *. Float.of_int frame_per_seconds))

end
type time = Time.t

(* The internal automaton of an object.
  Each state is an array index.
  It then stores the associated image, as well as the next step for each event.
  The next state is itself associated *)
type automaton = (subimage * (event -> (time * state))) array

type t = {
  state : state (* The current state of the automaton. *) ;
  time : time (* The number of cycles that were spent on this state without taking any transition. *) ;
  delta : automaton (* How this state responds to events. *)
}

let image t =
  let (image, _next) = t.delta.(t.state) in
  image

let next t e =
  let (_image, next) = t.delta.(t.state) in
  let (time, st) = next e in
  if Time.(t.time < time) then
    { t with time = Time.incr t.time }
  else { t with state = st ; time = Time.zero }

type sequence = (subimage * float) list

let loop s =
  let len = List.length s in
  assert (len > 0) ;
  let automaton =
    Array.of_list (List.mapi (fun index (image, time) ->
      (image, fun _ -> (Time.of_seconds time, (index + 1) mod len))) s) in {
    delta = automaton ;
    time = Time.zero ;
    state = 0
  }

let static i = loop [(i, 1.)]

let react t es s =
  let len = List.length s in
  if len = 0 then t
  else
    let es = ESet.of_list es in
    let shift = Array.length t.delta in
    let sequence =
      Array.of_list (List.mapi (fun index (image, time) ->
        let next =
          if index = len - 1 then t.state
          else shift + index + 1 in
        (image, fun e ->
          if ESet.mem e es then (Time.zero, shift)
          else (Time.of_seconds time, next))) s) in
    let automaton =
      Array.append
        (Array.map (fun (image, next) ->
          (image, fun e ->
            if ESet.mem e es then (Time.zero, shift)
            else next e)) t.delta)
        sequence in {
      delta = automaton ;
      time = Time.zero ;
      state = 0
    }


