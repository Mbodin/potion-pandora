(* Events from which animation object can respond to. *)

type t =
  (* The object is moving (at least until the next Tau). *)
  | MoveLeft
  | MoveRight
  | Fall
  (* Reaction to other objects. *)
  | Explode (* The object was touched by an explosion. *)
  | Touch (* The object was touched by another moving object. *)
  (* Some story-guided actions. *)
  | LookDown
  | LookUp
  | LookBehind
  (* The wind reached the object (occurs randomly and propagates from left to right). *)
  | Wind
  (* Randomly occurring event pulsing at different frequencies. *)
  | RandomRare (* More than 30 seconds. *)
  | RandomNormal (* Every 5–10 seconds. *)
  | RandomFrequent (* About every second. *)
  | RandomFlicker (* Several times per second. *)
  (* Time simply passes.
    It's not a no-event: it specifically states that time passed.
    Intuitively, at each turn an animation will receive several events, but exactly one Tau. *)
  | Tau

(* Comparison function. *)
val compare : t -> t -> int

(* A total map from events to something. *)
type 'a map

(* Create a map from a function. *)
val create_map : (t -> 'a) -> 'a map

(* Get the value associated to an event. *)
val fetch : 'a map -> t -> 'a

