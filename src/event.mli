(* Events from which animation object can respond to. *)

type t =
  (* The object is moving. The event is regularly repeated during the move. *)
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
  (* Time simply passes (it's a no-event). *)
  | Tau

(* Comparison function.
  The more important events are considered lower than the less important ones. *)
val compare : t -> t -> int

(* A total map from events to something. *)
type 'a map

(* Create a map from a function. *)
val create_map : (t -> 'a) -> 'a map

(* Get the value associated to an event. *)
val fetch : 'a map -> t -> 'a

