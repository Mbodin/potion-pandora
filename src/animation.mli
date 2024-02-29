(* This modules deals with storing the animation aspect (images, dynamics, etc.)
  of the displayed objects. *)

(* In this program, all images are bundled up into a single large image.
  This, any part is actually a subpart of this image.
  This type stores the corresponding coordinates. *)
type subimage

(* Given the width, height, and (x, y) position of the image part in the program data,
  store the corresponding subimage. *)
val make_subimage : int -> int -> (int * int) -> subimage


(* A representation of an object as an automaton.
  It can respond to events to update its internal image. *)
type t

(* The events from which the automaton responds. *)
type event =
  (* The object is moving. *)
  | MoveLeft
  | MoveRight
  (* Some user actions. *)
  | LookDown
  | LookUp
  | LookBehind
  (* The object was touched by another moving object. *)
  | Touch
  (* The wind reached the object (occurs randomly and propagates from left to right). *)
  | Wind
  (* Randomly occurring event pulsing at different frequencies. *)
  | RandomFlicker (* Several times per second. *)
  | RandomFrequent (* About every second. *)
  | RandomNormal (* Every 5–10 seconds. *)
  | RandomRare (* More than 30 seconds. *)
  (* Time simply passes (it's a no-event). *)
  | Tau

(* The current image of the object. *)
val image : t -> subimage

(* Respond to an event. *)
val next : t -> event -> t

(* An animation sequence, composed of subimages associated with a time (in seconds). *)
type sequence = (subimage * float) list

(* An object with a single image, never changing. *)
val static : subimage -> t

(* An object doing a single looping sequence. *)
val loop : sequence -> t

(* Like its first argument except when one of the provided event is fired:
  it then plays the sequence then goes back to its initial state. *)
val react : t -> event list -> sequence -> t

