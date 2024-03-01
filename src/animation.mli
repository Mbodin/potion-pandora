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

(* The current image of the object. *)
val image : t -> subimage

(* Respond to an event. *)
val next : t -> Event.t -> t

(* An animation sequence, composed of subimages associated with a time (in seconds). *)
type sequence = (subimage * float) list

(* An object with a single image, never changing. *)
val static : subimage -> t

(* An object doing a single looping sequence. *)
val loop : sequence -> t

(* Like its first argument except when one of the provided event is fired:
  it then plays the sequence then goes back to its initial state.
  The skip argument states whether the sequence will be skipped if the state after it
  (in this case the initial state of t) would react to the current event (that is,
  the associated time condition is zero and the transition makes it jump to a different state. *)
val react : t -> Event.t list -> ?skip:bool -> sequence -> t

(* Switch between two variant automatons for the same object, with events to go from
  one to the other.
  [switch t1 [e1] s1 t2 [e2] s2] behaves like [t1] until [e1] occurs.
  It then applies the sequence [s1], then act like [t2]… until [e2] occurs.
  It then applies the sequence [s2] and goes back to its initial behavior. *)
val switch : t -> Event.t list -> ?skip:bool -> sequence ->
             t -> Event.t list -> ?skip:bool -> sequence -> t

