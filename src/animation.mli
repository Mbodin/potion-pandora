(* This modules deals with storing the animation aspect (images, dynamics, etc.)
  of the displayed objects. *)

(* A static image, ready to be displayed. *)
type image

(* Given the width, height, and (x, y) position of the image part in the program data,
  store the corresponding subimage.
  The optional argument is there to extract an image from another image then the default
  bundle for Bundled_image. *)
val make_subimage : ?bundle:Image.image -> int -> int -> (int * int) -> image

(* Given an image, reads its (r, g, b, a) values at the provided coordinate. *)
val read_image : image -> (int * int) -> (int * int * int * int)

(* Return the dimensions (width, height) of the image. *)
val image_dimensions : image -> (int * int)


(* A representation of an object as an automaton.
  It can respond to events to update its internal image. *)
type t

(* The current image of the object. *)
val image : t -> image

(* Check that all the images of the animation have the exact same size. *)
val check_size : t -> bool

(* Respond to an event. *)
val send : t -> Event.t -> t

(* Whether an animation listens to a particular event.
  If an animation doesn't listen to an event, then its behaviour won't change when
  getting this event. *)
val listen : t -> Event.t -> bool

(* An animation sequence, composed of subimages associated with a time (in seconds). *)
type sequence = (image * float) list

(* An object with a single image, never changing. *)
val static : image -> t

(* An object doing a single looping sequence. *)
val loop : sequence -> t

(* Like its first argument except when one of the provided event is fired:
  it then plays the sequence then goes back to its initial state.
  The skip argument states whether the sequence will be skipped if the state after it
  (in this case the initial state of t) would react to the current event (that is,
  the associated time condition is zero and the transition makes it jump to a different state.
  The restart argument states whether a firing of such an event in the middle of the sequence
  should restart the sequence. *)
val react : t -> Event.t list -> ?skip:bool -> ?restart:bool -> sequence -> t

(* Switch between two variant automatons for the same object, with events to go from
  one to the other.
  [switch t1 [e1] s1 t2 [e2] s2] behaves like [t1] until [e1] occurs.
  It then applies the sequence [s1], then act like [t2]… until [e2] occurs.
  It then applies the sequence [s2] and goes back to its initial behavior. *)
val switch : t -> Event.t list -> ?skip:bool -> sequence ->
             t -> Event.t list -> ?skip:bool -> sequence -> t

