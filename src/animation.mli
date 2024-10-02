(* This module deals with storing the animation aspect of the displayed objects. *)

(* Number of expected frame per seconds. *)
val frames_per_second : int


(* A representation of an object as an automaton.
  It can respond to events to update its internal image. *)
type t

(* The current image of the object. *)
val image : t -> Subimage.t

(* Check that all the images of the animation have the exact same size. *)
val check_size : t -> bool

(* Create a copy of the animation object in which all the images have the same size. *)
val force_same_size : t -> t

(* Respond to an event. *)
val send : t -> Event.t -> t

(* Whether an animation listens to a particular event.
  If an animation doesn't listen to an event, then its behaviour won't change when
  getting this event. *)
val listen : t -> Event.t -> bool

(* For debugging purposes, print-out a GraphViz visualisation of the automaton.
  The [quiet] option removes the labels on the vertices and the vertices with the same origin and target.
  The [event] option marks a particular event with a special color, to help visualising. *)
val print : ?quiet:bool -> ?event:Event.t -> t -> string

(* An animation sequence, composed of subimages associated with a time (in seconds). *)
type sequence = (Subimage.t * float) list

(* An object with a single image, never changing. *)
val static : Subimage.t -> t

(* An object doing a single looping sequence. *)
val loop : sequence -> t

(* Plays a sequence, then stays in the other animation. *)
val prefix : sequence -> t -> t

(* Like its first argument except when one of the provided event is fired:
  it then plays the sequence then goes back to its initial state.
  The skip argument states whether the sequence will be skipped if the state after it
  (in this case the initial state of t) would react to the current event (that is,
  the associated time condition is zero and the transition makes it jump to a different state.
  The restart argument states whether a firing of such an event in the middle of the sequence
  should restart the sequence. *)
val react : t -> Event.t list -> ?skip:bool -> ?restart:bool -> sequence -> t

(* This is similar to [react] expect that instead of going back into its initial state,
  it jumps into another automaton and stays there. *)
val change_with : t -> Event.t list -> ?skip:bool -> sequence -> t -> t

(* Switch between two variant automatons for the same object, with events to go from
  one to the other.
  [switch t1 [e1] s1 t2 [e2] s2] behaves like [t1] until [e1] occurs.
  It then applies the sequence [s1], then act like [t2]… until [e2] occurs.
  It then applies the sequence [s2] and goes back to its initial behavior. *)
val switch : t -> Event.t list -> ?skip:bool -> sequence ->
             t -> Event.t list -> ?skip:bool -> sequence -> t

(* Explicitely define an automaton as a transition system.
  The first argument is the initial state (it can be any type, the (=) equality will be used on it),
  the second is the transition function: given a state, return a looping sequence as well as a
  function for its transitions. This function is such that given an event, return a sequence to be
  played followed by its new state. *)
val transitions : 'a -> ('a -> sequence * (Event.t -> sequence * 'a)) -> t

(* Like [transitions], but non deterministic: at each step, we provide a list of sequence and possible
  next state, along with an integer weight, to be chosen randomly. *)
val nd_transitions : 'a -> ('a -> sequence * (Event.t -> (int * sequence * 'a) list)) -> t

(* Combine a list of automatons with an offset into a single automaton.
  The predicate check_size must hold for each of the input automatons. *)
val combine : (t * (int * int)) list -> t

(* Force all the opaque pixels of an animation to follow a provided pattern
  (typically a single pixel image to fully decolorise the image into this color,
  but it can also be a gradient pattern). *)
val decolor : pattern:Subimage.t -> t -> t

