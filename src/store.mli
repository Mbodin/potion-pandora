
(* A structure storing all the elements in a level. *)
type t

(* Some kind of index to an object within the store. *)
type obj

(* Create an empty level. *)
val create : unit -> t

(* Add an animation object at the provided position (zero being the left top pixel of the
  animation image) to a store.
  It internally updates the store and returns an index to the object.
  The optionnal level can be used to place an object in the foreground (positive values)
  or the background (negative values): the larger the value, the less synchronised with
  the base level it will be.
  Note that negative positions are fine. *)
val add : t -> Animation.t -> ?level:int -> (int * int) -> obj

(* Remove an object from the store. *)
val remove : t -> obj -> unit

(* Given two coordinates, return all the objects that are within (or touching) the associated
  rectangle, as a pair of its coordinate and its current animation object.
  The objects will be listed in topological order. *)
val all : t -> (int * int) -> (int * int) -> ((int * int) * Animation.t) list

(* Move an object to a new position.
  By default it will trigger Touch events to objects in-passing: the ghost option disables it.
  It returns the number of turns this object will get there.
  The fast option makes the object go three times faster. *)
val move : t -> obj -> ?ghost:bool -> ?fast:bool -> (int * int) -> int

(* Send an event Explode around all objects at this distance to this position. *)
val explode : t -> ?level:int -> (int * int) -> int -> unit

(* Send an event to this object.
  By itself, the store already takes care of several events: by default, these events are
  refused. The safe option disables this check. *)
val send : t -> obj -> ?safe:bool -> Event.t -> unit

(* Make a single time step to the whole scene. *)
val step : t -> unit

