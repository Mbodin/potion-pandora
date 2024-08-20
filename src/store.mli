
(* A structure storing all the elements in a level. *)
type t

(* Some kind of index to an object within the store. *)
type obj

(* Create an empty level. *)
val create : unit -> t

(* Add an animation object at the provided position (zero being the left top pixel of the
  animation image) to a store.
  It internally updates the store and returns an index to the object.
  The optionnal level can be used to place an object in the foreground (negative values)
  or the background (positive values).
  Note that negative positions are fine. *)
val add : t -> Animation.t -> ?level:int -> Projection.game_coords -> obj

(* Remove an object from the store. *)
val remove : t -> obj -> unit

(* Add bird animations to be picked from when spawning. *)
val add_birds : t -> Animation.t list -> unit

(* Add a location for birds to stand.
  Bird will occasionnally get to one of these. *)
val add_bird_location : t -> ?level:int -> Projection.game_coords -> unit

(* Given two screen coordinates, return all the objects that are within (or touching)
  the associated rectangle.
  The objects will be listed in topological order.
  If a level is provided, only the objects from this specific level are returned. *)
val all : t -> ?level:int -> Projection.screen_coords -> Projection.screen_coords -> obj list

(* Given an object, return its coordinates. *)
val get_coords : t -> obj -> Projection.game_coords

(* Given an object, return its level. *)
val get_level : t -> obj -> int

(* Given an object, return its animation object. *)
val get_display : t -> obj -> Animation.t

(* Move an object to a new position.
  By default it will trigger Touch events to objects in-passing: the ghost option disables it.
  It returns the number of turns this object will get there.
  The fast option makes the object go three times faster. *)
val move : t -> obj -> ?ghost:bool -> ?fast:bool -> Projection.game_coords -> int

(* Send an event Explode around all objects at this distance to this position. *)
val explode : t -> ?level:int -> Projection.game_coords -> int -> unit

(* Send an event to this object.
  By itself, the store already takes care of several events: by default, these events are
  rejected, but the safe option disables this check. *)
val send : t -> obj -> ?safe:bool -> Event.t -> unit

(* Make a single time step to the whole scene.
  It takes as argument the screen coordinates, as the displayed objects are treated slightly
  differently than the others. *)
val step : t -> Projection.screen_coords -> Projection.screen_coords -> unit

