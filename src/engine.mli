
(* The width and height of the screen. *)
module type Display = sig
  val width : int
  val height : int
end

(* Given a level and a coordinate, return the screen coordinates where this object
  will be displayed. *)
val projection : int -> (int * int) -> (int * int)

(* Inverse of the projection function: given a level and a screen coordinate,
  return the corresponding game coordinates. *)
val inverse_projection : int -> (int * int) -> (int * int)

(* The main game engine.
  It deals with every object move, except special cases (like player or scripts).
  It can only step through the store, and is meant to be intertwinned with some
  code to deal with these special cases (see the Game module where the main loop
  is located). *)

module Engine (_ : Display) (I : Interface.T) : sig

  (* The interface as initialised by the engine. *)
  val interface : I.t I.m

  (* Load a level. *)
  val load : Store.t -> unit

  (* Make the scene move one step, and display it, but without dealing with specifics
    for the player. *)
  val step : unit -> unit I.m

end

