
(* The main game engine. *)

module Engine (I : Interface.T) : sig

  (* The interface as initialised by the engine. *)
  val interface : I.t I.m

  (* Load a level. *)
  val load : Store.t -> unit

  (* Make the scene move one step, and display it, but without dealing with specifics
    for the player. *)
  val step : unit -> unit I.m

end

