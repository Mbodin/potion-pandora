
(* The main game engine. *)

module Engine (I : Interface.T) : sig

  (* Load a level. *)
  val load : Store.t -> unit

  (* Make the scene move one step, and display it, but without dealing with specifics
    for the player. *)
  val step : unit -> unit

end

