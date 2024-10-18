
(* Functors to organise the screen. *)

(* This functor splits the screen into two independent interfaces.
  Both initialisations should happen before any other operation. *)
module SplitVertical : Interface.T -> sig
    module Up : Interface.T
    module Down : Interface.T
  end

