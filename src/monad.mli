
(* Utility functions with monads. *)

(* A monad signature. *)
module type T = sig
  type _ m

  (* The monadic operators. *)
  val return : 'a -> 'a m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
end

(* Operators on monads *)
module Ops (M : T) : sig

  (* For-loops within monads: [for_ min max f] behaves like [for i = min to max do f i done] but
    within the monad. *)
  val for_ : int -> int -> (int -> unit M.m) -> unit M.m

  (* Iteration on lists within monads. *)
  val iter_ : ('a -> unit M.m) -> 'a list -> unit M.m

end

