
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

  include T with type 'a m = 'a M.m

  (* Symbol for sequence. *)
  val ( %% ) : unit m -> 'a m -> 'a m

  (* For-loops within monads: [for_ min max f] behaves like [for i = min to max do f i done] but
    within the monad. *)
  val for_ : int -> int -> (int -> unit M.m) -> unit M.m

  (* Iteration on lists within monads. *)
  val iter_ : ('a -> unit M.m) -> 'a list -> unit M.m
  val iteri_ : (int -> 'a -> unit M.m) -> 'a list -> unit M.m

  (* Mapping on lists within monads. *)
  val list_map_ : ('a -> 'b M.m) -> 'a list -> 'b list M.m
  val list_mapi_ : (int -> 'a -> 'b M.m) -> 'a list -> 'b list M.m

end

(* Simple monads *)

module Pure : T with type 'a m = 'a

