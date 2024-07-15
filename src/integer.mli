
(* Module declaration for integers. *)
type t = int

(* The comparison function instantiated to integers. *)
val compare : t -> t -> int

(* The euclidean division, instead of the truncated division /.
  In contrary to the truncated division which biases towards 0,
  the euclidean division always biases towards negative infinity. *)
val div : t -> t -> t

