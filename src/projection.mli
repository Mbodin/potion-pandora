
(* Given a level and a coordinate, return the screen coordinates where this object
  will be displayed. *)
val to_screen : int -> (int * int) -> (int * int)

(* Inverse of the above projection function: given a level and a screen coordinate,
  return the corresponding game coordinates.
  Note that due to projection shifts, it might not get back exactly to the original
  coordinates. *)
val from_screen : int -> (int * int) -> (int * int)

