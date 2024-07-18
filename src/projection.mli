
(* Coordinates of a pixel on screen. *)
type screen_coords = int * int

(* Actual coordinates of an object, without any parallax shift. *)
type game_coords = int * int

(* Given a level and a coordinate, return the screen coordinates where this object
  will be displayed. *)
val to_screen : int -> game_coords -> screen_coords

(* Inverse of the above projection function: given a level and a screen coordinate,
  return the corresponding game coordinates.
  Note that due to projection shifts, it might not get back exactly to the original
  coordinates. *)
val from_screen : int -> screen_coords -> game_coords

