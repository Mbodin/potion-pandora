
(* Various filters to modify images or animations. *)

(* Force all the opaque pixels of an image to follow a provided pattern
  (typically a single pixel image to fully decolorise the image into this color,
  but it can also be a gradient pattern). *)
val decolor : pattern:Animation.image -> Image.image -> Image.image

(* From a static image, add a water-shimmering effect to it. *)
val shimmer : ?quantity:int -> ?amplitude:int -> ?duration:int -> Image.image -> Animation.image list

(* Given a list of pattern and height function, as well as dimensions, create an
  image with these dimensions following the patterns according to each function:
  from below to the first height (defined according to the first function for each
  x) according to the first pattern, from then to the second height according to
  the second, etc. *)
val curve : (Animation.image * (int -> int)) list -> int * int -> Image.image

(* Create a rectangle with this pattern. *)
val rectangle : Animation.image -> int * int -> Image.image

