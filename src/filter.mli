(* Various filters to modify images. *)

(* Force all the opaque pixels of an image to follow a provided pattern
  (typically a single pixel image to fully decolorise the image into this color,
  but it can also be a gradient pattern). *)
val decolor : pattern:Subimage.t -> Subimage.t -> Subimage.t

(* From a static image, add a water-shimmering effect to it.
  The quantity represents the amount of waves in the animation.
  The amplitude represents the size of the waves.
  The duration is the total nomber of frame of the final animation.
  The direction is the direction of travel of each wave. *)
val shimmer : ?quantity:int -> ?amplitude:int -> ?duration:int -> ?direction:float*float -> Subimage.t -> Subimage.t list

(* Given a list of pattern and height function, as well as dimensions, create an
  image with these dimensions following the patterns according to each function:
  from below to the first height (defined according to the first function for each
  x) according to the first pattern, from then to the second height according to
  the second, etc.
  The functions must increase along the list: for all x, if f2 is after f1 in the list,
  then f2 x >= f1 x >= 0. *)
val curve : (Subimage.t * (int -> int)) list -> int * int -> Subimage.t

(* Create a rectangle filled with this pattern. *)
val rectangle : Subimage.t -> int * int -> Subimage.t

(* Create rectangle/isosceles triangles of the provided dimension filled with the provided pattern. *)
val triangle_lower_left : Subimage.t -> int -> Subimage.t
val triangle_lower_right : Subimage.t -> int -> Subimage.t

(* Create an image like the provided image, but flipped. *)
val flip_horizontally : Subimage.t -> Subimage.t
val flip_vertically : Subimage.t -> Subimage.t

(* Create an image like the provided image, but flipped along the diagonal axes (x becomes y
  and vice-versa). *)
val flip_diagonally : Subimage.t -> Subimage.t

(* An entirely transparent image (with only pixel), useful to serve as an empty pattern. *)
val transparent : Subimage.t

(* Create a kind of inversion effect, in which each pixel will be mapped into the furthest
  colors in the palette (or the image itself if no palette is provided). *)
val invert : ?palette:Subimage.t -> Subimage.t -> Subimage.t

(* Return the RGB color of the brightest/darkest non-transparent pixel of the image. *)
val brightest : Subimage.t -> (int * int * int)
val darkest : Subimage.t -> (int * int * int)

(* Provided a background pattern and an image, update all its transparent pixels to fit
  the background pattern. *)
val add_background : Subimage.t -> Subimage.t -> Subimage.t

(* From an image and ornament specification, add the ornaments around the image. *)
val add_ornaments :
  Subimage.t (* Image to be ornamented. *) ->
  Subimage.t (* Pattern to fill the inside of the ornament. *) ->
  Subimage.t -> Subimage.t -> Subimage.t -> Subimage.t -> (* Angles of the ornament (NW, NE, SW, SE). *)
  Subimage.t -> Subimage.t -> Subimage.t -> Subimage.t -> (* Sides of the ornament (W, E, N, S). *)
  Subimage.t

