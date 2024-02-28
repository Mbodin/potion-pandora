(* This modules deals with storing the animation aspect (images, dynamics, etc.)
  of the displayed objects. *)

(* In this program, all images are bundled up into a single large image.
  This, any part is actually a subpart of this image.
  This type stores the corresponding coordinates. *)
type subimage

(* Given the width, height, and (x, y) position of the image part in the program data,
  store the corresponding subimage. *)
val make_subimage : int -> int -> (int * int) -> subimage

