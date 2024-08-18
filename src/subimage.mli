(* This module defines a type for parts of images (typically the one defined in Image_bundle
  and Image_coords generated modules). *)

(* A static image, ready to be displayed.
  Note that the (0, 0) coordinate of an image is on its top-left corner: y-coordinates are
  not reversed like the rest of the program. *)
type t

(* Conversion of types of images. *)
val from_image : Image.image -> t

(* Given the width, height, and (x, y) position of the image part in the program data,
  store the corresponding subimage.
  The optional argument is there to extract an image from another image then the default
  bundle for Bundled_image. *)
val make : ?bundle:Image.image -> int -> int -> (int * int) -> t

(* Given an image, reads its (r, g, b, a) values at the provided coordinate. *)
val read : t -> (int * int) -> (int * int * int * int)

(* Return the dimensions (width, height) of the image. *)
val dimensions : t -> (int * int)

(* Combine a list of images with an offset into a single image. *)
val combine : (t * (int * int)) list -> t

(* Create an image from the provided image of the provided dimensions.
  These dimensions must be larger than the image.
  The image will be centered. *)
val enlarge : t -> (int * int) -> t
