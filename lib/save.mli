
(* A type to represent an RGBA color (between 0 and 255 included). *)
type color = (int * int * int * int)

(* This type represents all the steps to save/load a type from a string. *)
type 'a t =
  | Unit : unit t
  | Bool : bool t
  | Int : int t
  | String : string t
  | Seq : 'a t * 'b t -> ('a * 'b) t
  | Option : 'a t -> 'a option t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | AddColor : 'a t -> (color * 'a) t (* Add a color to the available colors within. *)
  | Image : Image.image t (* All the image's (non-transparent) colors must have been encoded before. *)
  | Base64 : 'a t -> 'a t
  | Compress : 'a t -> 'a t

