
(* A type to represent a color palette, as an array of RGBA colors (between 0 and 255 included). *)
type palette = (int * int * int * int) array

(* This type represents all the steps to save/load a type from a string. *)
type 'a t =
  | Unit : unit t
  | Bool : bool t
  | Int : int t
  | Float : float t
  | String : string t
  | Seq : 'a t * 'b t -> ('a * 'b) t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Image : (palette * Image.image) t

