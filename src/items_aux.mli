(* Fonctions to be used in the Items module in order to define objects *)

(* All the numbers from the first to the second, included. *)
val range : int -> int -> int list

(* Some functions imported from [Animation]. *)
val static : Subimage.t -> Animation.t
val loop : Animation.sequence -> Animation.t

(* Plays a sequence once, then stays transparent. *)
val once : Animation.sequence -> Animation.t

(* Takes the nth coordinates of a list from Images_coords and convert it into an image. *)
val from : ?bundle:Image.image -> Images_coords.coords list -> int -> Subimage.t

(* Same than [from], but directly converts the full list. *)
val fromlist : ?bundle:Image.image -> Images_coords.coords list -> Subimage.t list

(* Convert a list of images into a sequence staying that time per picture. *)
val to_sequence : float -> Subimage.t list -> Animation.sequence

(* Given a function [mk] building an image and a list of arguments for [mk], build
  the corresponding sequence. *)
val mk_sequence : float -> ('a -> Subimage.t) -> 'a list -> Animation.sequence

(* Build a sequence from the time between frame, a function [mk] building the nth image,
  and the minimum and maximum values that should be provided to this function to build
  the sequence. *)
val mk_sequence_range : float -> (int -> Subimage.t) -> int -> int -> Animation.sequence

(* Build triangles from textures (useful for roofs). *)
val triangle_left : Subimage.t -> Animation.t
val triangle_right : Subimage.t -> Animation.t

(* An object that only reacts to wind and explosions, with the provided animation. *)
val rwind : Subimage.t -> Animation.sequence -> Animation.t

(* An object that reacts to explosions, to get into a new final state. *)
val rexplosions : Subimage.t -> Animation.sequence -> Subimage.t -> Animation.t

(* When the raw image list contains a single static image. *)
val static_unique : Images_coords.coords list -> Animation.t

(* Useful function to use [Animation.nd_transitions]: given the current state and a list of
   weighed next transitions, it only reacts to [Event.Tau]. *)
val only_tau : 'a -> (int * Animation.sequence * 'a) list -> Event.t -> (int * Animation.sequence * 'a) list

(* Extract all the elements of a sequence from a provided index. *)
val seq_from : Animation.sequence -> int -> Animation.sequence

