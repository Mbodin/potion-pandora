
(* Decode a type from a string, assuming that the string is valid. *)
val decode : 'a Save.t -> string -> 'a

(* Converts a character into a list of 8 bits.
 It is meant to be an internal function, left here to test its interaction with the Write module. *)
val char_to_bits : char -> bool list


(* Number of bits required to store n different values. *)
val log2 : int -> int

