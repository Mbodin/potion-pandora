
(* Ad-hoc definitions for the images of the game. *)

open Animation

let range a b =
  List.init (b - a + 1) (fun c -> c + a)

let from img i =
  List.nth img i

(* Convert a list of images into a sequence staying that time per picture. *)
let to_sequence time l =
  List.map (fun i -> (i, time)) l

(* An object that only reacts to wind/explosions. *)
let rwind img s =
  react (static img) Event.[Wind; Explode] s

module Perso = struct

  let mk = from Images_coords.perso
  let range a b = List.map mk (range a b)

  let normal = mk 0
  let inspiration = mk 1
  let dos = mk 2
  let vent = mk 3
  let regard_haut = mk 4
  let regard_bas = mk 5
  let clignement = mk 6
  let gratte = range 7 8
  let avance = range 9 14
  let protection = mk 15
  let chute = range 16 17
  let atterissage = mk 18

  (* TODO: automaton *)

end

let potion = loop (to_sequence 0.1 Images_coords.potion)

let plante1 =
  let mk = from Images_coords.plante1 in
  rwind (mk 0) (to_sequence 0.2 [mk 1])

let buisson =
  let mk = from Images_coords.buisson in
  rwind (mk 0) (to_sequence 0.2 [mk 1])

let fleur1 =
  let mk = from Images_coords.fleur1 in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fleur2 =
  let mk = from Images_coords.fleur2 in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fenetre =
  let mk = from Images_coords.fenetre in
  react (static (mk 0)) Event.[RandomFlicker] (to_sequence 0.05 [mk 1])

