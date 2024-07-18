
(* Ad-hoc definitions for the images of the game. *)

open Animation

let range a b =
  List.init (b - a + 1) (fun c -> c + a)

(* Converts the coordinates of Images_coords into an image. *)
let from_coords ~bundle ((width, height), (x, y)) =
  Animation.make_subimage ~bundle width height (x, y)

(* Call the above conversion function on the nth element of a list of coordinates,
  as given in Images_coords. *)
let from ?(bundle = Bundled_image.image) coords i : Animation.image =
  assert (i < List.length coords) ;
  from_coords ~bundle (List.nth coords i)

(* Directly converts a list. *)
let fromlist ?(bundle = Bundled_image.image) coords : Animation.image list =
  List.map (from_coords ~bundle) coords

(* Convert a list of images into a sequence staying that time per picture. *)
let to_sequence time l =
  List.map (fun i -> (i, time)) l

(* An object that only reacts to wind/explosions. *)
let rwind img s =
  react (static img) Event.[Wind; Explode] s

(* The palette is an exception, as it is provided as a single image (with a single line).
  We here split it into several one-pixel images. *)
let palette =
  match Images_coords.palette with
  | [((width, height), (x, y))] ->
    assert (height = 1) ;
    List.init width (fun dx ->
      ((1, height), (x + dx, y)))
  | _ -> assert false

let vert_fonce = Filter.decolor ~pattern:(from palette 3) Bundled_image.image
let vert_tres_fonce = Filter.decolor ~pattern:(from palette 2) Bundled_image.image

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

  let perso = static normal (* TODO: automaton *)

end

let potion = loop (to_sequence 0.1 (fromlist Images_coords.potion))

let plante1 =
  let mk = from Images_coords.plante1 in
  rwind (mk 0) (to_sequence 0.2 [mk 1])

let plante1_sombre =
  let mk = from ~bundle:vert_fonce Images_coords.plante1 in
  rwind (mk 0) (to_sequence 0.2 [mk 1])

let plante1_tres_sombre =
  let mk = from ~bundle:vert_tres_fonce Images_coords.plante1 in
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

