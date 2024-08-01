
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

(* Build a sequence from the time between frame, a function [mk] building the nth image,
  and the minimum and maximum values that should be provided to this function to build
  the sequence. *)
let mk_sequence time mk min max =
  assert (max >= min) ;
  to_sequence time (List.init (1 + max - min) (fun i -> mk (i + min)))

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

(* * Personnages *)

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

let crash = loop (to_sequence 0.7 (fromlist Images_coords.crash))

let emmele_cable =
  let mk = from Images_coords.emmele_cable in
  loop (to_sequence 0.4 [mk 0; mk 1; mk 2; mk 1])

let enfant =
  (* TODO: Parfois, faire une pause sur la position initiale. *)
  loop (to_sequence 0.2 (fromlist Images_coords.enfant))

(* TODO: aveugle *)
(* TODO: chauffard *)
(* TODO: chercheuse *)

(* TODO: chevre *)
(* TODO: coq *)

(* * Objets *)

let potion = loop (to_sequence 0.1 (fromlist Images_coords.potion))

let petite_potion = loop (to_sequence 0.1 (fromlist Images_coords.petite_potion))

let caisse = static (from Images_coords.caisse 0)

let cane = static (from Images_coords.cane 0)

let cerf_volant =
  loop (to_sequence 0.1 (fromlist Images_coords.cerf_volant)) (* TODO: Add randomness *)

let chariot = static (from Images_coords.chariot 0)

let echelle1 = static (from Images_coords.echelle 0)
let echelle2 = static (from Images_coords.echelle 1)
let echelle3 = static (from Images_coords.echelle 2)

let plot1 = static (from Images_coords.plot 0)
let plot2 = static (from Images_coords.plot 1)

(* * Plantes *)

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

let buisson_boule1 = static (from Images_coords.buisson_boule1 0)
let buisson_boule2 = static (from Images_coords.buisson_boule2 0)

let buisson_taille = static (from Images_coords.buisson_taille 0)

let arbuste_taille = static (from Images_coords.arbuste_taille 0)

let fleur1 =
  let mk = from Images_coords.fleur1 in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fleur2 =
  let mk = from Images_coords.fleur2 in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let arbre1 =
  let mk = from Images_coords.arbre1 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 5)

let arbre1_sombre =
  let mk = from ~bundle:vert_fonce Images_coords.arbre1 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 5)

let arbre1_tres_sombre =
  let mk = from ~bundle:vert_tres_fonce Images_coords.arbre1 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 5)

let arbre2 =
  let mk = from Images_coords.arbre2 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 3)

let arbre2_sombre =
  let mk = from ~bundle:vert_fonce Images_coords.arbre2 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 3)

let arbre2_tres_sombre =
  let mk = from ~bundle:vert_tres_fonce Images_coords.arbre2 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 3)

let arbre3 =
  let mk = from Images_coords.arbre3 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 4)

let arbre3_sombre =
  let mk = from ~bundle:vert_fonce Images_coords.arbre3 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 4)

let arbre3_tres_sombre =
  let mk = from ~bundle:vert_tres_fonce Images_coords.arbre3 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 4)

let arbre_loin1 = static (from Images_coords.arbre_loin 0)
let arbre_loin2 = static (from Images_coords.arbre_loin 1)
let arbre_loin3 = static (from Images_coords.arbre_loin 2)
let arbre_loin4 = static (from Images_coords.arbre_loin 3)
let arbre_loin5 = static (from Images_coords.arbre_loin 4)
let arbre_loin6 = static (from Images_coords.arbre_loin 5)
let arbre_loin7 = static (from Images_coords.arbre_loin 6)
let arbre_loin8 = static (from Images_coords.arbre_loin 7)
let arbre_loin9 = static (from Images_coords.arbre_loin 8)
let arbre_loin10 = static (from Images_coords.arbre_loin 9)

let arbre_tombe = static (from Images_coords.arbre_tombe 0)

let branche = static (from Images_coords.branche 0)

let champignon1 = static (from Images_coords.champignons 0)
let champignon2 = static (from Images_coords.champignons 1)
let champignon3 = static (from Images_coords.champignons 2)
let champignon4 = static (from Images_coords.champignons 3)

(* * Minéraux *)

let cailloux = static (from Images_coords.cailloux 0)

(* * Mobilier rural *)

let colline = static (from Images_coords.colline 0)
let dome_herbe = static (from Images_coords.dome_herbe 0)

let chemin1 = static (from Images_coords.chemin1 0)
let chemin2 = static (from Images_coords.chemin2 0)
let chemin3 = static (from Images_coords.chemin3 0)
let chemin4 = static (from Images_coords.chemin4 0)
let chemin5 = static (from Images_coords.chemin5 0)

let chemin_herbe1 = static (from Images_coords.chemin_herbe1 0)
let chemin_herbe2 = static (from Images_coords.chemin_herbe2 0)

let barriere_casse = static (from Images_coords.barriere_casse 0)

let cabane_outils = static (from Images_coords.cabane_outils 0)

(* * Mobilier urbain *)

let banc = static (from Images_coords.banc 0)

let barriere = static (from Images_coords.barriere 0)

let barriere_travaux =
  let mk = from Images_coords.barriere_travaux in
  rwind (mk 0) (mk_sequence 0.25 mk 1 4)

let arceau_velo = static (from Images_coords.arceau 0)

let egouts_ouverts = static (from Images_coords.egouts_ouverts 0)
let plaque_egouts = static (from Images_coords.plaque_egouts 0)

let affiche1 = static (from Images_coords.affiche1 0)
let affiche2 = static (from Images_coords.affiche2 0)

let boite_aux_lettres = static (from Images_coords.boite_aux_lettres 0)

let borne_incendie = static (from Images_coords.borne_incendie 0)

let bac_a_sable = static (from Images_coords.bac_a_sable 0)

let balancoire =
  let mk = from Images_coords.balancoire in
  rwind (mk 0) (mk_sequence 0.3 mk 1 4)

let balancoire_ressort = static (from Images_coords.balancoire_ressort 0)

(* * Panneaux *)

let cedez_le_passage = static (from Images_coords.cedez_le_passage 0)

let panneau_ville = static (from Images_coords.panneau_ville 0)

let potion_interdite = static (from Images_coords.panneaux 0)
let attention = static (from Images_coords.panneaux 1)
let attention_pietons = static (from Images_coords.panneaux 2)

(* * Toits *)

let antenne =
  let mk = from Images_coords.antenne in
  rwind (mk 0) (mk_sequence 0.2 mk 1 2)

let chemine = static (from Images_coords.chemine 0)
let chemines = static (from Images_coords.chemines 0)

let climatiseur = loop (to_sequence 0.1 (fromlist Images_coords.climatiseur))

(* * Fenêtres *)

let fenetre =
  let mk = from Images_coords.fenetre in
  react (static (mk 0)) Event.[RandomFlicker] (to_sequence 0.05 [mk 1])

(* * Portes *)

let arche = static (from Images_coords.arche 0)

(* * Murs *)

let briques1 = static (from Images_coords.briques1 0)
let briques2 = static (from Images_coords.briques2 0)

let briques_cotes = static (from Images_coords.briques_cotes 0)
let briques_ligne = static (from Images_coords.briques_ligne 0)

(* * Autres *)

let bouton_fleche_droite = static (from Images_coords.boutons 0)
let bouton_ajout = static (from Images_coords.boutons 1)
let bouton_fermer = static (from Images_coords.boutons 2)
let bouton_autre = static (from Images_coords.boutons 3)

