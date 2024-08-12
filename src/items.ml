
(* Definitions of the objects and their associated animations for the game. *)

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

(* Build triangles from textures (useful for roofs). *)
let triangle_left img =
  let (dimx, _dimy) = Animation.image_dimensions img in
  static (Filter.triangle_lower_left img dimx)
let triangle_right img =
  let (dimx, _dimy) = Animation.image_dimensions img in
  static (Filter.triangle_lower_right img dimx)

(* An object that only reacts to wind/explosions. *)
let rwind img s =
  react (static img) Event.[Wind; Explode] s

(* An object that reacts to explosions. *)
let rexplosions img s final =
  react (static img) Event.[Explode] (s @ [(final, infinity)]) (* FIXME: This is kind of ugly. *)

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

  let sym =
    let len = List.length Images_coords.perso in
    fun i -> len - 1 - i

  let mk i =
    let mk = from Images_coords.perso in
    (mk i, mk (sym i))

  let range a b = List.split (List.map mk (range a b))

  let (normal_droit, normal_gauche) = mk 0
  let (inspiration_droit, inspiration_gauche) = mk 1
  let (dos_droit, dos_gauche) = mk 2
  let (vent_droit, vent_gauche) = mk 3
  let (regard_haut_droit, regard_haut_gauche) = mk 4
  let (regard_bas_droit, regard_bas_gauche) = mk 5
  let (clignement_droit, clignement_gauche) = mk 6
  let (gratte_droit, gratte_gauche) = range 7 8
  let (avance_droit, avance_gauche) = range 9 14
  let (protection_droit, protection_gauche) = mk 15
  let (chute_droit, chute_gauche) = range 16 17
  let (atterissage_droit, atterissage_gauche) = mk 18

  let perso =
    let perso = static normal_droit in
    let perso = react perso Event.[Wind] [(vent_droit, 0.3)] in
    let perso = react perso Event.[RandomFrequent] [(inspiration_droit, 1.)] in
    let perso = react perso Event.[RandomNormal] [(clignement_droit, 0.1)] in
    let perso = react perso Event.[LookDown] [(regard_bas_droit, 1.)] in
    let perso = react perso Event.[LookUp] [(regard_haut_droit, 1.)] in
    let perso = react perso Event.[LookBehind] [(dos_droit, 1.)] in
    let perso = react perso Event.[Explode] [(protection_droit, 0.8)] in
    let perso = react ~skip:true perso Event.[MoveLeft] (to_sequence 0.05 avance_gauche @ [(normal_gauche, infinity)]) in
    let perso = react ~skip:true perso Event.[MoveRight] (to_sequence 0.05 avance_droit) in
    let perso = react ~restart:true perso Event.[Fall] (to_sequence 0.1 chute_droit @ [(atterissage_droit, 0.5)]) in
    perso (* TODO: This can be improved. *)

end

let femme_fenetre =
  let mk = from Images_coords.femme_fenetre in
  loop (to_sequence 0.5 (List.map mk [0; 2; 0; 1]))

let personne_echelle =
  let mk = from Images_coords.personne_echelle in
  loop (to_sequence 0.5 (List.map mk [0; 2; 0; 1]))

let mineur =
  let mk = from Images_coords.mineur in
  let mineur = static (mk 0) in
  let mineur = react mineur Event.[RandomFrequent] [(mk 1, 1.)] in
  let mineur =
    let s = mk_sequence 0.25 mk 2 6 in
    react mineur Event.[RandomNormal] (s @ List.rev s) in
  mineur

let fumeur =
  let mk = from Images_coords.fumeur in
  loop (to_sequence 0.3 (List.map mk [0; 1; 0; 2]))

let crash = loop (to_sequence 0.7 (fromlist Images_coords.crash))

let emmele_cable =
  let mk = from Images_coords.emmele_cable in
  loop (to_sequence 0.4 (List.map mk [0; 1; 2; 1]))

let enfant =
  let mk = from Images_coords.enfant in
  (* TODO: Faire une pause de durée aléatoire sur la position initiale.
      Faire quelques aller/retour entre les deux premières positions. *)
  loop ((mk 0, 1.) :: to_sequence 0.2 (fromlist Images_coords.enfant))

let enfant_cache1 =
  let mk = from Images_coords.enfant_cache1 in
  (* TODO: La pause initiale doit être aléatoire. *)
  loop ((mk 0, 1.5) :: to_sequence 0.4 (List.map mk [1; 2; 1]))

let enfant_cache2 =
  let mk = from Images_coords.enfant_cache2 in
  (* TODO: Plus de diversité de mouvement ici. *)
  loop ((mk 0, 1.5) :: to_sequence 0.4 (List.map mk [1; 2; 1]))

let enfant_cache3 =
  let mk = from Images_coords.enfant_cache3 in
  (* TODO: Pause aléatoire. *)
  loop ((mk 0, 1.5) :: to_sequence 0.4 (List.map mk [1; 2; 1]))

let enfant_cache_cache_compte =
  let mk = from Images_coords.enfant_cache_cache_compte in
  (* TODO: Plus de diversité de mouvement ici. *)
  loop (to_sequence 1. (List.map mk [0; 1; 2; 0; 1; 2]) @ to_sequence 0.2 (List.map mk [3; 4]))

let enfant_cerf_volant =
  let mk = from Images_coords.enfant_cerf_volant in
  (* TODO: Plus de diversité de mouvement ici. *)
  loop (to_sequence 0.3 (List.map mk [0; 1; 2]))

(* TODO: aveugle *)
(* TODO: chauffard *)
(* TODO: chercheuse *)
(* TODO: femme_discute *)
(* TODO: mere_choquee *)
(* TODO: monsieur_journal *)
(* TODO: ouvrier *)
(* TODO: pecheur *)
(* TODO: personne_livre *)

(* * Animaux *)

(* TODO: chevre *)
(* TODO: coq *)
(* TODO: poule *)
(* TODO: vache_face *)

(* TODO: merle *)
(* TODO: pie *)
(* TODO: oiseau - à animer et renommer. *)
(* TODO: papillon *)
(* TODO: poissons *)
(* TODO: rouge_gorge *)

let papillons =
  let mk = from Images_coords.papillons in
  (* TODO: Il faudrait ajouter des aller/retours dans l'animation. *)
  react (static (mk 0)) Event.[Touch; Explode] (mk_sequence 0.1 mk 1 15)

(* * Objets *)

let potion = loop (to_sequence 0.1 (fromlist Images_coords.potion))

let petite_potion = loop (to_sequence 0.1 (fromlist Images_coords.petite_potion))

let caisse = static (from Images_coords.caisse 0)

let cane = static (from Images_coords.cane 0)

let peluche1 = static (from Images_coords.peluches 0)
let peluche2 = static (from Images_coords.peluches 1)
let peluche3 = static (from Images_coords.peluches 2)
let peluche4 = static (from Images_coords.peluches 3)
let peluche5 = static (from Images_coords.peluches 4)
let peluche6 = static (from Images_coords.peluches 5)
let peluche7 = static (from Images_coords.peluches 6)
let peluche8 = static (from Images_coords.peluches 7)

let cerf_volant =
  loop (to_sequence 0.1 (fromlist Images_coords.cerf_volant)) (* TODO: Add randomness *)

let chariot = static (from Images_coords.chariot 0)

let pioche = static (from Images_coords.pioche 0)

let echelle1 = static (from Images_coords.echelle 0)
let echelle2 = static (from Images_coords.echelle 1)
let echelle3 = static (from Images_coords.echelle 2)

let plot1 = static (from Images_coords.plot 0)
let plot2 = static (from Images_coords.plot 1)

let pot = static (from Images_coords.pot 0)

let etageres1 = static (from Images_coords.etageres 0)
let etageres2 = static (from Images_coords.etageres 1)

let fume =
  loop (to_sequence 0.2 (fromlist Images_coords.fume))

let lampe_interne = static (from Images_coords.lampe_interne 0)

let linge_chute =
  to_sequence 0.1 (fromlist Images_coords.linge_chute)
  @ [(from Images_coords.linge_chute 7, infinity)]
let linge1 =
  let mk = from Images_coords.linge1 in
  let linge = rwind (mk 0) (mk_sequence 0.2 mk 1 2) in
  Animation.force_same_size (react linge Event.[Fall] linge_chute)
let linge2 = static (from Images_coords.linge2 0)
let linge3 =
  let mk = from Images_coords.linge3 in
  let linge = rwind (mk 0) (mk_sequence 0.2 mk 1 3) in
  Animation.force_same_size (react linge Event.[Fall] linge_chute)
let linge4 =
  let mk = from Images_coords.linge4 in
  let linge = rwind (mk 0) (mk_sequence 0.2 mk 1 2) in
  Animation.force_same_size (react linge Event.[Fall] linge_chute)
let linge5 =
  let mk = from Images_coords.linge5 in
  let linge = rwind (mk 0) (mk_sequence 0.2 mk 1 2) in
  Animation.force_same_size (react linge Event.[Fall] linge_chute)

let planche =
  let mk = from Images_coords.planche in
  react (static (mk 0)) Event.[Touch] [(mk 1, 0.5)]

let tronconeuse = static (from Images_coords.tronconeuse 0)

let poussette = static (from Images_coords.poussette 0)

let telescope = static (from Images_coords.telescope 0)

let velo1 = static (from Images_coords.velo 0)
let velo2 = static (from Images_coords.velo 1)

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

let mousse1 = static (from Images_coords.mousse 0)
let mousse2 = static (Filter.flip_horizontally (from Images_coords.mousse 0))
let mousse3 = static (Filter.flip_vertically (from Images_coords.mousse 0))

let herbe =
  let mk = from Images_coords.herbe in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fleur1 =
  let mk = from Images_coords.fleur1 in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fleur2 =
  let mk = from Images_coords.fleur2 in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fleur3 =
  let mk = from Images_coords.fleur3 in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fracture =
  let mk = from Images_coords.fracture in
  rwind (mk 0) (to_sequence 0.15 [mk 1])

let fleurs = static (from Images_coords.fleurs 0)

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

let arbre4 =
  let mk = from Images_coords.arbre4 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 6)
let arbre4_sombre =
  let mk = from ~bundle:vert_fonce Images_coords.arbre4 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 6)
let arbre4_tres_sombre =
  let mk = from ~bundle:vert_tres_fonce Images_coords.arbre4 in
  rwind (mk 0) (mk_sequence 0.2 mk 1 6)

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

let souche = static (from Images_coords.souche 0)

let sapins = static (from Images_coords.sapins 0)

let champignon1 = static (from Images_coords.champignons 0)
let champignon2 = static (from Images_coords.champignons 1)
let champignon3 = static (from Images_coords.champignons 2)
let champignon4 = static (from Images_coords.champignons 3)

let epi_ble =
  let mk = from Images_coords.epi_ble in
  let epi = static (mk 0) in
  let epi = react epi Event.[Wind; Touch] (mk_sequence 0.2 mk 1 3) in
  let epi = react epi Event.[Explode] (mk_sequence 0.2 mk 1 2 @ mk_sequence 0.2 mk 4 6 @ [(mk 6, infinity)]) in
  (* TODO: This infinity is an ugly fix. *)
  epi

let rosiers =
  let mk = from Images_coords.rosiers in
  rwind (mk 0) (to_sequence 0.2 (List.map mk [1; 2; 3; 2; 1]))

(* * Minéraux *)

let cailloux = static (from Images_coords.cailloux 0)

let graviers1 = static (from Images_coords.graviers 0)
let graviers2 = static (Filter.flip_vertically (from Images_coords.graviers 0))
let graviers3 = static (Filter.flip_horizontally (from Images_coords.graviers 0))

let impurte1 = static (from Images_coords.impurte 0)
let impurte2 = static (Filter.flip_horizontally (from Images_coords.impurte 0))
let impurte3 = static (Filter.flip_vertically (from Images_coords.impurte 0))

let pierre1 = static (from Images_coords.pierres 0)
let pierre2 = static (from Images_coords.pierres 1)
let pierre3 = static (from Images_coords.pierres 2)
let pierre4 = static (from Images_coords.pierres 3)
let pierre5 = static (from Images_coords.pierres 4)
let pierre6 = static (from Images_coords.pierres 5)
let pierre7 = static (from Images_coords.pierres 6)
let pierre8 = static (from Images_coords.pierres 7)
let pierre9 = static (from Images_coords.pierres 8)

let pierre_bouge1 =
  let mk = from Images_coords.pierres_bougent in
  react (static (mk 0)) Event.[Touch; Explode] (mk_sequence 0.1 mk 1 3)
let pierre_bouge2 =
  let mk = from Images_coords.pierres_bougent in
  react (static (mk 4)) Event.[Touch; Explode] (mk_sequence 0.1 mk 5 6)

let roche = static (from Images_coords.roche 0)

let rocher = static (from Images_coords.rocher 0)

let rebord = static (from Images_coords.rebord 0)
let rebord_herbe = static (from Images_coords.rebord_herbe 0)

(* * Mobilier rural *)

let colline = static (from Images_coords.colline 0)
let dome_herbe = static (from Images_coords.dome_herbe 0)
let montagne1 = static (from Images_coords.montagne1 0)
let montagne2 = static (from Images_coords.montagne2 0)
let montagne_village = static (from Images_coords.montagne_village 0)

let paroi = static (from Images_coords.paroi 0)

let chemin1 = static (from Images_coords.chemin1 0)
let chemin2 = static (from Images_coords.chemin2 0)
let chemin3 = static (from Images_coords.chemin3 0)
let chemin4 = static (from Images_coords.chemin4 0)
let chemin5 = static (from Images_coords.chemin5 0)

let chemin_herbe1 = static (from Images_coords.chemin_herbe1 0)
let chemin_herbe2 = static (from Images_coords.chemin_herbe2 0)

let tas1 = static (from Images_coords.tas1 0)
let tas2 = static (from Images_coords.tas2 0)

let pont = static (from Images_coords.pont 0)
let pont_debut = static (from Images_coords.pont_debut 0)
let pont_fin = static (from Images_coords.pont_fin 0)
let pont_tablier = static (from Images_coords.pont_tablier 0)
let poutre_pont = static (from Images_coords.poutre_pont 0)
let poutre_pont_horizontale = static (from Images_coords.poutre_pont_horizontale 0)
let support_pont = static (from Images_coords.support_pont 0)

let piquet = static (from Images_coords.piquet 0)

let barriere_casse = static (from Images_coords.barriere_casse 0)

let maison = static (from Images_coords.maison 0)

let cabane_outils = static (from Images_coords.cabane_outils 0)

let escaliers = static (from Images_coords.escaliers 0)

let fond_foret1 = static (from Images_coords.fond_foret 0)
let fond_foret2 = static (Filter.flip_horizontally (from Images_coords.fond_foret 0))

let lac =
  let lac = from Images_coords.lac 0 in
  let lac = Filter.shimmer ~quantity:150 ~amplitude:10 ~duration:35 ~direction:(-0.2, 1.) lac in
  loop (List.map (fun img -> (img, 0.15)) lac)

let mare =
  let mare = from Images_coords.mare 0 in
  let mare = Filter.shimmer ~quantity:40 ~amplitude:3 ~duration:20 ~direction:(-0.2, 1.) mare in
  loop (List.map (fun img -> (img, 0.3)) mare)

let ruisseau =
  (* TODO: Ajouter de l'aléatoire de temps en temps. *)
  loop (to_sequence 0.1 (fromlist Images_coords.ruisseau))

let puit = static (from Images_coords.puit 0)
let rail_stop = static (from Images_coords.rail_stop 0)
let rampe = static (from Images_coords.rampe 0)

let grotte_fond1 = static (from Images_coords.grotte_fond 0)
let grotte_fond2 = static (Filter.flip_horizontally (from Images_coords.grotte_fond 0))
let stalactique = static (from Images_coords.stalactique 0)
let stalagmite = static (from Images_coords.stalagmite 0)

let traces1 = static (from Images_coords.traces 0)
let traces2 = static (from Images_coords.traces 1)
let traces3 = static (from Images_coords.traces 2)

(* * Mobilier urbain *)

let banc = static (from Images_coords.banc 0)

let poubelle = static (from Images_coords.poubelle 0)

let barriere = static (from Images_coords.barriere 0)

let poteau = static (from Images_coords.poteau 0)

let barriere_travaux =
  let mk = from Images_coords.barriere_travaux in
  rwind (mk 0) (mk_sequence 0.25 mk 1 4)

let arceau_velo = static (from Images_coords.arceau 0)

let egouts_ouverts = static (from Images_coords.egouts_ouverts 0)
let plaque_egouts = static (from Images_coords.plaque_egouts 0)

let grille_ventilation = static (from Images_coords.grille_ventilation 0)

let affiche1 = static (from Images_coords.affiche1 0)
let affiche2 = static (from Images_coords.affiche2 0)

let plaque_rue = static (from Images_coords.plaque_rue 0)

let boite_aux_lettres = static (from Images_coords.boite_aux_lettres 0)

let lampe_droit = static (from Images_coords.lampe 0)
let lampe_gauche = static (Filter.flip_horizontally (from Images_coords.lampe 0))

let borne_incendie = static (from Images_coords.borne_incendie 0)

let horloge = static (from Images_coords.horloge 0)

let espace_arbre = static (from Images_coords.espace_arbre 0)

let paves = static (from Images_coords.paves 0)

let bac_a_sable = static (from Images_coords.bac_a_sable 0)

let balancoire =
  let mk = from Images_coords.balancoire in
  rwind (mk 0) (mk_sequence 0.3 mk 1 4)

let balancoire_ressort = static (from Images_coords.balancoire_ressort 0)

let publicite_dos = static (from Images_coords.publicite_dos 0)

let statue = static (from Images_coords.statue 0)

let tag_visage = static (from Images_coords.tag_divers 0)
let tag_coeur = static (from Images_coords.tag_divers 1)
let tag_nE = static (from Images_coords.tag_divers 2)
let tag_message = static (from Images_coords.tag_simple 0)
let tag_message_gros = static (from Images_coords.tag_gros 0)
let tag_A = static (from Images_coords.tag_lettres 0)
let tag_Anar = static (from Images_coords.tag_lettres 1)
let tag_C = static (from Images_coords.tag_lettres 2)
let tag_B = static (from Images_coords.tag_lettres 3)

let tuyau_horizontal = static (from Images_coords.tuyau_horizontal 0)
let tuyau_vertical = static (from Images_coords.tuyau_vertical 0)
let tuyau_courbe_bas_gauche = static (from Images_coords.tuyau_courbe1 0)
let tuyau_courbe_haut_gauche = static (from Images_coords.tuyau_courbe4 0)
let tuyau_courbe_bas_droite = static (from Images_coords.tuyau_courbe3 0)
let tuyau_courbe_haut_droite = static (from Images_coords.tuyau_courbe2 0)
let tuyau_troue = static (from Images_coords.tuyau_troue 0)

(* * Panneaux *)

let cedez_le_passage = static (from Images_coords.cedez_le_passage 0)

let panneau_ville = static (from Images_coords.panneau_ville 0)

  let support_panneau =
    let mk = from Images_coords.support_panneau in
    react (static (mk 0)) Event.[Explode] (to_sequence infinity [mk 1])

(* Monte un panneau sur un support. *)
let monter_panneau =
  let support_panneau =
    let mk = from Images_coords.support_panneau in
    react (static (mk 0)) Event.[Explode] (to_sequence infinity [mk 1]) in
  assert (Animation.check_size support_panneau) ;
  let (dimx_support, _dimy_support) = Animation.image_dimensions (Animation.image support_panneau) in
  (* Le panneau n'est pas centré sur l'image, d'où cette différence de 1. *)
  let adhoc_offset = -1 in
  fun img ->
    let (dimx, dimy) = Animation.image_dimensions img in
    Animation.combine [
      (static img, (0, 0)) ;
      (support_panneau, ((dimx - dimx_support) / 2 + adhoc_offset, dimy))
    ]

let potion_interdite = monter_panneau (from Images_coords.panneaux 0)
let attention = monter_panneau (from Images_coords.panneaux 1)
let attention_pietons = monter_panneau (from Images_coords.panneaux 2)

let priorite_pieton = monter_panneau (from Images_coords.priorite_pieton 0)

(* * Toits *)

let toit = static (from Images_coords.toit 0)
let toit_plat = static (from Images_coords.toit_plat 0)
let toit_faite = static (from Images_coords.toit_faite 0)
let toit_angle_bas_gauche = static (from Images_coords.toit_angle_bas_gauche 0)
let toit_angle_gauche = static (from Images_coords.toit_angle_gauche 0)
let toit_goutiere = static (from Images_coords.toit_goutiere 0)

let toit_plat_gauche = static (from Images_coords.toit_plat_gauche 0)
let toit_plat_droit = static (from Images_coords.toit_plat_droit 0)

let toit_plat_muret1 = static (from Images_coords.toit_plat_muret 0)
let toit_plat_muret2 = static (from Images_coords.toit_plat_muret 1)
let toit_plat_muret3 = static (from Images_coords.toit_plat_muret 2)

let toit_shed_creux = static (from Images_coords.toit_shed_creux 0)
let toit_shed_debut = static (from Images_coords.toit_shed_debut 0)
let toit_shed_vitre = static (from Images_coords.toit_shed_vitre 0)
let toit_shed_fin = static (from Images_coords.toit_shed_fin 0)
let toit_shed_pente = static (from Images_coords.toit_shed_pente 0)
let toit_shed_pointe = static (from Images_coords.toit_shed_pointe 0)

let toit_tuile = static (from Images_coords.toit_tuile 0)
let toit_tuile_plat = static (from Images_coords.toit_tuile_plat 0)
let toit_tuile_faite = static (from Images_coords.toit_tuile_faite 0)
let toit_tuile_angle_bas_droit = static (from Images_coords.toit_tuile_angle_bas_droit 0)
let toit_tuile_angle_bas_gauche = static (from Images_coords.toit_tuile_angle_bas_gauche 0)
let toit_tuile_angle_droit = static (from Images_coords.toit_tuile_angle_droit 0)
let toit_tuile_angle_gauche = static (from Images_coords.toit_tuile_angle_gauche 0)
let toit_tuile_fenetre_couverture = static (from Images_coords.toit_tuile_fenetre_couverture 0)
let toit_tuile_pignon = static (from Images_coords.toit_tuile_pignon 0)

let antenne =
  let mk = from Images_coords.antenne in
  rwind (mk 0) (mk_sequence 0.2 mk 1 2)

let chemine = static (from Images_coords.chemine 0)
let chemines = static (from Images_coords.chemines 0)
let petite_cheminee = static (from Images_coords.petite_cheminee 0)
let sortie_air = static (from Images_coords.sortie_air 0)

let fenetre_toit_gauche =
  let mk = from Images_coords.fenetre_toit in
  rexplosions (mk 0) [] (mk 1)

let fenetre_toit_droit =
  let mk i = Filter.flip_horizontally (from Images_coords.fenetre_toit i) in
  rexplosions (mk 0) [] (mk 1)

let climatiseur = loop (to_sequence 0.1 (fromlist Images_coords.climatiseur))

(* * Fenêtres *)

let fenetre =
  let mk = from Images_coords.fenetre in
  react (static (mk 0)) Event.[RandomFlicker] (to_sequence 0.05 [mk 1])

let fenetre_cote_gauche =
  let mk = from Images_coords.fenetre_cote in
  rexplosions (mk 0) [] (mk 1)

let fenetre_cote_droit =
  let mk i = Filter.flip_horizontally (from Images_coords.fenetre_cote i) in
  rexplosions (mk 0) [] (mk 1)

let fenetre_face1 = static (from Images_coords.fenetre_face 0)
let fenetre_face2 = static (from Images_coords.fenetre_face 1)

let fenetre_haute = static (from Images_coords.fenetre_haute 0)

let fenetre_large = static (from Images_coords.fenetre_large 0)

let fenetre_ouverte = static (from Images_coords.fenetre_ouverte 0)

let fenetre_ronde = static (from Images_coords.fenetre_ronde 0)

(* * Portes *)

let arche = static (from Images_coords.arche 0)

let porte_immeuble = static (from Images_coords.porte_immeuble 0)
let porte_variante = static (from Images_coords.porte_variante 0)
let porte_commerce = static (from Images_coords.porte_commerce 0)
let porte_vitre_commerce = static (from Images_coords.porte_vitre_commerce 0)
let porte_services = static (from Images_coords.porte_services 0)

let store_terrasse =
  let mk = from Images_coords.store_terrasse in
  rwind (mk 0) (mk_sequence 0.2 mk 1 4)

(* * Murs *)

let briques1 = static (from Images_coords.briques1 0)
let briques2 = static (from Images_coords.briques2 0)

let briques_cotes = static (from Images_coords.briques_cotes 0)
let briques_ligne = static (from Images_coords.briques_ligne 0)

let mur_abime = static (from Images_coords.mur_abime 0)

let texture_fond1 = static (from Images_coords.texture_fond1 0)
let texture_fond2 = static (from Images_coords.texture_fond2 0)
let texture_mur1 = static (from Images_coords.texture_mur1 0)
let texture_mur1_toit_gauche = triangle_left (from Images_coords.texture_mur1 0)
let texture_mur1_toit_droit = triangle_right (from Images_coords.texture_mur1 0)
let texture_mur2 = static (from Images_coords.texture_mur2 0)
let texture_mur2_toit_gauche = triangle_left (from Images_coords.texture_mur2 0)
let texture_mur2_toit_droit = triangle_right (from Images_coords.texture_mur2 0)
let texture_mur3 = static (from Images_coords.texture_mur3 0)
let texture_mur3_toit_gauche = triangle_left (from Images_coords.texture_mur3 0)
let texture_mur3_toit_droit = triangle_right (from Images_coords.texture_mur3 0)

(* * Ciel *)

let etoile =
  let mk = from Images_coords.etoile in
  react (static (mk 0)) Event.[RandomFlicker] (to_sequence 0.05 (List.map mk [2; 1; 2]))

let nuage1 = static (from Images_coords.nuage 0)
let nuage2 = static (from Images_coords.nuage 1)

let soleil =
  let mk = from Images_coords.soleil in
  react (static (mk 0)) Event.[RandomRare] (mk_sequence 0.05 mk 1 3)

(* * Interface *)

(* TODO: Générer ces buttons à partir des boutons de l'interface, avec une animation qui appuie sur la touche. *)
let touche_droite = static (from Images_coords.touche_droite 0)

(* Interface buttons are expressed as Animation.image and not Animation.t for convenience. *)

let bouton_fleche_droite = from Images_coords.boutons 0
let bouton_fleche_gauche = Filter.flip_horizontally bouton_fleche_droite
let bouton_ajout = from Images_coords.boutons 1
let bouton_supprimer = from Images_coords.boutons 2
let bouton_enregistrer = from Images_coords.boutons 3
let bouton_deplacer = from Images_coords.boutons 4

