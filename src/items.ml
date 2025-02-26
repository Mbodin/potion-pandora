
(* Definitions of the objects and their associated animations for the game. *)

open Items_aux

(* The palette is an exception, as it is provided as a single image (with a single line).
  We here split it into several one-pixel images. *)
let palette =
  match Images_coords.palette with
  | [((width, height), (x, y))] ->
    assert (height = 1) ;
    List.init width (fun dx ->
      ((1, height), (x + dx, y)))
  | _ -> assert false

(* Patterns *)
let vert_fonce = from palette 3
let vert_tres_fonce = from palette 2

(* * Personnages *)

module Perso = struct

  let sym =
    let len = List.length Images_coords.perso in
    fun i -> len - 1 - i

  let mk i =
    let mk = from Images_coords.perso in
    (mk i, mk (sym i))

  let range a b = List.split (List.map mk (range a b))

  let normal = mk 0
  let inspiration = mk 1
  let dos = mk 2
  let vent = mk 3
  let regard_haut = mk 4
  let regard_bas = mk 5
  let clignement = mk 6
  let gratte = range 7 8
  let avance = range 9 14
  let tourne_debut = mk 10
  let tourne_fin = mk 11
  let protection = mk 15
  let chute = range 16 17
  let atterissage = mk 18

  let perso =
    let open Animation in
    let make b =
      let f = if b then snd else fst in
      let perso = static (f normal) in
      let perso = react perso Event.[Wind] [(f vent, 0.3)] in
      let perso = react perso Event.[RandomFrequent] [(f inspiration, 0.5)] in
      let perso = react perso Event.[RandomNormal] [(f clignement, 0.05)] in
      let perso = react perso Event.[RandomRare] (to_sequence 0.5 (f gratte)) in
      let perso = react perso Event.[LookDown] [(f regard_bas, 1.)] in
      let perso = react perso Event.[LookUp] [(f regard_haut, 1.)] in
      let perso = react perso Event.[LookBehind] [(f dos, 1.)] in
      let perso = react perso Event.[Explode] [(f protection, 0.8)] in
      let perso =
        react ~restart:true perso Event.[Fall]
          (to_sequence 0.1 (f chute) @ [(f atterissage, 0.5)]) in
      perso in
    let perso_droite =
      let perso = make false in
      let perso = react perso Event.[MoveRight] ~skip:true (to_sequence 0.05 (fst avance)) in
      perso in
    let perso_gauche =
      let perso = make true in
      let perso = react perso Event.[MoveLeft] ~skip:true (to_sequence 0.05 (snd avance)) in
      perso in
    let vers_droite = to_sequence 0.1 [fst tourne_debut; snd tourne_fin] in
    let vers_gauche = to_sequence 0.1 [snd tourne_debut; fst tourne_fin] in
    let perso =
      Animation.switch
        perso_droite Event.[MoveLeft] ~skip:true vers_gauche
        perso_gauche Event.[MoveRight] ~skip:true vers_droite in
    perso

end

let perso = Perso.perso

let femme_fenetre =
  let mk = from Images_coords.femme_fenetre in
  loop (mk_sequence 0.5 mk [0; 2; 0; 1])

let personne_echelle =
  let mk = from Images_coords.personne_echelle in
  loop (mk_sequence 0.5 mk [0; 2; 0; 1])

let mineur =
  let open Animation in
  let mk = from Images_coords.mineur in
  let mineur = static (mk 0) in
  let mineur = react mineur Event.[RandomFrequent] [(mk 1, 1.)] in
  let mineur =
    let s = mk_sequence_range 0.18 mk 2 6 in
    react mineur Event.[RandomNormal] (s @ List.rev s) in
  mineur

let fumeur =
  let mk = from Images_coords.fumeur in
  loop (mk_sequence 0.3 mk [0; 1; 0; 2])

let crash = loop (to_sequence 0.7 (fromlist Images_coords.crash))

let emmele_cable =
  let mk = from Images_coords.emmele_cable in
  loop (mk_sequence 0.4 mk [0; 1; 2; 1])

let enfant =
  let mk = from Images_coords.enfant in
  (* L'état indique soit [None] quand la séquence commence, ou [Some n] quand on va attendre
    [n] aller/retours sur deux positions. *)
  Animation.nd_transitions None (function
    | None ->
      (to_sequence 0.2 (fromlist Images_coords.enfant), only_tau None [
        (2, [], None) ;
        (2, [], Some 0) ;
        (1, [], Some 3) ;
        (1, [], Some 2)
      ])
    | Some n ->
      let next =
        if n = 0 then only_tau (Some n) [(1, [], None)]
        else only_tau (Some n) [(1, [], Some (n - 1))] in
      ([(mk 0, 0.5); (mk 8, 0.2)], next))

let enfant_cache1 =
  let mk = from Images_coords.enfant_cache1 in
  Animation.nd_transitions () (fun () ->
    ([(mk 0, 1.)], only_tau () [
        (2, [], ()) ;
        (1, [(mk 2, 0.6)], ())
      ]))

let enfant_cache2 =
  let mk = from Images_coords.enfant_cache2 in
  Animation.nd_transitions () (fun () ->
    ([(mk 0, 1.1)], only_tau () [
        (2, [], ()) ;
        (1, [(mk 2, 0.5)], ())
      ]))

let enfant_cache3 =
  let mk = from Images_coords.enfant_cache3 in
  Animation.nd_transitions () (fun () ->
    ([(mk 0, 0.9)], only_tau () [
        (2, [], ()) ;
        (1, [(mk 2, 0.4)], ())
      ]))

let enfant_cache_cache_compte =
  let mk = from Images_coords.enfant_cache_cache_compte in
  Animation.nd_transitions 0 (function
    | 0 ->
      ((mk 1, 1.) :: mk_sequence 0.3 mk [4; 3], only_tau 0 [
        (1, [], 13) ;
        (1, [], 10) ;
        (1, [], 7)
      ])
    | n ->
      (mk_sequence 0.5 mk [0; 1; 2], only_tau n [
        (1, [], (n - 1))
      ]))

let enfant_cerf_volant =
  let sfrom = seq_from (to_sequence 0.3 (fromlist Images_coords.enfant_cerf_volant)) in
  Animation.nd_transitions () (fun () ->
    (sfrom 0, only_tau () [
        (3, [], ()) ;
        (1, sfrom 1, ()) ;
        (1, sfrom 2, ())
      ]))

(* TODO: aveugle *)
(* TODO: chauffard *)
(* TODO: chercheuse *)
(* TODO: femme_discute *)
(* TODO: mere_choquee *)
(* TODO: monsieur_journal *)
(* TODO: ouvrier *)
(* TODO: pecheur *)
(* TODO: personne_livre *)
(* TODO: femme_promene_chien *)
(* TODO: monsieur_discute *)

(* * Animaux *)

(* TODO: chevre *)
(* TODO: coq *)
(* TODO: poule *)
(* TODO: vache_face *)

(* TODO: merle *)
(* TODO: pie *)
(* TODO: papillon *)
(* TODO: poissons *)
(* TODO: rouge_gorge *)

(* Intermediary type to describe flight *)
type ('flying, 'standing) flight =
  | InFlight of 'flying   (* Currently in flight. *)
  | Landing of 'flying    (* Was in flight, possibly still in flight (with this state). *)
  | Standing of 'standing (* Standing, with some state. *)
  | TakingOff             (* Is currently taking off. *)

(* Useful function to deal with [Animation.nd_transitions] in flight: one provides it
  the current state and a list of possible flying and standing state, and this function
  produces the event map. *)
let event_flight st lflight lstanding e =
  assert (lflight <> [] && lstanding <> []) ;
  match st, e with
  (* Events that move the bird, indicating that we are indeed in flight. *)
  | (InFlight _ | Landing _ | TakingOff), Event.(MoveLeft | MoveRight | Fall) ->
    List.map (fun vflight -> (1, [], InFlight vflight)) lflight
  | Standing _, Event.(MoveLeft | MoveRight | Fall) -> [(1, [], TakingOff)]
  (* Time passing. *)
  | (Standing _ | Landing _), Event.Tau ->
    List.map (fun vstanding -> (1, [], Standing vstanding)) lstanding
  | (InFlight _ | TakingOff), Event.Tau ->
    List.map (fun vflight -> (1, [], Landing vflight)) lflight
  (* Other events are ignored. *)
  | _, _ -> [(1, [], st)]

let canard =
  let mk = from Images_coords.canard in
  let t = 0.1 in
  let flight = mk_sequence_range t mk 4 6 in
  let taking_off = mk_sequence t mk [2; 3] in
  (* Le booléen sur [Standing] indique s'il regarde en arrière. *)
  Animation.nd_transitions (InFlight ()) (fun st ->
    match st with
    | TakingOff -> (taking_off, event_flight st [()] [false])
    | InFlight () -> (flight, event_flight st [()] [false])
    | Landing () -> ([(mk 2, t)], event_flight st [()] [false])
    | Standing false -> ([(mk 0, t)], event_flight st [()] [false; false; false; true])
    | Standing true -> ([(mk 1, t)], event_flight st [()] [false; true]))

let () =
  (* TODO FIXME: There is something very wrong here: it has only 3 states. *)
  print_endline (Animation.print canard)

let papillon =
  let mk = from Images_coords.papillon in
  let index_st = function
    | 0 -> [0; 1]
    | 1 -> [2; 3]
    | 2 -> [4; 5]
    | 3 -> [0; 1; 0]
    | 4 -> [2; 3; 2]
    | 5 -> [4; 5; 4]
    | _ -> assert false in
  let t = 0.1 in
  let seq_st m = mk_sequence t mk (index_st m) in
  let full = [0; 1; 2; 3; 4; 5] in
  let taking_off = [(1, 3); (2, 3); (2, 0)] in
  let rec combine l1 = function
    | [] -> []
    | x2 :: l2 -> List.map (fun x1 -> (x1, x2)) l1 @ combine l1 l2 in
  (* Dans les paires [(n, m)], [m] indique l'état (0, 1, ou 2) et [n] sa répétition. *)
  Animation.nd_transitions (InFlight (2, 0)) (fun st ->
    match st with
    | InFlight (0, m) ->
      (seq_st m, event_flight st (combine [1; 2; 3; 5] full) [()])
    | InFlight (n, m) ->
      (seq_st m, event_flight st [(n - 1, m)] [()])
    | Landing (_, (0 | 3 | 5)) | TakingOff ->
      (seq_st 1, event_flight st taking_off [()])
    | Landing (_, _) | Standing () ->
      ([(mk 3, t)], event_flight st taking_off [()]))

let papillons =
  let mk = from Images_coords.papillons in
  (* TODO: Il faudrait ajouter des aller/retours dans l'animation. *)
  Animation.react (static (mk 0)) Event.[Touch; Explode] (mk_sequence_range 0.1 mk 1 15)

(* * Objets *)

let potion = loop (to_sequence 0.1 (fromlist Images_coords.potion))

let petite_potion = loop (to_sequence 0.1 (fromlist Images_coords.petite_potion))

let caisse = static_unique Images_coords.caisse

let cane = static_unique Images_coords.cane

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

let chariot = static_unique Images_coords.chariot

let pioche = static_unique Images_coords.pioche

let echelle1 = static (from Images_coords.echelle 0)
let echelle2 = static (from Images_coords.echelle 1)
let echelle3 = static (from Images_coords.echelle 2)

let plot1 = static (from Images_coords.plot 0)
let plot2 = static (from Images_coords.plot 1)

let pot = static_unique Images_coords.pot

let etageres1 = static (from Images_coords.etageres 0)
let etageres2 = static (from Images_coords.etageres 1)

let fume =
  loop (to_sequence 0.2 (fromlist Images_coords.fume))

let lampe_interne = static_unique Images_coords.lampe_interne

let chute_linge linge =
  let linge_chute =
    to_sequence 0.1 (fromlist Images_coords.linge_chute) in
  Animation.force_same_size
    (Animation.change_with linge Event.[Fall] linge_chute
      (static (from Images_coords.linge_chute 7)))
let linge1 =
  let mk = from Images_coords.linge1 in
  let linge = rwind (mk 0) (mk_sequence_range 0.2 mk 1 2) in
  chute_linge linge
let linge2 = static (from Images_coords.linge2 0)
let linge3 =
  let mk = from Images_coords.linge3 in
  let linge = rwind (mk 0) (mk_sequence_range 0.2 mk 1 3) in
  chute_linge linge
let linge4 =
  let mk = from Images_coords.linge4 in
  let linge = rwind (mk 0) (mk_sequence_range 0.2 mk 1 2) in
  chute_linge linge
let linge5 =
  let mk = from Images_coords.linge5 in
  let linge = rwind (mk 0) (mk_sequence_range 0.2 mk 1 2) in
  chute_linge linge

let planche =
  let mk = from Images_coords.planche in
  Animation.react (static (mk 0)) Event.[Touch] [(mk 1, 0.5)]

let tronconeuse = static_unique Images_coords.tronconeuse

let poussette = static_unique Images_coords.poussette

let telescope = static_unique Images_coords.telescope

let velo1 = static (from Images_coords.velo 0)
let velo2 = static (from Images_coords.velo 1)

(* * Plantes *)

let plante1 =
  let mk = from Images_coords.plante1 in
  rwind (mk 0) (mk_sequence 0.2 mk [1])
let plante1_sombre = Animation.decolor ~pattern:vert_fonce plante1
let plante1_tres_sombre = Animation.decolor ~pattern:vert_tres_fonce plante1

let buisson =
  let mk = from Images_coords.buisson in
  rwind (mk 0) (mk_sequence 0.2 mk [1])

let buisson_boule1 = static_unique Images_coords.buisson_boule1
let buisson_boule2 = static_unique Images_coords.buisson_boule2

let buisson_taille = static_unique Images_coords.buisson_taille

let arbuste_taille = static_unique Images_coords.arbuste_taille

let mousse1 = static_unique Images_coords.mousse
let mousse2 = static (Filter.flip_horizontally (from Images_coords.mousse 0))
let mousse3 = static (Filter.flip_vertically (from Images_coords.mousse 0))

let herbe =
  let mk = from Images_coords.herbe in
  rwind (mk 0) (mk_sequence 0.15 mk [1])

let fleur1 =
  let mk = from Images_coords.fleur1 in
  rwind (mk 0) (mk_sequence 0.15 mk [1])

let fleur2 =
  let mk = from Images_coords.fleur2 in
  rwind (mk 0) (mk_sequence 0.15 mk [1])

let fleur3 =
  let mk = from Images_coords.fleur3 in
  rwind (mk 0) (mk_sequence 0.15 mk [1])

let fracture =
  let mk = from Images_coords.fracture in
  rwind (mk 0) (mk_sequence 0.15 mk [1])

let fleurs = static_unique Images_coords.fleurs

let fleurs_large =
  let mk = from Images_coords.fleurs_large in
  rwind (mk 0) (mk_sequence 0.15 mk [1; 2; 3; 2; 1])

let fleur_ecrasee =
  let mk = from Images_coords.fleur_ecrasee in
  Animation.change_with (static (mk 0)) Event.[Touch; Explode] (mk_sequence_range 0.1 mk 1 2)

let fougere1 =
  let mk = from Images_coords.fougere in
  rwind (mk 0) (mk_sequence 0.15 mk [1; 2; 1])

let fougere2 =
  let mk = from Images_coords.fougere_fanee in
  rwind (mk 0) (mk_sequence_range 0.15 mk 1 3)

let arbre1 =
  let mk = from Images_coords.arbre1 in
  rwind (mk 0) (mk_sequence_range 0.2 mk 1 5)
let arbre1_sombre = Animation.decolor ~pattern:vert_fonce arbre1
let arbre1_tres_sombre = Animation.decolor ~pattern:vert_tres_fonce arbre1

let arbre2 =
  let mk = from Images_coords.arbre2 in
  rwind (mk 0) (mk_sequence_range 0.2 mk 1 3)
let arbre2_sombre = Animation.decolor ~pattern:vert_fonce arbre2
let arbre2_tres_sombre = Animation.decolor ~pattern:vert_tres_fonce arbre2

let arbre3 =
  let mk = from Images_coords.arbre3 in
  rwind (mk 0) (mk_sequence_range 0.2 mk 1 4)
let arbre3_sombre = Animation.decolor ~pattern:vert_fonce arbre3
let arbre3_tres_sombre = Animation.decolor ~pattern:vert_tres_fonce arbre3

let arbre4 =
  let mk = from Images_coords.arbre4 in
  rwind (mk 0) (mk_sequence_range 0.2 mk 1 6)
let arbre4_sombre = Animation.decolor ~pattern:vert_fonce arbre4
let arbre4_tres_sombre = Animation.decolor ~pattern:vert_tres_fonce arbre4

let arbre5 =
  let mk = from Images_coords.arbre5 in
  rwind (mk 0) (mk_sequence_range 0.2 mk 1 6)
let arbre5_sombre = Animation.decolor ~pattern:vert_fonce arbre5
let arbre5_tres_sombre = Animation.decolor ~pattern:vert_tres_fonce arbre5

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

let haie_loin = static (from Images_coords.haie_loin 0)

let arbre_tombe = static_unique Images_coords.arbre_tombe

let branche = static_unique Images_coords.branche
let souche = static_unique Images_coords.souche
let bois_mousse = static_unique Images_coords.bois_mousse

let sapins = static_unique Images_coords.sapins

let champignon1 = static (from Images_coords.champignons 0)
let champignon2 = static (from Images_coords.champignons 1)
let champignon3 = static (from Images_coords.champignons 2)
let champignon4 = static (from Images_coords.champignons 3)

let epi_ble =
  let mk = from Images_coords.epi_ble in
  let epi = static (mk 0) in
  let epi = Animation.react epi Event.[Wind; Touch] (mk_sequence_range 0.2 mk 1 3) in
  let epi =
    Animation.change_with epi Event.[Explode] (mk_sequence_range 0.05 mk 1 2 @ mk_sequence_range 0.05 mk 4 6)
      (static (mk 6)) in
  epi

let rosiers =
  let mk = from Images_coords.rosiers in
  rwind (mk 0) (mk_sequence 0.2 mk [1; 2; 3; 2; 1])

let lierre_pied =
  let mk = from Images_coords.lierre_pied in
  rwind (mk 0) (mk_sequence 0.2 mk [1; 0; 1])

let lierre_vertical =
  let mk = from Images_coords.lierre_vertical in
  rwind (mk 0) (mk_sequence 0.2 mk [1; 0; 1])

let lierre_diagonal =
  let mk = from Images_coords.lierre_diagonal in
  rwind (mk 0) (mk_sequence 0.2 mk [1; 0; 1])

let lierre_horizontal =
  let mk = from Images_coords.lierre_horizontal in
  rwind (mk 0) (mk_sequence 0.2 mk [1; 0; 1])

let tour_de_lierre =
  let mk = from Images_coords.tour_de_lierre in
  rwind (mk 0) (mk_sequence 0.2 mk [1; 0; 1])

let vieux_lierre = static_unique Images_coords.vieux_lierre


(* * Minéraux *)

let cailloux = static_unique Images_coords.cailloux

let graviers1 = static_unique Images_coords.graviers
let graviers2 = static (Filter.flip_vertically (from Images_coords.graviers 0))
let graviers3 = static (Filter.flip_horizontally (from Images_coords.graviers 0))

let impurte1 = static_unique Images_coords.impurte
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
  Animation.react (static (mk 0)) Event.[Touch; Explode] (mk_sequence_range 0.1 mk 1 3)
let pierre_bouge2 =
  let mk = from Images_coords.pierres_bougent in
  Animation.react (static (mk 4)) Event.[Touch; Explode] (mk_sequence_range 0.1 mk 5 6)

let roche = static_unique Images_coords.roche

let rocher = static_unique Images_coords.rocher

let rebord = static_unique Images_coords.rebord
let rebord_herbe = static_unique Images_coords.rebord_herbe

(* * Mobilier rural *)

let colline = static_unique Images_coords.colline
let dome_herbe = static_unique Images_coords.dome_herbe
let montagne1 = static_unique Images_coords.montagne1
let montagne2 = static_unique Images_coords.montagne2
let montagne_village = static_unique Images_coords.montagne_village

let paroi = static_unique Images_coords.paroi

let chemin1 = static_unique Images_coords.chemin1
let chemin2 = static_unique Images_coords.chemin2
let chemin3 = static_unique Images_coords.chemin3
let chemin4 = static_unique Images_coords.chemin4
let chemin5 = static_unique Images_coords.chemin5

let chemin_herbe1 = static_unique Images_coords.chemin_herbe1
let chemin_herbe2 = static_unique Images_coords.chemin_herbe2

let tas1 = static_unique Images_coords.tas1
let tas2 = static_unique Images_coords.tas2

let pont = static_unique Images_coords.pont
let pont_debut = static_unique Images_coords.pont_debut
let pont_fin = static_unique Images_coords.pont_fin
let pont_tablier = static_unique Images_coords.pont_tablier
let poutre_pont = static_unique Images_coords.poutre_pont
let poutre_pont_horizontale = static_unique Images_coords.poutre_pont_horizontale
let support_pont = static_unique Images_coords.support_pont

let piquet = static_unique Images_coords.piquet

let barriere_casse = static_unique Images_coords.barriere_casse

let maison = static_unique Images_coords.maison

let cabane_outils = static_unique Images_coords.cabane_outils

let escaliers = static_unique Images_coords.escaliers

let fond_foret1 = static_unique Images_coords.fond_foret
let fond_foret2 = static (Filter.flip_horizontally (from Images_coords.fond_foret 0))

let lac =
  let d = 14 in
  (* TODO: Il semblerait que les deux images des sapins et du lac ne s'emboitent pas comme prévu. *)
  let lac =
    Subimage.combine [
        (from Images_coords.lac 0, (0, d)) ;
        (from Images_coords.sapins 0, (0, 0))
      ] in
  let lac = Filter.shimmer ~quantity:220 ~amplitude:10 ~duration:45 ~direction:(-0.2, 1.) lac in
  let lac = loop (to_sequence 0.15 lac) in
  Animation.combine [
      (lac, (0, 0)) ;
      (sapins, (0, 0))
    ]

let mare =
  let mare = from Images_coords.mare 0 in
  let mare = Filter.shimmer ~quantity:40 ~amplitude:3 ~duration:20 ~direction:(-0.2, 1.) mare in
  loop (to_sequence 0.3 mare)

let ruisseau =
  let sfrom = seq_from (to_sequence 0.1 (fromlist Images_coords.ruisseau)) in
  Animation.nd_transitions () (fun () ->
    (sfrom 0, only_tau () [
        (5, [], ()) ;
        (1, sfrom 1, ()) ;
        (1, sfrom 2, ())
      ]))

let puit = static_unique Images_coords.puit
let rail_stop = static_unique Images_coords.rail_stop
let rampe = static_unique Images_coords.rampe

let grotte_fond1 = static_unique Images_coords.grotte_fond
let grotte_fond2 = static (Filter.flip_horizontally (from Images_coords.grotte_fond 0))
let stalactique = static_unique Images_coords.stalactique
let stalagmite = static_unique Images_coords.stalagmite

let traces1 = static (from Images_coords.traces 0)
let traces2 = static (from Images_coords.traces 1)
let traces3 = static (from Images_coords.traces 2)

(* * Mobilier urbain *)

let banc = static_unique Images_coords.banc

let poubelle = static_unique Images_coords.poubelle

let barriere = static_unique Images_coords.barriere

let poteau = static_unique Images_coords.poteau

let barriere_travaux =
  let mk = from Images_coords.barriere_travaux in
  rwind (mk 0) (mk_sequence_range 0.25 mk 1 4)

let arceau_velo = static_unique Images_coords.arceau

let egouts_ouverts = static_unique Images_coords.egouts_ouverts
let plaque_egouts = static_unique Images_coords.plaque_egouts

let grille_ventilation = static_unique Images_coords.grille_ventilation

let affiche1 = static_unique Images_coords.affiche1
let affiche2 = static_unique Images_coords.affiche2

let plaque_rue = static_unique Images_coords.plaque_rue

let boite_aux_lettres = static_unique Images_coords.boite_aux_lettres

let lampe_droit = static_unique Images_coords.lampe
let lampe_gauche = static (Filter.flip_horizontally (from Images_coords.lampe 0))

let borne_incendie = static_unique Images_coords.borne_incendie

let horloge = static_unique Images_coords.horloge

let espace_arbre = static_unique Images_coords.espace_arbre

let paves = static_unique Images_coords.paves

let bac_a_sable = static_unique Images_coords.bac_a_sable

let balancoire =
  let mk = from Images_coords.balancoire in
  rwind (mk 0) (mk_sequence_range 0.3 mk 1 4)

let balancoire_ressort = static_unique Images_coords.balancoire_ressort

let publicite_dos = static_unique Images_coords.publicite_dos

let statue = static_unique Images_coords.statue

let immeuble_loin1 = static_unique Images_coords.immeuble_loin1
let immeuble_loin2 = static_unique Images_coords.immeuble_loin2
let immeuble_loin3 = static_unique Images_coords.immeuble_loin3
let immeuble_loin4 = static_unique Images_coords.immeuble_loin_cote1
let immeuble_loin5 = static_unique Images_coords.immeuble_loin_cote2
let immeuble_loin6 = static_unique Images_coords.immeuble_loin_texture

let maison_loin1 = static_unique Images_coords.maison_loin1
let maison_loin2 = static_unique Images_coords.maison_loin2
let maison_loin3 = static_unique Images_coords.maison_loin3
let maison_loin_haute1 = static_unique Images_coords.maison_loin_haute1
let maison_loin_haute2 = static_unique Images_coords.maison_loin_haute2

let tag_visage = static (from Images_coords.tag_divers 0)
let tag_coeur = static (from Images_coords.tag_divers 1)
let tag_nE = static (from Images_coords.tag_divers 2)
let tag_message = static_unique Images_coords.tag_simple
let tag_message_gros = static_unique Images_coords.tag_gros
let tag_A = static (from Images_coords.tag_lettres 0)
let tag_Anar = static (from Images_coords.tag_lettres 1)
let tag_C = static (from Images_coords.tag_lettres 2)
let tag_B = static (from Images_coords.tag_lettres 3)

let tuyau_horizontal = static_unique Images_coords.tuyau_horizontal
let tuyau_vertical = static_unique Images_coords.tuyau_vertical
let tuyau_courbe_bas_gauche = static_unique Images_coords.tuyau_courbe1
let tuyau_courbe_haut_gauche = static_unique Images_coords.tuyau_courbe4
let tuyau_courbe_bas_droite = static_unique Images_coords.tuyau_courbe3
let tuyau_courbe_haut_droite = static_unique Images_coords.tuyau_courbe2
let tuyau_troue = static_unique Images_coords.tuyau_troue

(* * Panneaux *)

let cedez_le_passage = static_unique Images_coords.cedez_le_passage

let panneau_ville = static_unique Images_coords.panneau_ville

(* Monte un panneau sur un support. *)
let monter_panneau =
  let support_panneau =
    let mk = from Images_coords.support_panneau in
    rexplosions (mk 0) [] (mk 1) in
  assert (Animation.check_size support_panneau) ;
  let (dimx_support, _dimy_support) = Subimage.dimensions (Animation.image support_panneau) in
  (* Le panneau n'est pas centré sur l'image, d'où cette différence de 1. *)
  let adhoc_offset = -1 in
  fun img ->
    let (dimx, dimy) = Subimage.dimensions img in
    Animation.combine [
      (static img, (0, 0)) ;
      (support_panneau, ((dimx - dimx_support) / 2 + adhoc_offset, dimy))
    ]

let potion_interdite = monter_panneau (from Images_coords.panneaux 0)
let attention = monter_panneau (from Images_coords.panneaux 1)
let attention_pietons = monter_panneau (from Images_coords.panneaux 2)

let priorite_pieton = monter_panneau (from Images_coords.priorite_pieton 0)

(* * Toits *)

let toit = static_unique Images_coords.toit
let toit_plat = static_unique Images_coords.toit_plat
let toit_faite = static_unique Images_coords.toit_faite
let toit_angle_bas_gauche = static_unique Images_coords.toit_angle_bas_gauche
let toit_angle_gauche = static_unique Images_coords.toit_angle_gauche
let toit_goutiere = static_unique Images_coords.toit_goutiere

let toit_plat_gauche = static_unique Images_coords.toit_plat_gauche
let toit_plat_droit = static_unique Images_coords.toit_plat_droit

let toit_plat_muret1 = static (from Images_coords.toit_plat_muret 0)
let toit_plat_muret2 = static (from Images_coords.toit_plat_muret 1)
let toit_plat_muret3 = static (from Images_coords.toit_plat_muret 2)

let toit_shed_creux = static_unique Images_coords.toit_shed_creux
let toit_shed_debut = static_unique Images_coords.toit_shed_debut
let toit_shed_vitre = static_unique Images_coords.toit_shed_vitre
let toit_shed_fin = static_unique Images_coords.toit_shed_fin
let toit_shed_pente = static_unique Images_coords.toit_shed_pente
let toit_shed_pointe = static_unique Images_coords.toit_shed_pointe

let toit_tuile = static_unique Images_coords.toit_tuile
let toit_tuile_plat = static_unique Images_coords.toit_tuile_plat
let toit_tuile_faite = static_unique Images_coords.toit_tuile_faite
let toit_tuile_angle_bas_droit = static_unique Images_coords.toit_tuile_angle_bas_droit
let toit_tuile_angle_bas_gauche = static_unique Images_coords.toit_tuile_angle_bas_gauche
let toit_tuile_angle_droit = static_unique Images_coords.toit_tuile_angle_droit
let toit_tuile_angle_gauche = static_unique Images_coords.toit_tuile_angle_gauche
let toit_tuile_fenetre_couverture = static_unique Images_coords.toit_tuile_fenetre_couverture
let toit_tuile_pignon = static_unique Images_coords.toit_tuile_pignon

let antenne =
  let mk = from Images_coords.antenne in
  rwind (mk 0) (mk_sequence_range 0.2 mk 1 2)

let chemine = static_unique Images_coords.chemine
let chemines = static_unique Images_coords.chemines
let petite_cheminee = static_unique Images_coords.petite_cheminee
let sortie_air = static_unique Images_coords.sortie_air

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
  Animation.react (static (mk 0)) Event.[RandomFlicker] (mk_sequence 0.05 mk [1])

let fenetre_cote_gauche =
  let mk = from Images_coords.fenetre_cote in
  rexplosions (mk 0) [] (mk 1)

let fenetre_cote_droit =
  let mk i = Filter.flip_horizontally (from Images_coords.fenetre_cote i) in
  rexplosions (mk 0) [] (mk 1)

let fenetre_face1 = static (from Images_coords.fenetre_face 0)
let fenetre_face2 = static (from Images_coords.fenetre_face 1)

let fenetre_haute = static_unique Images_coords.fenetre_haute

let fenetre_large = static_unique Images_coords.fenetre_large

let fenetre_ouverte = static_unique Images_coords.fenetre_ouverte

let fenetre_ronde = static_unique Images_coords.fenetre_ronde

(* * Portes *)

let arche = static_unique Images_coords.arche

let porte_immeuble = static_unique Images_coords.porte_immeuble
let porte_variante = static_unique Images_coords.porte_variante
let porte_commerce = static_unique Images_coords.porte_commerce
let porte_vitre_commerce = static_unique Images_coords.porte_vitre_commerce
let porte_services = static_unique Images_coords.porte_services

let store_terrasse =
  let mk = from Images_coords.store_terrasse in
  rwind (mk 0) (mk_sequence_range 0.2 mk 1 4)

(* * Murs *)

let briques1 = static_unique Images_coords.briques1
let briques2 = static_unique Images_coords.briques2

let briques_cotes = static_unique Images_coords.briques_cotes
let briques_ligne = static_unique Images_coords.briques_ligne

let mur_abime = static_unique Images_coords.mur_abime

let texture_fond1 = static (from Images_coords.texture_fond 0)
let texture_fond2 = static (from Images_coords.texture_fond 1)
let texture_mur1a = static (from Images_coords.texture_mur 0)
let texture_mur1b = static (Filter.flip_vertically (from Images_coords.texture_mur 0))
let texture_mur1_toit_gauche = triangle_left (from Images_coords.texture_mur 0)
let texture_mur1_toit_droit = triangle_right (from Images_coords.texture_mur 0)
let texture_mur2a = static (from Images_coords.texture_mur 1)
let texture_mur2b = static (Filter.flip_vertically (from Images_coords.texture_mur 1))
let texture_mur2_toit_gauche = triangle_left (from Images_coords.texture_mur 1)
let texture_mur2_toit_droit = triangle_right (from Images_coords.texture_mur 1)
let texture_mur3a = static (from Images_coords.texture_mur 2)
let texture_mur3b = static (Filter.flip_horizontally (from Images_coords.texture_mur 2))
let texture_mur3_toit_gauche = triangle_left (from Images_coords.texture_mur 2)
let texture_mur3_toit_droit = triangle_right (from Images_coords.texture_mur 2)

(* * Ciel et effets d'air *)

let etoile =
  let mk = from Images_coords.etoile in
  Animation.react (static (mk 0)) Event.[RandomFlicker] (mk_sequence 0.05 mk [2; 1; 2])

let nuage1 = static (from Images_coords.nuage 0)
let nuage2 = static (from Images_coords.nuage 1)
let nuage3 = static (from Images_coords.nuage 2)
let nuage4 = static (from Images_coords.nuage 3)

let soleil =
  let mk = from Images_coords.soleil in
  Animation.react (static (mk 0)) Event.[RandomRare] (mk_sequence_range 0.05 mk 1 3)

let explosion_potion =
  let mk = from Images_coords.explosion_potion in
  once (mk_sequence_range 0.05 mk 0 6)

let trainee_loin =
  let mk = from Images_coords.trainee_loin in
  once (mk_sequence_range 0.1 mk 0 12)


(* * Interface *)

(* TODO: Générer ces buttons à partir des boutons de l'interface, avec une animation qui appuie sur la touche. *)
let touche_droite = static_unique Images_coords.touche_droite

let bouton_fleche_droite = from Images_coords.boutons 1
let bouton_fleche_gauche = Filter.flip_horizontally bouton_fleche_droite
let bouton_fleche_haut = Filter.flip_diagonally bouton_fleche_droite
let bouton_fleche_bas = Filter.flip_vertically bouton_fleche_droite

let bouton_fleche_double_droite = from Images_coords.boutons 5
let bouton_fleche_double_gauche = Filter.flip_horizontally bouton_fleche_double_droite
let bouton_fleche_double_haut = Filter.flip_diagonally bouton_fleche_double_droite
let bouton_fleche_double_bas = Filter.flip_vertically bouton_fleche_double_droite

let bouton_ajout = from Images_coords.boutons 0
let bouton_supprimer = from Images_coords.boutons 2
let bouton_enregistrer = from Images_coords.boutons 8
let bouton_deplacer = from Images_coords.boutons 4
let bouton_modifier = from Images_coords.boutons 6
let bouton_animer = from Images_coords.boutons 9
let bouton_etage = from Images_coords.boutons 10
let bouton_automatique = from Images_coords.boutons 7
let bouton_valider = from Images_coords.boutons 5
let bouton_copier = from Images_coords.boutons 13

let bouton_menu_points = from Images_coords.boutons 11
let bouton_menu_lignes = from Images_coords.boutons 12

