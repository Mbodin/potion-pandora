
(* Width of the button panel. *)
let panel_width = 100

(* Height of the panel reserved for the mode buttons. *)
let button_height = 10

(* Height of the panel reserved for the elements. *)
let element_height = 50

(* Width and height of the main display. *)
let display_width = 120
let display_height = 60


module Start (I : Interface.T) = struct

  module MainLayout = Screen.SplitHorizontal (I)

  module ButtonLayout = Screen.SplitVertical (MainLayout.Left)

  module MonadOps = Monad.Ops (I)
  open MonadOps
  open I

  type mode =
    | View (* Move within the level. *)
    | Add (* Adding new elements to the level. *)
    | Remove (* Removing elements to the level. *)
    | Move (* Move elements of the level. *)

  let current_mode = ref View
  let current_element = ref Items.potion

  let store = Store.create ()

  let screen_coords = ref (0, 0)

  let set_view_ref = ref (fun _ -> failwith "set_view is unset.")
  let set_view () = !set_view_ref true

  let view_actions = Screen.{
    on_press = (fun _ -> current_mode := View ; return ()) ;
    on_release = (fun _ -> return ()) ;
    set_toggle = (fun f -> set_view_ref := f ; return ())
  }

  let add_actions = Screen.{
    on_press = (fun _ -> current_mode := Add ; return ()) ;
    on_release = (fun _ -> return ()) ;
    set_toggle = (fun _ -> return ())
  }

  let remove_actions = Screen.{
    on_press = (fun _ -> current_mode := Remove ; return ()) ;
    on_release = (fun _ -> return ()) ;
    set_toggle = (fun _ -> return ())
  }

  let move_actions = Screen.{
    on_press = (fun _ -> current_mode := Move ; return ()) ;
    on_release = (fun _ -> return ()) ;
    set_toggle = (fun _ -> return ())
  }

  let save_actions =
    let save () =
      () (* TODO: Create a file. *) in
    Screen.{
      on_press = (fun _ -> save () ; set_view ()) ;
      on_release = (fun _ -> return ()) ;
      set_toggle = (fun _ -> return ())
    }

  module KindButtonsList : Screen.ButtonInputs with type 'a m = 'a I.m = struct

    type 'a m = 'a I.m

    let width = panel_width
    let height = button_height

    let buttons =
      return [
        (Items.bouton_ajout, add_actions) ;
        (Items.bouton_supprimer, remove_actions) ;
        (Items.bouton_deplacer, view_actions) ;
        (Items.bouton_modifier, move_actions) ;
        (Items.bouton_enregistrer, save_actions)
      ]

  end

  module KindButtons = Screen.SelectButtons (ButtonLayout.Up) (KindButtonsList)

  module ElementButtonsList : Screen.ButtonInputs with type 'a m = 'a I.m = struct
    (* Technically should be something else as there won't be space for everything on the screen. *)

    type 'a m = 'a I.m

    let width = panel_width
    let height = element_height

    let buttons =
      return (List.map (fun item ->
          (Animation.image item, Screen.{
            on_press = (fun _ -> current_element := item ; return ()) ;
            on_release = (fun _ -> return ()) ;
            set_toggle = (fun _ -> return ())
          })
        ) Items.[
          arbre_loin1 ;
          arbre_loin2 ;
          arbre_loin3 ;
          arbre_loin4 ;
          arbre_loin5 ;
          arbre_loin6 ;
          arbre_loin7 ;
          arbre_loin8 ;
          arbre_loin9 ;
          arbre_loin10
        ])

  end

  module ElementButtons = Screen.SelectButtons (ButtonLayout.Down) (ElementButtonsList)

  module Display = struct

    module D = MainLayout.Right
    include D

    let width = display_width
    let height = display_height

    let interface = D.init width height

    module ScreenOps = Screen.Ops (D)

  end

  (* Display a Subimage.t into the interface with its lower left corner at
    the provided coordinates.
    The interface coordinates and the image coordinates (0 is up) are different
    from the game coordinates (0 is down), so some computations have to take place. *)
  let display_image img (coord_x, coord_y) =
    let (_dim_x, dim_y) = Subimage.dimensions img in
    let open I in
    let* interface = Display.interface in
    Display.ScreenOps.display_image interface img (coord_x, Display.height - dim_y - coord_y)

  let u =
    let* () = KindButtons.u in
    let* () = ElementButtons.u in
    let* interface = Display.interface in
    let rec loop () : unit I.m =
      Display.wait interface (1000 / Animation.frames_per_second) (fun () ->
        let min_coords =
          (fst !screen_coords - Display.width / 2, snd !screen_coords - Display.height / 2) in
        let max_coords = (fst min_coords + Display.width, snd min_coords + Display.height) in
        let* () =
          iter_ (fun obj ->
            let image = Animation.image (Store.get_display store obj) in
            let raw_coords = Store.get_coords store obj in
            let diff_coords = (fst raw_coords - fst min_coords, snd raw_coords - snd min_coords) in
            let coords = Projection.to_screen (Store.get_level store obj) diff_coords in
            display_image image coords)
            (Store.all_in store min_coords max_coords) in
        let* () = Display.flush interface in
        loop ()) in
    loop ()

end

