
module Start (I : Interface.T) = struct

  module MainLayout = Screen.SplitHorizontal (I)

  module ButtonLayout = Screen.SplitVertical (MainLayout.Left)

  open I

  type mode =
    | View (* Move within the level. *)
    | Add (* Adding new elements to the level. *)
    | Remove (* Removing elements to the level. *)
    | Move (* Move elements of the level. *)

  let current_mode = ref View

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

    let width = 100
    let height = 10
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

  (* TODO: ButtonLayout.Down should display images to place on the level. *)

  (* TODO: MainLayout.Right should display the current level. *)

  let u =
    let* () = KindButtons.u in
    return ()

end

