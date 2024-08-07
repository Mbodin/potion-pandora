
let () = Random.self_init ()

module Launch (I : Interface.T) = struct

  open I

  module Display = struct
    let width = 120
    let height = 80
  end

  module E = Engine.Engine (Display) (I)

  (* The center of the screen coordinates. *)
  let screen_coords = ref (0, 0)

  (* Direction towards which the screen ist meant to focus.
    true: to the left, false: to the right. *)
  let screen_direction = ref false

  (* Ratio of the screen where the player character is placed. *)
  let focus_ratio = 25
  (* Acceptable error around the previous value (as a percentage of the screen dimension). *)
  let dist_ratio = 7

  (* First step: prepare the levels. *)
  let level_store =
    let store = Store.create () in
    (* TODO: Load levels created from an editor, not this placeholder made by hand. *)
    ignore (Store.add store Items.cerf_volant (0, 40)) ;
    ignore (Store.add store Items.potion (-40, 0)) ;
    ignore (Store.add store Items.petite_potion (-60, 0)) ;
    ignore (Store.add store Items.plante1_tres_sombre (49, 0)) ;
    ignore (Store.add store Items.plante1_sombre (21, 0)) ;
    ignore (Store.add store Items.plante1 (-21, 0)) ;
    ignore (Store.add store Items.buisson (-7, 0)) ;
    ignore (Store.add store Items.fleur1 (7, 0)) ;
    ignore (Store.add store Items.fleur2 (0, 0)) ;
    ignore (Store.add store Items.arbre1 (70, 0)) ;
    ignore (Store.add store Items.arbre2 (90, 0)) ;
    ignore (Store.add store Items.arbre3 (110, 0)) ;
    ignore (Store.add store Items.chemin1 (125, 0)) ;
    ignore (Store.add store Items.enfant (170, 0)) ;
    for i = 0 to 10 do
      ignore (Store.add store Items.epi_ble (200 + 3 * i, 0)) ;
    done ;
    ignore (Store.add store Items.lac (240, 0)) ;
    ignore (Store.add store Items.mare (370, 0)) ;
    let player = Store.add store Items.Perso.perso (0, 0) in
    ignore (Store.add store Items.arbre1_tres_sombre (130, -25)) ;
    (player, store)

  (* Given coordinates and a direction, move the coordinate in the direction by step. *)
  let follow_direction ?(step=1) (x, y) = function
    | Interface.North -> (x, y + step)
    | Interface.West -> (x - step, y)
    | Interface.South -> (x, y - step)
    | Interface.East -> (x + step, y)

  let play_level (player, store) =
    let* interface = E.interface in
    E.load store ;
    let quit = ref false in
    let* () = I.on_quit interface (fun () -> quit := true; I.quit interface) in
    let target_coords = ref None in
    let* () =
      I.on_click interface (fun (x, y) ->
        (* Interface coordinate are reversed compared to game coordinates. *)
        let click_coords = (x, Display.height - 1 - y) in
        let coords =
          (fst !screen_coords - Display.width / 2 + fst click_coords,
           snd !screen_coords - Display.height / 2 + snd click_coords) in
        target_coords := Some coords ;
        I.return ()) in
    let* () =
      I.on_key_pressed interface (function
        | None -> I.return ()
        | Some d ->
          target_coords := Some (follow_direction ~step:3 (Store.get_coords store player) d) ;
          I.return ()) in
    let rec play () =
      let* () =
        wait interface (1000 / Animation.frames_per_second) (fun () ->
          let* () = E.step !screen_coords in
          (* TODO: Meaningfully move player here, avoiding obstacles and falling if accurate.
            It currently blindly goes where targetted. *)
          let () =
            match !target_coords with
            | Some coords ->
              let x_before = fst (Store.get_coords store player) in
              let x_after = fst coords in
              if x_before < x_after then screen_direction := false
              else if x_before > x_after then screen_direction := true ;
              ignore (Store.move store player coords)
            | None -> () in
          target_coords := None ;
          (* Move screen if needed: the player has to stay in the middle third, below from
            the direction of move. *)
          let new_screen_coords =
            let (min_player_coords, max_player_coords) =
              let (center_x, center_y) = !screen_coords in
              let (screen_low_x, screen_low_y) =
                (center_x - Display.width / 2, center_y - Display.height / 2) in
              let screen_focus_x =
                if !screen_direction then
                  (screen_low_x + (Display.width * (100 - focus_ratio)) / 100)
                else (screen_low_x + (Display.width * focus_ratio) / 100) in
              let screen_focus_y = screen_low_y + Display.height / 3 + 3 in
              let dist_x = (Display.width * dist_ratio) / 100 in
              let dist_y = (Display.height * dist_ratio) / 100 in
              ((screen_focus_x - dist_x, screen_focus_y - dist_y),
               (screen_focus_x + dist_x, screen_focus_y + dist_y)) in
            let player_center =
              let (x, y) = Store.get_coords store player in
              let (dim_x, dim_y) =
                Animation.image_dimensions (Animation.image (Store.get_display store player)) in
              (x + dim_x / 2, y + dim_y / 2) in
            let move_camera target_min target_max current v =
              if v < target_min then current - 1
              else if v > target_max then current + 1
              else current in
            let move_camera_proj proj =
              move_camera
                (proj min_player_coords) (proj max_player_coords)
                (proj !screen_coords) (proj player_center) in
            (move_camera_proj fst, move_camera_proj snd) in
          screen_coords := new_screen_coords ;
          return ()) in
      if !quit then return ()
      else play () in
    play ()

  let _ : unit I.m =
    let* interface = E.interface in
    let* () = play_level level_store in
    return ()

end

