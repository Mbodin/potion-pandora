
module Launch (I : Interface.T) = struct

  open I

  module Display = struct
    let width = 120
    let height = 80
  end

  module E = Engine.Engine (Display) (I)

  (* First step: prepare the levels. *)
  let level =
    let store = Store.create () in
    (* TODO: Load levels created from an editor, not this placeholder made by hand. *)
    let player_animation = (* TODO *) Animation.static World.Perso.normal in
    let player = Store.add store player_animation (0, 0) in
    (player, store)

  let play_level (player, store) =
    let* interface = E.interface in
    E.load store ;
    let quit = ref false in
    let* () = I.on_quit interface (fun () -> quit := true; I.quit interface) in
    let rec play () =
      let* () =
        wait interface 200 (fun () ->
          let* () = E.step () in
          (* TODO: Move player here. *)
          return ()) in
      if !quit then return ()
      else play () in
    play ()

  let _ : unit I.m =
    let* interface = E.interface in
    let* () = play_level level in
    return ()

end

