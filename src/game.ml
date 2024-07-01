
module Launch (I : Interface.T) = struct

  open I

  module E = Engine.Engine (I)

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
    let rec play () =
      let* () =
        wait interface 300 (fun () ->
          let* () = E.step () in
          (* TODO: Move player here. *)
          return ()) in
      play () in
    play ()

  let _ : unit I.m =
    let* interface = E.interface in
    let* () = play_level level in
    I.quit interface

end

