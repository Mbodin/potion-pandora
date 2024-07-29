
module type Display = sig
  val width : int
  val height : int
end

module Engine (D : Display) (I : Interface.T) = struct

  let interface = I.init D.width D.height

  (* The current selected game level. *)
  let level = ref (Store.create ())

  let load st =
    level := st

  (* For-loops within monads. *)
  let rec for_ min max f =
    let open I in
    if min > max then return ()
    else
      let* () = f min in
      for_ (min + 1) max f

  (* Iteration on lists within monads. *)
  let iter_ f l =
    let open I in
    List.fold_left (fun u v ->
      let* () = u in
      f v) (return ()) l

  (* Display an Animation.image into the interface with its lower left corner at
    the provided coordinates.
    The interface coordinates and the image coordinates (0 is up) are different
    from the game coordinates (0 is down), so some computations have to take place. *)
  let display_image img (coord_x, coord_y) =
    let (dim_x, dim_y) = Animation.image_dimensions img in
    let min_x = max 0 (-coord_x) in
    let max_x = min (dim_x - 1) (D.width - coord_x - 1) in
    let min_y = max 0 (dim_y + coord_y - D.height) in
    let max_y = min (dim_y - 1) (dim_y + coord_y - 1) in
    let open I in
    let* interface in
    for_ min_x max_x (fun x ->
      for_ min_y max_y (fun y ->
        let (r, g, b, a) = Animation.read_image img (x, y) in
        match a with
        | 0 -> return ()
        | 255 -> I.write interface (r, g, b) (coord_x + x, D.height - dim_y - coord_y + y)
        | _ -> assert false))

  let step screen_coords =
    let open I in
    let* interface in
    (* The display coordinates. *)
    let min_coords = (fst screen_coords - D.width / 2, snd screen_coords - D.height / 2) in
    let max_coords = (fst min_coords + D.width, snd min_coords + D.height) in
    Store.step !level min_coords max_coords ;
    let* () =
      iter_
        (fun obj ->
          let image = Animation.image (Store.get_display !level obj) in
          let raw_coords = Store.get_coords !level obj in
          let diff_coords =
            (fst raw_coords - fst min_coords, snd raw_coords - snd min_coords) in
          let coords = Projection.to_screen (Store.get_level !level obj) diff_coords in
          display_image image coords)
        (Store.all !level min_coords max_coords) in
    I.flush interface

end

