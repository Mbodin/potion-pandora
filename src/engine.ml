
module type Display = sig
  val width : int
  val height : int
end

module Engine (D : Display) (I : Interface.T) = struct

  let interface = I.init D.width D.height

  (* The current selected level. *)
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

  (* Display an Animation.image into the interface at the provided coordinates. *)
  let display_image img (coord_x, coord_y) =
    let (dim_x, dim_y) = Animation.image_dimensions img in
    let min_x = max 0 (-coord_x) in
    let max_x = min (dim_x - 1) (D.width - coord_x - 1) in
    let min_y = max 0 (-coord_y) in
    let max_y = min (dim_y - 1) (D.height - coord_y - 1) in
    let open I in
    let* interface in
    for_ min_x max_x (fun x ->
      for_ min_y max_y (fun y ->
        let (r, g, b, a) = Animation.read_image img (x, y) in
        match a with
        | 0 -> return ()
        | 255 -> I.write interface (r, g, b) (coord_x + x, coord_y + y)
        | _ -> assert false))

  let step () =
    let open I in
    let* interface in
    Store.step !level ;
    (* The display coordinates. *)
    let coords = (0, 0) (* TODO: stay close to the player. *) in
    let min_coords = (fst coords - D.width / 2, snd coords - D.height / 2) in
    let max_coords = (fst min_coords + D.width - 1, snd min_coords + D.height - 1) in
    let* () =
      iter_
        (fun (coord, anim) -> display_image (Animation.image anim) coord)
        (Store.all !level min_coords max_coords) in
    I.flush interface

end

