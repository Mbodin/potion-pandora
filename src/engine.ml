
(* The width and height of the screen. *)
let display_width = 120
let display_height = 120

module Engine (I : Interface.T) = struct

  let interface = I.init display_width display_height

  (* The current selected level. *)
  let level = ref (Store.create ())

  let load st =
    level := st

  (* Display an Animation.image into the interface at the provided coordinates. *)
  let display_image img (coord_x, coord_y) =
    let (dim_x, dim_y) = Animation.image_dimensions img in
    let min_x = max 0 (-coord_x) in
    let max_x = min (dim_x - 1) (display_width - coord_x - 1) in
    let min_y = max 0 (-coord_y) in
    let max_y = min (dim_y - 1) (display_height - coord_y - 1) in
    for x = min_x to max_x do
      for y = min_y to max_y do
        let (r, g, b, a) = Animation.read_image img (x, y) in
        match a with
        | 0 -> ()
        | 255 -> I.write interface (r, g, b) (coord_x + x, coord_y + y)
        | _ -> assert false
      done
    done

  let step () =
    Store.step !level ;
    (* The display coordinates. *)
    let coords = (0, 0) (* TODO: stay close to the player. *) in
    let min_coords = (fst coords - display_width / 2, snd coords - display_height / 2) in
    let max_coords = (fst min_coords + display_width - 1, snd min_coords + display_height - 1) in
    List.iter
      (fun (coord, anim) -> display_image (Animation.image anim) coord)
      (Store.all !level min_coords max_coords) 

end

