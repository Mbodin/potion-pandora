
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

  module MonadOps = Monad.Ops (I)
  open MonadOps
  module ScreenOps = Screen.Ops (I)

  (* Display a Subimage.t into the interface with its lower left corner at
    the provided coordinates.
    The interface coordinates and the image coordinates (0 is up) are different
    from the game coordinates (0 is down), so some computations have to take place. *)
  let display_image img (coord_x, coord_y) =
    let (dim_x, dim_y) = Subimage.dimensions img in
    let img =
      Subimage.sub img
        (min (dim_x - 1) (D.width - coord_x - 1))
        (min (dim_y - 1) (dim_y + coord_y - 1))
        (0, 0) in
    let open I in
    let* interface in
    ScreenOps.display_image interface img (coord_x, D.height - dim_y - coord_y)

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

