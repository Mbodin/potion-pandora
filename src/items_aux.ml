
let range a b =
  if a > b then []
  else List.init (b - a + 1) (fun c -> c + a)

(* These are the most used functions from [Animation], and I prefer to avoid global opens. *)
let static = Animation.static
let loop = Animation.loop

let once s =
  let final = static Filter.transparent in
  Animation.force_same_size (Animation.prefix s final)

let from_coords ~bundle ((width, height), (x, y)) =
  Subimage.make ~bundle width height (x, y)

let to_sequence time l =
  List.map (fun i -> (i, time)) l

let mk_sequence time mk l =
  to_sequence time (List.map mk l)

let mk_sequence_range time mk min max =
  assert (max >= min) ;
  mk_sequence time mk (List.init (1 + max - min) (fun i -> i + min))

let triangle_left img =
  let (dimx, _dimy) = Subimage.dimensions img in
  static (Filter.triangle_lower_left img dimx)
let triangle_right img =
  let (dimx, _dimy) = Subimage.dimensions img in
  static (Filter.triangle_lower_right img dimx)

let rwind img s =
  Animation.react (static img) Event.[Wind; Explode] s

let rexplosions img s final =
  Animation.change_with (static img) Event.[Explode] s (static final)

let only_tau st l = function
  | Event.Tau -> l
  | _ -> [(1, [], st)]

let rec seq_from s n =
  match s, n with
  | s, 0 -> s
  | _ :: s, n -> seq_from s (n - 1)
  | _, _ -> assert false

module Make (M : Libsave.Monad.T) = struct

  module Read = Libsave.Read.Make (M)

  let bundled_image : Image.image M.m = [%data_image "all.png"]

  let%test_unit "Generating all2.png, that should be the same than all.png." =
    let image = ImageLib.PNG.bytes_of_png bundled_image in
    let channel = open_out_bin "all2.png" in
    output_bytes channel image ;
    close_out channel

  let from ?bundle coords i =
    assert (i < List.length coords) ;
    let open M in
    let* bundle = Option.fold ~none:bundled_image ~some:return bundle in
    return (from_coords ~bundle (List.nth coords i))

  let fromlist ?bundle coords =
    let open M in
    let* bundle = Option.fold ~none:bundled_image ~some:return bundle in
    return (List.map (from_coords ~bundle) coords)

  let static_unique img =
    assert (List.length img = 1) ;
    static (from img 0)

end

