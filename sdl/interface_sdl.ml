
(* Scale factor between the internal pixels of the game, and the pixels actually displayed
  on screen. *)
let scale_factor = 10

type t = {
  window : Sdlwindow.t ;
  renderer : Sdlrender.t ;
  image : Image.image
}

(* We use the identity monad, relying on the SDL effects. *)
type 'a m = 'a
let return v = v
let ( let* ) v f = f v

let init width height =
  Sdl.init [`VIDEO] ;
  let (window, renderer) =
    Sdlrender.create_window_and_renderer ~width ~height ~flags:[] in
  let image = Image.create_rgb width height in
  Image.fill_rgb image 0 0 0 ;
  { window ; renderer ; image }

let write { image } (r, g, b) (x, y) =
  Image.write_rgb image x y r g b

let flush { window ; renderer ; image } =
  Sdlrender.set_draw_color3 renderer ~r:0 ~g:0 ~b:0 ~a:255 ;
  Sdlrender.clear renderer ;
  for x = 0 to image.Image.width do
    for y = 0 to image.Image.height do
      Image.read_rgb image x y (fun r g b ->
        let rect =
          Sdlrect.make4 ~x:(scale_factor * x) ~y:(scale_factor * y)
            ~w:scale_factor ~h:scale_factor in
        Sdlrender.set_draw_color3 renderer ~r ~g ~b ~a:255 ;
        Sdlrender.draw_rect renderer rect)
    done
  done ;
  Sdlrender.render_present renderer

let on_click _ _ = () (* TODO *)

let on_move _ _ = () (* TODO *)

let on_key_pressed _ _ = () (* TODO *)

let wait _ time f =
  let before = Sdltimer.get_ticks () in
  let r = f () in
  let diff = Sdltimer.get_ticks () - before in
  if time > diff then
    Sdltimer.delay ~ms:(time - diff) ;
  r

let quit _ = Sdl.quit ()

