open Js_of_ocaml
open Js_of_ocaml_lwt

module Html = Dom_html

type t = {
  width : int ;
  height : int ;
  canvas : Html.canvasElement Js.t ;
  context : Html.canvasRenderingContext2D Js.t ;
  pixel : Html.imageData Js.t
}

type 'a m = 'a Lwt.t
let return v = Lwt.return v
let ( let* ) = Lwt.bind

let init width height =
  let canvas = Html.createCanvas Html.document in
  canvas##.width := width ;
  canvas##.height := height ;
  Dom.appendChild Html.document##.body canvas ;
  let context = canvas##getContext Html._2d_ in
  return {
    width ;
    height ;
    canvas ;
    context ;
    pixel = context##createImageData 1 1
  }

let write { width ; height ; context ; pixel ; _ } (r, g, b) (x, y) =
  assert (x >= 0 && y >= 0) ;
  assert (x < width && y < height) ;
  Html.pixel_set pixel##.data 0 r ;
  Html.pixel_set pixel##.data 1 g ;
  Html.pixel_set pixel##.data 2 b ;
  Html.pixel_set pixel##.data 3 255 ;
  context##putImageData pixel (Float.of_int x) (Float.of_int y) ;
  return ()

let flush _ =
  Lwt.pause ()

(* Given a canvas coordinate, computes in which pixel of the canvas it happened. *)
let get_pixel_from_event { width ; height ; canvas ; _ } (x, y) =
  let mx = canvas##.offsetWidth in
  let my = canvas##.offsetHeight in
  let c w x mx = Float.to_int (Float.of_int w *. Float.of_int x /. Float.of_int mx) in
  (c width x mx, c height y my)

let on_click =
  let thread = ref (Lwt.return ()) in fun t f ->
    Lwt.cancel !thread ;
    thread :=
      Lwt_js_events.clicks canvas (fun ev _ ->
        let (x, y) = get_pixel_from_event t (ev##.offsetX, ev##.offsetY) in
        f x y) ;
    !thread

let on_move =
  let thread = ref (Lwt.return ()) in fun t f ->
    Lwt.cancel !thread ;
    thread :=
      Lwt_js_events.mouusemoves canvas (fun ev _ ->
        let (x, y) = get_pixel_from_event t (ev##.offsetX, ev##.offsetY) in
        f x y)
    !thread

let set_event default =
  let e = ref default in
  let call_e a = !e a in
  let on_e _ f = e := f in
  (on_e, call_e)

let on_click, call_click =
  set_event (fun _ -> ())

let on_move, call_move =
  set_event (fun _ _ -> ())

let on_key_pressed, call_key_pressed =
  set_event (fun _ -> ())

let on_quit, call_quit =
  set_event (fun () -> ())

let rec process_events () =
  let convert_coords (x, y) =
    (x / scale_factor, y / scale_factor) in
  let open Sdlevent in
  let aux = function
    | KeyDown { scancode = Sdlscancode.A ; _ }
    | KeyDown { scancode = Sdlscancode.H ; _ }
    | KeyDown { scancode = Sdlscancode.LEFT ; _ }
    | KeyDown { keycode = Sdlkeycode.Left ; _ }
      -> call_key_pressed (Some Potion_pandora.Interface.West)
    | KeyDown { scancode = Sdlscancode.D ; _ }
    | KeyDown { scancode = Sdlscancode.L ; _ }
    | KeyDown { scancode = Sdlscancode.RIGHT ; _ }
    | KeyDown { keycode = Sdlkeycode.Right ; _ }
      -> call_key_pressed (Some Potion_pandora.Interface.East)
    | KeyDown { scancode = Sdlscancode.W ; _ }
    | KeyDown { scancode = Sdlscancode.K ; _ }
    | KeyDown { scancode = Sdlscancode.UP ; _ }
    | KeyDown { keycode = Sdlkeycode.Up ; _ }
      -> call_key_pressed (Some Potion_pandora.Interface.North)
    | KeyDown { scancode = Sdlscancode.S ; _ }
    | KeyDown { scancode = Sdlscancode.J ; _ }
    | KeyDown { scancode = Sdlscancode.DOWN ; _ }
    | KeyDown { keycode = Sdlkeycode.Down ; _ }
      -> call_key_pressed (Some Potion_pandora.Interface.South)
    | KeyDown { scancode = Sdlscancode.ESCAPE ; _ }
    | KeyDown { keycode = Sdlkeycode.Escape ; _ }
      -> call_quit ()
    | KeyDown _ -> call_key_pressed None
    | Mouse_Button_Down e ->
      call_click (convert_coords (e.mb_x, e.mb_y))
    | Mouse_Motion e ->
      call_move
        (convert_coords (e.mm_x - e.mm_xrel, e.mm_y - e.mm_yrel))
        (convert_coords (e.mm_x, e.mm_y))
    | Window_Event { kind = WindowEvent_Close ; _ } -> call_quit ()
    | _ -> () in
  match poll_event () with
  | None -> ()
  | Some e ->
    aux e ;
    process_events ()

(* A global variable to detect when quitting has been requested,
  hence preventing the program to launch anything that might take time. *)
let has_quit = ref false

let quit _ =
  has_quit := true ;
  Sdl.quit ()

let wait _ time f =
  let before = Sdltimer.get_ticks () in
  let r = f () in
  process_events () ;
  let diff = Sdltimer.get_ticks () - before in
  if time > diff && not !has_quit then
    Sdltimer.delay ~ms:(time - diff) ;
  r

