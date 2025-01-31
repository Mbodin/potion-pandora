open Js_of_ocaml

type t = {
  width : int ;
  height : int ;
  canvas : unit (* TODO *) ;
  context : unit ;
  pixel : unit
}

type 'a m = 'a Lwt.t
let return v = Lwt.return v
let ( let* ) = Lwt.bind

let init width height =
  let canvas = Html.createCanvas Html.window##document in
  canvas##.width := width ;
  canvas##.height := height ;
  Dom.appendChild Html.window##document##body ;
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
  pixel##.data.(0) <- r ;
  pixel##.data.(1) <- g ;
  pixel##.data.(2) <- b ;
  pixel##.data.(3) <- 255 ;
  context##putImageData pixel x y ;
  return () (* TODO *)

let flush _ =
  Lwt.pause ()

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

