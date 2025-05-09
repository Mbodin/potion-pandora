
(* Scale factor between the internal pixels of the game, and the pixels actually displayed
  on screen. *)
let scale_factor = 10

type t = {
  renderer : Sdlrender.t ;
  image : Image.image
}

(* We use the identity monad, relying on the SDL effects. *)
type 'a m = 'a
let return v = v
let ( let* ) v f = f v

let init width height =
  Sdl.init [`VIDEO] ;
  let window =
    Sdlwindow.create
      ~pos:(`centered, `centered)
      ~dims:(width * scale_factor, height * scale_factor)
      ~title:"Potion Pandora"
      ~flags:[Sdlwindow.Shown] in
  let renderer =
    let open Sdlrender in
    create_renderer ~win:window ~index:(-1) ~flags:[Accelerated; PresentVSync] in
  let image = Image.create_rgb width height in
  Image.fill_rgb image 0 0 0 ;
  { renderer ; image }

let write { image ; _ } (r, g, b) (x, y) =
  assert (x >= 0 && y >= 0) ;
  assert (x < image.Image.width && y < image.Image.height) ;
  Image.write_rgb image x y r g b

let flush { renderer ; image } =
  Sdlrender.set_draw_color3 renderer ~r:0 ~g:0 ~b:0 ~a:255 ;
  Sdlrender.clear renderer ;
  for x = 0 to image.Image.width - 1 do
    for y = 0 to image.Image.height - 1 do
      Image.read_rgb image x y (fun r g b ->
        let rect =
          Sdlrect.make4 ~x:(scale_factor * x) ~y:(scale_factor * y)
            ~w:scale_factor ~h:scale_factor in
        Sdlrender.set_draw_color3 renderer ~r ~g ~b ~a:255 ;
        Sdlrender.fill_rect renderer rect)
    done
  done ;
  Sdlrender.render_present renderer ;
  Image.fill_rgb image 0 0 0

let set_event default =
  let e = ref default in
  let call_e a = !e a in
  let on_e _ f = e := f in
  (on_e, call_e)

let on_click, call_click =
  set_event (fun _ -> ())

let on_move, call_move =
  set_event (fun _ _ -> ())

let on_drag, call_drag =
  set_event (fun _ _ -> ())

let on_key_pressed, call_key_pressed =
  set_event (fun _ -> ())

let on_quit, call_quit =
  set_event (fun () -> ())

(* Place when the mouse was last pressed down. *)
let mouse_down = ref None

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
      if e.mb_button = 0 then
        mouse_down := Some (convert_coords (e.mb_x, e.mb_y))
    | Mouse_Motion e ->
      (match !mouse_down with
       | None -> ()
       | Some (init_x, init_y) ->
         call_move (init_x, init_y) (convert_coords (e.mm_x, e.mm_y)))
    | Mouse_Button_Up e ->
      if e.mb_button = 0 then (
        match !mouse_down with
        | None -> () (* The mouse probably pressed outside the window, then moved into it. *)
        | Some (init_x, init_y) ->
          let (x, y) = convert_coords (e.mb_x, e.mb_y) in
          if (init_x, init_y) = (x, y) then call_click (x, y)
          else call_drag (init_x, init_y) (x, y)
      )
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

let run g = g ()

