open Js_of_ocaml
open Js_of_ocaml_lwt

module Html = Dom_html

type t = unit

type obj = {
  width : int ;
  height : int ;
  canvas : Html.canvasElement Js.t ;
  context : Html.canvasRenderingContext2D Js.t ;
  data : Html.imageData Js.t
}

type 'a m = 'a Lwt.t
let return v = Lwt.return v
let ( let* ) = Lwt.bind

let global_object = ref None

let get_object () =
  match !global_object with
  | None -> failwith "Uninitialised interface."
  | Some obj -> obj

let dimensions () =
  let { width ; height ; _ } = get_object () in
  return (width, height)

(* Given a canvas coordinate, computes in which pixel of the canvas it happened. *)
let convert_coords (x, y) =
  let { width ; height ; canvas ; _ } = get_object () in
  let mx = canvas##.offsetWidth in
  let my = canvas##.offsetHeight in
  let c w x mx = Float.to_int (Float.of_int w *. Float.of_int x /. Float.of_int mx) in
  (c width x mx, c height y my)

let set_event default =
  let e = ref default in
  let call_e a = !e a in
  let on_e t f = e := f ; return () in
  (on_e, call_e)

let on_click, call_click =
  set_event (fun _ -> return ())

let on_move, call_move =
  set_event (fun _ _ -> return ())

let on_drag, call_drag =
  set_event (fun _ _ -> return ())

let on_key_pressed, call_key_pressed =
  set_event (fun _ -> return ())

let on_quit, _call_quit =
  set_event (fun () -> return ())

(* Place when the mouse was last pressed down. *)
let mouse_down = ref None

let to_int (n : Js.number_t) : int =
  int_of_float (Js.to_float n)

let init width height =
  assert (!global_object = None) ;
  let canvas = Html.createCanvas Html.document in
  canvas##.width := width ;
  canvas##.height := height ;
  Dom.appendChild Html.document##.body canvas ;
  let context = canvas##getContext Html._2d_ in
  context##.fillStyle := Js.string "black" ;
  context##fillRect (Js.float 0.) (Js.float 0.)
    (Js.float (Float.of_int width)) (Js.float (Float.of_int height)) ;
  global_object :=
    Some {
      width ;
      height ;
      canvas ;
      context ;
      data =
        context##getImageData (Js.float 0.) (Js.float 0.)
          (Js.float (Float.of_int width)) (Js.float (Float.of_int height))
    } ;
  Lwt.async (fun () ->
    Lwt_js_events.mousedowns canvas
      (fun event _thread ->
        if event##.button = 0 then
          mouse_down := Some (convert_coords (to_int event##.offsetX, to_int event##.offsetY)) ;
        return ()
      )) ;
  Lwt.async (fun () ->
    Lwt_js_events.mousemoves canvas
      (fun event _thread ->
        match !mouse_down with
        | None -> return ()
        | Some (init_x, init_y) ->
          call_move (init_x, init_y) (convert_coords (to_int event##.offsetX, to_int event##.offsetY))
      )) ;
  Lwt.async (fun () ->
    Lwt_js_events.mouseups canvas
      (fun event _thread ->
        if event##.button = 0 then (
          match !mouse_down with
          | None -> return ()
          | Some (init_x, init_y) ->
            let (x, y) = convert_coords (to_int event##.offsetX, to_int event##.offsetY) in
            if (init_x, init_y) = (x, y) then call_click (x, y)
            else call_drag (init_x, init_y) (x, y)
        ) else return ()
      )) ;
  Lwt.async (fun () ->
    Lwt_js_events.keydowns Html.document
      (fun event _thread ->
        let get key = Option.map Js.to_string (Js.Optdef.to_option key) in
        match get event##.code, get event##.key with
        | Some ("KeyA" | "KeyH" | "ArrowLeft"), _
        | _, Some "ArrowLeft"
          -> call_key_pressed (Some Potion_pandora.Interface.West)
        | Some ("KeyD" | "KeyL" | "ArrowRight"), _
        | _, Some "ArrowRight"
          -> call_key_pressed (Some Potion_pandora.Interface.East)
        | Some ("KeyW" | "KeyK" | "ArrowUp"), _
        | _, Some "ArrowUp"
          -> call_key_pressed (Some Potion_pandora.Interface.North)
        | Some ("KeyS" | "KeyJ" | "ArrowDown"), _
        | _, Some "ArrowDown"
          -> call_key_pressed (Some Potion_pandora.Interface.South)
        | _, _ -> call_key_pressed None
      )) ;
  (* One could add an event on_quit linked to the beforeunload Javascript event,
    but it seems that the corresponding binding is lacking Lwt_js_events. *)
  return ()

let flush () =
  let { width ; height ; context ; data ; _ } = get_object () in
  context##putImageData data (Js.float 0.) (Js.float 0.) ;
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let index = 4 * (y * width + x) in
      Html.pixel_set data##.data (index + 0) 0 ;
      Html.pixel_set data##.data (index + 1) 0 ;
      Html.pixel_set data##.data (index + 2) 0 ;
      Html.pixel_set data##.data (index + 3) 255
    done
  done ;
  Lwt_js.yield ()

let write () (r, g, b) (x, y) =
  let { width ; height ; context ; data ; _ } = get_object () in
  assert (x >= 0 && y >= 0) ;
  assert (x < width && y < height) ;
  let index = 4 * (y * width + x) in
  Html.pixel_set data##.data (index + 0) r ;
  Html.pixel_set data##.data (index + 1) g ;
  Html.pixel_set data##.data (index + 2) b ;
  Html.pixel_set data##.data (index + 3) 255 ;
  return ()

let quit () =
  let { canvas ; _ } = get_object () in
  ignore (Html.document##.body##removeChild (canvas :> Dom.node Js.t)) ;
  return ()

let wait () time f =
  let res = ref None in
  let run () =
    let* r = f () in
    res := Some r ;
    return () in
  let* () = Lwt.join [ Lwt_js.sleep (Float.of_int time /. 1000.) ; run () ] in
  match !res with
  | None -> assert false
  | Some r -> return r

let run = Lwt.async

