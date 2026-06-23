
module Ops (I : Interface.T) = struct

  open I

  module Ops = Monad.Ops (I)
  include Ops

  let display_image interface img (coord_x, coord_y) =
    let (dim_x, dim_y) = Subimage.dimensions img in
    let* (width, height) = dimensions interface in
    let min_x = max 0 (-coord_x) in
    let min_y = max 0 (-coord_y) in
    let max_x = min (dim_x - 1) (width - 1 - coord_x) in
    let max_y = min (dim_y - 1) (height - 1 - coord_y) in
    for_ min_x max_x (fun x ->
      for_ min_y max_y (fun y ->
        let (r, g, b, a) = Subimage.read img (x, y) in
        match a with
        | 0 -> return ()
        | 255 -> write interface (r, g, b) (coord_x + x, coord_y + y)
        | _ -> assert false))

  let horizontal_line interface color ~y x1 x2 =
    let* (width, height) = dimensions interface in
    if y >= 0 && y < height then
      let (x1, x2) = (min x1 x2, max x1 x2) in
      let x1 = max 0 x1 in
      let x2 = min x2 (width - 1) in
      for_ x1 x2 (fun x ->
        write interface color (x, y))
    else return ()

  let vertical_line interface color ~x y1 y2 =
    let* (width, height) = dimensions interface in
    if x >= 0 && x < width then
      let (y1, y2) = (min y1 y2, max y1 y2) in
      let y1 = max 0 y1 in
      let y2 = min y2 (height - 1) in
      for_ y1 y2 (fun y ->
        write interface color (x, y))
    else return ()

  let rectangle interface color (x1, y1) (x2, y2) =
    horizontal_line interface color ~y:y1 x1 x2 %%
    horizontal_line interface color ~y:y2 x1 x2 %%
    vertical_line interface color ~x:x1 y1 y2 %%
    vertical_line interface color ~x:x2 y1 y2

  let fill interface color (x1, y1) (x2, y2) =
    let* (width, _height) = dimensions interface in
    let (x1, x2) = (min x1 x2, max x1 x2) in
    let x1 = max 0 x1 in
    let x2 = min x2 (width - 1) in
    for_ x1 x2 (fun x ->
      vertical_line interface color ~x y1 y2)

end


module Split (I : Interface.T) (Get : sig
      (* From both dimensions, compute both shifts. *)
      val shifts : (int * int) -> (int * int) -> (int * int) * (int * int)
    end) = struct

  (* Functions to be called when the appropriate event occurs. *)
  type event_functions = {
    event_click : ((int * int) -> unit I.m) option ;
    event_move : ((int * int) -> (int * int) -> unit I.m) option ;
    event_drag : ((int * int) -> (int * int) -> unit I.m) option ;
    event_key_pressed : (Interface.direction option -> unit I.m) option ;
    event_quit : (unit -> unit I.m) option
  }

  (* The state of half the screen. *)
  type half_state = {
    shift_x : int (* By how much this subscreen is shifted right. *) ;
    shift_y : int (* By how much this subscreen is shifted up. *) ;
    width : int (* The width of the subscreen. *) ;
    height : int (* The height of the subscreen. *) ;
    event_functions : event_functions
  }

  let none_event_functions = {
    event_click = None ;
    event_move = None ;
    event_drag = None ;
    event_key_pressed = None ;
    event_quit = None
  }

  (* A state, waiting for both [First.init] and [Second.init] to be called. *)
  type state =
    | WaitingBoth (* None of [First.init] and [Second.init] has been called. *)
    | WaitingSecond of (int * int) (* Only [First.init] has been called. *)
    | WaitingFirst of (int * int) (* Only [Second.init] has been called. *)
    | State of I.t * half_state * half_state (* Both has been called and both subscreens are ready. *)
    | ClosedFirst of half_state (* [First.quit] has been called: we continue to store second's state. *)
    | ClosedSecond of half_state (* [Second.quit] has been called: we continue to store first's state. *)
    | Closed (* Both [First.quit] and [Second.quit] have been called. *)

  (* The shared variable between the two created interfaces. *)
  let global_state = ref WaitingBoth

  type 'a m = 'a I.m

  let return = I.return
  let ( let* ) = I.( let* )

  let run = I.run

  let get_states () =
    match !global_state with
    | State (t, st1, st2) -> (t, st1, st2)
    | WaitingBoth | WaitingSecond _ | WaitingFirst _ -> failwith "Split.get_states: not ready."
    | ClosedFirst _ | ClosedSecond _ | Closed -> failwith "Split.get_states: closed."

  let wait (type t) (time : int) (f : unit -> t m) : t m =
    let (t, _st1, _st2) = get_states () in
    I.wait t time f

  (* Check if a global coordinate is within an half interface. *)
  let is_in st (x, y) =
    y >= st.shift_y && y < st.shift_y + st.height
    && x >= st.shift_x && x - st.shift_x < st.width

  (* Convert a global coordinate into a local one. *)
  let convert st (x, y) =
    assert (is_in st (x, y)) ;
    (x - st.shift_x, y - st.shift_y)

  (* Convert a local coordinate into the global one. *)
  let convert_inv st (x, y) =
    assert (x >= 0 && y >= 0 && x < st.width && y < st.height) ;
    (x + st.shift_x, y + st.shift_y)

  (* Set up the [I.on_*] functions to call the relevant function stored in the state. *)
  let setup_on_events () : unit m =
    let open I in
    let run_if_event e k =
      match e with
      | None -> return ()
      | Some f -> k f in
    let (t, _st1, _st2) = get_states () in
    let* () =
      on_click t (fun xy ->
        let (_t, st1, st2) = get_states () in
        if is_in st1 xy then
          run_if_event st1.event_functions.event_click
            (fun f -> f (convert st1 xy))
        else if is_in st2 xy then
          run_if_event st2.event_functions.event_click
            (fun f -> f (convert st2 xy))
        else (* The click was done outside of both subscreens. *) return ()) in
    let on_move_drag f xy_start xy_end =
      let (_t, st1, st2) = get_states () in
      if is_in st1 xy_end then
        let xy_start = if is_in st1 xy_start then xy_start else xy_end in
        run_if_event (f st1) (fun f ->
          f (convert st1 xy_start)
            (convert st1 xy_end))
      else if is_in st2 xy_end then
        let xy_start = if is_in st2 xy_start then xy_start else xy_end in
        run_if_event (f st2) (fun f ->
          f (convert st2 xy_start)
            (convert st2 xy_end))
      else return () in
    let* () = on_move t (on_move_drag (fun st -> st.event_functions.event_move)) in
    let* () = on_drag t (on_move_drag (fun st -> st.event_functions.event_drag)) in
    let* () =
      on_key_pressed t (fun d ->
        let (_t, st1, st2) = get_states () in
        let* () = run_if_event st1.event_functions.event_key_pressed (fun f -> f d) in
        let* () = run_if_event st2.event_functions.event_key_pressed (fun f -> f d) in
        return ()) in
    return ()

  let make_state (x1, y1) (x2, y2) : unit m =
    let ((shift_x1, shift_y1), (shift_x2, shift_y2)) = Get.shifts  (x1, y1) (x2, y2) in
    let* t =
      I.init
        (max (shift_x1 + x1) (shift_x2 + x2))
        (max (shift_y1 + y1) (shift_y2 + y2)) in
    let setup shift_x shift_y (x, y) = {
        shift_x ;
        shift_y ;
        width = x ;
        height = y ;
        event_functions = none_event_functions
      } in
    let st1 = setup shift_x1 shift_y1 (x1, y1) in
    let st2 = setup shift_x2 shift_y2 (x2, y2) in
    let st = State (t, st1, st2) in
    global_state := st ;
    let* () = setup_on_events () in
    return ()

  (* Given the setting of [First.init] and the current state, build the new state. *)
  let set_st1 width height =
    let dim1 = (width, height) in
    match !global_state with
    | WaitingBoth -> return (global_state := WaitingSecond dim1)
    | WaitingSecond _ | State _ -> failwith "Split.set_st1: setting First.init twice."
    | WaitingFirst dim2 -> make_state dim1 dim2
    | ClosedFirst _ | ClosedSecond _ | Closed -> failwith "Split.set_st1: initialising after close."

  (* Same than for [Second.init]. *)
  let set_st2 width height =
    let dim2 = (width, height) in
    match !global_state with
    | WaitingBoth -> return (global_state := WaitingFirst dim2)
    | WaitingFirst _ | State _ -> failwith "Split.set_st2: setting Second.init twice."
    | WaitingSecond dim1 -> make_state dim1 dim2
    | ClosedFirst _ | ClosedSecond _ | Closed -> failwith "Split.set_st2: initialising after close."

  (* Call a function [f] taking as arguments:
    - The global state [t],
    - The [half_state] corresponding to the provided projection ([fst] for [First] and [snd] for [Second]).
    - Other arguments, returning within the monad [m]. *)
  let call proj f () =
    match !global_state with
    | State (t, st1, st2) -> f t (proj (st1, st2))
    | WaitingFirst _ | WaitingSecond _ | WaitingBoth -> failwith "Split.call: waiting for an I.init."
    | ClosedFirst _ | ClosedSecond _ | Closed -> failwith "Split.call: the interface has been closed."

  module Make (P : sig val proj : 'a * 'a -> 'a end) = struct
    open P

    type t = unit

    type 'a m = 'a I.m
    let return = return
    let ( let* ) = ( let* )
    let wait () = wait
    let run = run

    let init = proj (set_st1, set_st2)

    let dimensions1 () =
      match !global_state with
      | State (_t, st1, _st2) -> return (st1.width, st1.height)
      | WaitingSecond dim1 -> return dim1
      | _ -> failwith "Split.dimensions1: not ready"

    let dimensions2 () =
      match !global_state with
      | State (_t, _st1, st2) -> return (st2.width, st2.height)
      | WaitingFirst dim2 -> return dim2
      | _ -> failwith "Split.dimensions2: not ready"

    let dimensions = proj (dimensions1, dimensions2)

    let run_quit st =
      match st.event_functions.event_quit with
      | None -> return ()
      | Some f -> f ()

    let quit1 () =
      match !global_state with
      | State (_t, st1, st2) ->
        let* () = run_quit st1 in
        return (global_state := ClosedFirst st2)
      | ClosedSecond st1 ->
        let* () = run_quit st1 in
        return (global_state := Closed)
      | WaitingBoth | WaitingFirst _ | WaitingSecond _ ->
          failwith "Split.quit1: calling First.quit before the initialisation finished."
      | ClosedFirst _ | Closed -> failwith "Split.quit1: Already closed."

    let quit2 () =
      match !global_state with
      | State (_t, st1, st2) ->
        let* () = run_quit st2 in
        return (global_state := ClosedSecond st1)
      | ClosedFirst st2 ->
        let* () = run_quit st2 in
        return (global_state := Closed)
      | WaitingBoth | WaitingFirst _ | WaitingSecond _ ->
        failwith "Split.quit2: calling First.quit before the initialisation finished."
      | ClosedSecond _ | Closed -> failwith "Split.quit2: Already closed."

    let quit = proj (quit1, quit2)

    let shift f x =
      let (_t, st1, st2) = get_states () in
      proj (f st1, f st2) x

    let is_in = shift is_in
    let convert_coord = shift convert
    let convert_coord_inv = shift convert_inv

    let write =
      call proj (fun t _st rgb xy ->
        if is_in xy then I.write t rgb (convert_coord_inv xy)
        else return  ())

    let flush = call proj (fun t _st -> I.flush t)

    let on_event update () handler =
      match !global_state with
      | State (t, st1, st2) ->
        let update_st st =
          { st with event_functions = update st.event_functions (Some handler) } in
        let (st1, st2) =
          proj ((update_st st1, st2), (st1, update_st st2)) in
        return (global_state := State (t, st1, st2))
      | WaitingFirst _ | WaitingSecond _ | WaitingBoth ->
        failwith "Split.on_event: waiting for an I.init function."
      | ClosedFirst _ | ClosedSecond _ | Closed ->
        failwith "Split.on_event: the interface has been closed."

    let on_click = on_event (fun st handler -> { st with event_click = handler })
    let on_move = on_event (fun st handler -> { st with event_move = handler })
    let on_drag = on_event (fun st handler -> { st with event_drag = handler })
    let on_key_pressed = on_event (fun st handler -> { st with event_key_pressed = handler })
    let on_quit = on_event (fun st handler -> { st with event_quit = handler })

  end

end

module SplitVertical (I : Interface.T) = struct

  module B = struct
    open I

    let shifts (x_up, y_up) (x_down, y_down) =
      let dim_x = max x_up x_down in
      let shift_x x = (dim_x - x) / 2 in
      ((shift_x x_up, 0), (shift_x x_down, y_up))
  end
  module S = Split (I) (B)
  include S

  module Up = Make (struct let proj = fst end)
  module Down = Make (struct let proj = snd end)

end

module SplitHorizontal (I : Interface.T) = struct

  module B = struct
    open I

    let shifts (x_left, y_left) (x_right, y_right) =
      let dim_y = max y_left y_right in
      let shift_y y = (dim_y - y) / 2 in
      ((0, shift_y y_left), (x_left, shift_y y_right))
  end
  module S = Split (I) (B)
  include S

  module Left = Make (struct let proj = fst end)
  module Right = Make (struct let proj = snd end)

end


type 'unit button_actions = {
  on_press : bool -> 'unit ;
  on_release : bool -> 'unit ;
  set_toggle : (bool -> 'unit) -> 'unit
}

module type ButtonInputs = sig
  type _ m
  val width : int
  val height : int
  val buttons : (Subimage.t * unit m button_actions) list m
end

module Buttons (I : Interface.T) (B : ButtonInputs with type 'a m = 'a I.m) = struct

  open I

  module Ops = Ops (I)
  open Ops

  let u =
    let* buttons = B.buttons in

    (* (Maximal) dimension of buttons. *)
    let (dimx, dimy) =
        List.fold_left (fun (dimx, dimy) (img, _actions) ->
          let (dimx', dimy') = Subimage.dimensions img in
          (max dimx dimx', max dimy dimy')) (0, 0) buttons in

    (* The interface will use a bright and a dark color for its display. *)
    let bright_color =
      List.fold_left (fun (r, g, b) (img, _actions) ->
        let (r', g', b') = Filter.brightest img in
        if r + g + b < r' + g' + b' then (r', g', b') else (r, g, b)) (0, 0, 0) buttons in
    let dark_color =
      List.fold_left (fun (r, g, b) (img, _actions) ->
        let (r', g', b') = Filter.darkest img in
        if r + g + b > r' + g' + b' then (r', g', b') else (r, g, b)) (255, 255, 255) buttons in

    (* We then resize each image to have all of then the same dimensions.
     We also convert the list of images into an array. *)
    let buttons_img =
      Array.of_list (List.map (fun (img, _actions) ->
          let img = Subimage.enlarge img (dimx, dimy) in
          let img_invert =
            Subimage.map (fun (r, g, b, a) ->
              let c = (r, g, b) in
              let return (r, g, b) = (r, g, b, a) in
              if c = bright_color then return dark_color
              else if c = dark_color then return bright_color
              else return c) img in
          (img, img_invert)) buttons) in

    let nb_buttons = Array.length buttons_img in

    (* We now compute the disposition of buttons, and in particular how many
      buttons should be placed per line. *)
    let (button_per_lines, nb_lines) =
      let aspect_ratio (width, height) = Float.of_int width /. Float.of_int height in
      let target_aspect_ratio = aspect_ratio (B.width, B.height) in
      assert (nb_buttons >= 1) ;
      let nb_line button_per_lines =
        (nb_buttons / button_per_lines)
        + if nb_buttons mod button_per_lines = 0 then 0 else 1 in
      (* Given a configuration, the minimal dimension of the interface. *)
      let minimal_dimensions button_per_lines =
        (* We draw lines between each buttons, hence the +1s. *)
        let width = button_per_lines * (1 + dimx) + 1 in
        let height = nb_line button_per_lines * (1 + dimy) + 1 in
        (width, height) in
      (* Whether a configuration can indeed be drawn within the constraints. *)
      let valid button_per_lines =
        let (w, h) = minimal_dimensions button_per_lines in
        w <= B.width && h <= B.height in
      let better_than nb1 nb2 =
        match valid nb1, valid nb2 with
        | false, false -> assert false
        | true, false -> true
        | false, true -> false
        | true, true ->
          let a1 = aspect_ratio (minimal_dimensions nb1) in
          let a2 = aspect_ratio (minimal_dimensions nb2) in
          Float.abs (a1 -. target_aspect_ratio) <= Float.abs (a2 -. target_aspect_ratio) in
      let rec aux nb_min nb_max =
        if nb_min + 1 <= nb_max then (
          if better_than nb_min nb_max then nb_min else nb_max
        ) else (
          let middle = (nb_max + nb_max) / 2 in
          if better_than middle (middle + 1) then
            aux nb_min middle
          else aux (middle + 1) nb_max
        ) in
      let nb = aux 1 nb_buttons in
      (nb, nb_line nb) in

    let nb_buttons_on_line y =
      if y = nb_lines - 1 then (
        let m = nb_buttons mod button_per_lines in
        if m = 0 then button_per_lines
        else m
      ) else button_per_lines in

    (* The button statuses: true for pressed. *)
    let state =
      Array.of_list (List.map (fun (_img, actions) ->
        (ref false, actions.on_press, actions.on_release)) buttons) in

    let interface =
      let* interface = init B.width B.height in
      (* Drawing lines and buttons. *)
      let* () = fill interface dark_color (1, 1) (B.width - 2, B.height - 2) in
      let* () =
        for_ 0 (nb_lines - 1) (fun y ->
          let pixel_at_y y = (B.height * y) / nb_lines in
          let py_begin = pixel_at_y y in
          let py_end = pixel_at_y (y + 1) - 1 in
          horizontal_line interface bright_color ~y:py_begin 0 (B.width - 1) %%
          let nb_buttons_on_this_line = nb_buttons_on_line y in
          for_ 0 (nb_buttons_on_this_line - 1) (fun x ->
            let b =
              let i = y * button_per_lines + x in
              fst buttons_img.(i) in
            let pixel_at_x x = (B.width * x) / nb_buttons_on_this_line in
            let px_begin = pixel_at_x x in
            let px_end = pixel_at_x (x + 1) - 1 in
            vertical_line interface bright_color ~x:px_begin (py_begin + 1) (py_end - 1) %%
            display_image interface b
              ((px_end - px_begin) / 2 - dimx / 2,
               (py_end - py_begin) / 2 - dimy / 2))) %%
        horizontal_line interface bright_color ~y:(B.height - 1) 0 (B.width - 1) in
      flush interface %%
      return interface in

    let toggle i b =
      let (status, on_press, on_release) = state.(i) in
      if !status <> b then (
        status := b ;
        (* Redrawing the corresponding button *)
        let y = i / button_per_lines in
        let x = i mod button_per_lines in
        let nb_buttons_on_this_line = nb_buttons_on_line y in
        let pixel_at_y y = (B.height * y) / nb_lines in
        let py_begin = pixel_at_y y in
        let py_end = pixel_at_y (y + 1) - 1 in
        let pixel_at_x x = (B.width * x) / nb_buttons_on_this_line in
        let px_begin = pixel_at_x x in
        let px_end = pixel_at_x (x + 1) - 1 in
        let* interface in
        let* () =
          fill interface (if b then bright_color else dark_color)
            (px_begin + 1, py_begin + 1) (px_end - 1, py_end - 1) %%
          display_image interface
            ((if b then fst else snd) buttons_img.(i))
            ((px_end - px_begin) / 2 - dimx / 2,
             (py_end - py_begin) / 2 - dimy / 2) in
        (if b then on_press else on_release) false
      ) else return () in

    (* Associating each button to its corresponding toggling action. *)
    iteri_ (fun i (_img, actions) ->
      actions.set_toggle (toggle i)) buttons

  (* TODO: React to a click. *)

end


module SelectButtons (I : Interface.T) (B : ButtonInputs with type 'a m = 'a I.m) = struct

  module Ops = Monad.Ops (I)
  open Ops

  module B' = struct

    type 'a m = 'a I.m

    let width = B.width
    let height = B.height

    let buttons =
      let* buttons = B.buttons in
      (* The currently pressed button, as its place within B.buttons. *)
      let current = ref 0 in
      let* () =
        match buttons with
        | [] -> return () (* No buttons are provided.  I guess that's fine. *)
        | (_img, actions) :: _ -> actions.on_press false in
      let toggles = List.map (fun _ -> ref (fun _b -> assert false)) buttons in
      (* Reset all buttons except one. *)
      let set_index i =
        let rec aux i = function
          | [] -> return (assert (i < 0))
          | toggle :: l ->
            let* () = !toggle (i = 0) in
            aux (i - 1) l in
        aux i toggles in
      list_mapi_ (fun index ((img, actions), toggle) ->
        let on_press _ =
          if !current <> index then (
            current := index ;
            set_index index
          ) else return () in
        let on_release _ =
          (* On can't release a button in this setting, so we just cancel the action. *)
          !toggle true in
        (* When we receive a message to toggle a button, we apply it. *)
        let* () = actions.set_toggle (fun b -> if b then on_press false else return ()) in
        return (img, {
          on_press ;
          on_release ;
          set_toggle = (fun f -> toggle := f ; return ())
         })) (List.combine buttons toggles)

  end

  module Run = Buttons (I) (B')

  let u = Run.u

end

