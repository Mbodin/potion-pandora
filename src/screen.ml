
module SplitVertical (I : Interface.T) = struct

  (* Functions to be called when the appropriate event occurs. *)
  type event_functions = {
    event_click : ((int * int) -> unit I.m) option ;
    event_move : ((int * int) -> (int * int) -> unit I.m) option ;
    event_drag : ((int * int) -> (int * int) -> unit I.m) option ;
    event_key_pressed : (Interface.direction option -> unit I.m) option ;
    event_quit : (unit -> unit I.m) option
  }

  let none_event_functions = {
    event_click = None ;
    event_move = None ;
    event_drag = None ;
    event_key_pressed = None ;
    event_quit = None
  }

  (* The state of half the screen. *)
  type half_state = {
    shift_x : int (* By how much this screen is shifted right. *) ;
    shift_y : int (* By how much this screen is shifted up. *) ;
    width : int (* How many pixels are reserved after the shift. *) ;
    height : int ;
    event_functions : event_functions
  }

  (* A state, waiting for both [Up.init] and [Down.init] to be called. *)
  type state =
    | WaitingBoth (* None of Up.init and [Down.init] has been called. *)
    | WaitingDown of (int * int) (* Only [Up.init] has been called. *)
    | WaitingUp of (int * int) (* Only [Down.init] has been called. *)
    | State of I.t * half_state * half_state (* Both has been called and the canvas are ready. *)
    | ClosedUp of half_state (* [Up.quit] has been called: we continue to store down's state. *)
    | ClosedDown of half_state (* [Down.quit] has been called: we continue to store up's state. *)
    | Closed (* Both [Up.quit] and [Down.quit] have been called. *)

  (* The shared variable between the two created interfaces. *)
  let global_state = ref WaitingBoth

  type 'a m = 'a I.m

  let return = I.return
  let ( let* ) = I.( let* )

  let run = I.run

  let get_states () =
    match !global_state with
    | State (t, st_up, st_down) -> (t, st_up, st_down)
    | _ -> failwith "SplitVertical.get_states: not ready."

  let wait (type t) (time : int) (f : unit -> t m) : t m =
    let (t, _st_up, _st_down) = get_states () in
    I.wait t time f

  (* Check if a global coordinate is within the upper interface. *)
  let is_up (x, y) =
    let (_t, st_up, _st_down) = get_states () in
    y >= 0 && y < st_up.height
    && x >= st_up.shift_x && x - st_up.shift_x < st_up.width

  (* Check if a global coordinate is within the down interface. *)
  let is_down (x, y) =
    let (_t, st_up, st_down) = get_states () in
    y >= st_up.height && y < st_up.height + st_down.height
    && x >= st_down.shift_x && x - st_down.shift_x < st_down.width

  (* Convert a global coordinate into the local up one. *)
  let convert_up (x, y) =
    let (_t, st_up, _st_down) = get_states () in
    (x - st_up.shift_x, y)

  (* Convert a global coordinate into the local down one. *)
  let convert_down (x, y) =
    let (_t, st_up, st_down) = get_states () in
    (x - st_down.shift_x, y - st_up.height)

  (* Convert a local up coordinate into the global one. *)
  let convert_up_inv (x, y) =
    let (_t, st_up, _st_down) = get_states () in
    assert (x >= 0 && y >= 0 && x < st_up.width && y < st_up.height) ;
    (x + st_up.shift_x, y)

  (* Convert a local down coordinate into the global one. *)
  let convert_down_inv (x, y) =
    let (_t, st_up, st_down) = get_states () in
    assert (x >= 0 && y >= 0 && x < st_down.width && y < st_down.height) ;
    (x + st_down.shift_x, y + st_up.height)

  (* Set up the [I.on_*] functions to call the relevant function stored in the state. *)
  let setup_on_events () : unit m =
    let open I in
    let run_if_event e k =
      match e with
      | None -> return ()
      | Some f -> k f in
    let (t, _st_up, _st_down) = get_states () in
    let* () =
      on_click t (fun xy ->
        let (_t, st_up, st_down) = get_states () in
        if is_up xy then
          run_if_event st_up.event_functions.event_click
            (fun f -> f (convert_up xy))
        else if is_down xy then
          run_if_event st_down.event_functions.event_click
            (fun f -> f (convert_down xy))
        else return ()) in
    let on_move_drag f xy_start xy_end =
      let (_t, st_up, st_down) = get_states () in
      if is_up xy_end then
        let xy_start =
          if is_up xy_start then xy_start
          else xy_end in
        run_if_event (f st_up)
          (fun f ->
            f (convert_up xy_start)
              (convert_up xy_end))
      else if is_down xy_end then
        let xy_start =
          if is_down xy_start then xy_start
          else xy_end in
        run_if_event (f st_down)
            (fun f ->
              f (convert_down xy_start)
                (convert_down xy_end))
      else return () in
    let* () = on_move t (on_move_drag (fun st -> st.event_functions.event_move)) in
    let* () = on_drag t (on_move_drag (fun st -> st.event_functions.event_drag)) in
    let* () =
      on_key_pressed t (fun d ->
        let (_t, st_up, st_down) = get_states () in
        let* () = run_if_event st_up.event_functions.event_key_pressed (fun f -> f d) in
        let* () = run_if_event st_down.event_functions.event_key_pressed (fun f -> f d) in
        return ()) in
    return ()

  (* Build a [State _] from both up and down dimensions and store it into [global_state]. *)
  let make_state (x_up, y_up) (x_down, y_down) : unit m =
    let dim_x = max x_up x_down in
    let* t = I.init dim_x (y_up + y_down) in
    let setup shift_y (x, y) = {
      shift_x = (dim_x - x) / 2 ;
      shift_y ;
      width = x ;
      height = y ;
      event_functions = none_event_functions
    } in
    let st_up = setup 0 (x_up, y_up) in
    let st_down = setup y_up (x_down, y_down) in
    let st = State (t, st_up, st_down) in
    global_state := st ;
    let* () = setup_on_events () in
    return ()

  (* Given the setting of [Up.init] and the current state, build the new state. *)
  let set_state_up width height =
    let dim_up = (width, height) in
    match !global_state with
    | WaitingBoth -> return (global_state := WaitingDown dim_up)
    | WaitingDown _ | State _ -> failwith "SplitVertical.set_state_up: setting Up.init twice."
    | WaitingUp dim_down -> make_state dim_up dim_down
    | ClosedUp _ | ClosedDown _ | Closed -> failwith "SplitVertical.set_state_up: initialising after close."

  (* Same than for [Down.init]. *)
  let set_state_down width height =
    let dim_down = (width, height) in
    match !global_state with
    | WaitingBoth -> return (global_state := WaitingUp dim_down)
    | WaitingUp _ | State _ -> failwith "SplitVertical.set_state_down: setting Down.init twice."
    | WaitingDown dim_up -> make_state dim_up dim_down
    | ClosedUp _ | ClosedDown _ | Closed -> failwith "SplitVertical.set_state_down: initialising after close."

  (* Call a function [f] taking as arguments:
    - The global state [t],
    - The [half_state] corresponding to the provided projection ([fst] for [Up] and [snd] for [Down]).
    - Other arguments, returning within the monad [m]. *)
  let call proj f () =
    match !global_state with
    | State (t, st_up, st_down) -> f t (proj (st_up, st_down))
    | WaitingUp _ | WaitingDown _ | WaitingBoth -> failwith "SplitVertical.call: waiting for an I.init function."
    | ClosedUp _ | ClosedDown _ | Closed -> failwith "SplitVertical.call: the interface has been closed."

  module Make (P : sig val proj : 'a * 'a -> 'a end) = struct
    open P

    type t = unit

    type 'a m = 'a I.m
    let return = return
    let ( let* ) = ( let* )
    let wait () = wait
    let run = run

    let init = proj (set_state_up, set_state_down)

    let run_quit st =
      match st.event_functions.event_quit with
      | None -> return ()
      | Some f -> f ()

    let quit_up () =
      match !global_state with
      | State (_t, st_up, st_down) ->
        let* () = run_quit st_up in
        return (global_state := ClosedUp st_down)
      | ClosedDown st_up ->
        let* () = run_quit st_up in
        return (global_state := Closed)
      | WaitingBoth | WaitingUp _ | WaitingDown _ -> failwith "SplitVertical.quit_up: calling Up.quit before the initialisation finished."
      | ClosedUp _ | Closed -> failwith "SplitVertical.quit_up: Already closed."

    let quit_down () =
      match !global_state with
      | State (_t, st_up, st_down) ->
        let* () = run_quit st_down in
        return (global_state := ClosedUp st_up)
      | ClosedUp st_down ->
        let* () = run_quit st_down in
        return (global_state := Closed)
      | WaitingBoth | WaitingUp _ | WaitingDown _ -> failwith "SplitVertical.quit_down: calling Up.quit before the initialisation finished."
      | ClosedDown _ | Closed -> failwith "SplitVertical.quit_down: Already closed."

    let quit = proj (quit_up, quit_down)

    let is_in = proj (is_up, is_down)
    let convert_coord = proj (convert_up, convert_down)
    let convert_coord_inv = proj (convert_up_inv, convert_down_inv)

    let write =
      call proj (fun t st rgb xy ->
        if is_in xy then I.write t rgb (convert_coord_inv xy)
        else return  ())

    let flush = call proj (fun t _st -> I.flush t)

    let on_event update () handler =
      match !global_state with
      | State (t, st_up, st_down) ->
        let update_st st =
          { st with event_functions = update st.event_functions (Some handler) } in
        let (st_up, st_down) =
          proj ((update_st st_up, st_down), (st_up, update_st st_down)) in
        return (global_state := State (t, st_up, st_down))
      | WaitingUp _ | WaitingDown _ | WaitingBoth -> failwith "SplitVertical.on_event: waiting for an I.init function."
      | ClosedUp _ | ClosedDown _ | Closed -> failwith "SplitVertical.on_event: the interface has been closed."

    let on_click = on_event (fun st handler -> { st with event_click = handler })
    let on_move = on_event (fun st handler -> { st with event_move = handler })
    let on_drag = on_event (fun st handler -> { st with event_drag = handler })
    let on_key_pressed = on_event (fun st handler -> { st with event_key_pressed = handler })
    let on_quit = on_event (fun st handler -> { st with event_quit = handler })

  end

  module Up = Make (struct let proj = fst end)
  module Down = Make (struct let proj = snd end)

end

