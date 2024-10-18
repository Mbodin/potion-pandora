
module SplitVertical (I : Interface.T) = struct

  (* Functions to be called when the appropriate event occurs. *)
  type event_functions = {
    event_click : ((int * int) -> unit I.m) option ;
    event_move : ((int * int) -> (int * int) -> unit I.m) option ;
    event_key_pressed : (Interface.direction option -> unit I.m) option ;
    event_quit : (unit -> unit I.m) option
  }

  let none_event_functions = {
    event_click = None ;
    event_move = None ;
    event_key_pressed = None ;
    event_quit = None
  }

  (* The state of half the screen. *)
  type half_state = {
    shift_x : int (* By how much this screen is shifted right. *) ;
    shift_y : int (* By how much this screen is shifted up. *) ;
    width : int (* How many pixels are reserved after the shift. *) ;
    height : int ;
    event_functions : event_functions ref
  }

  (* A state, waiting for both [Up.init] and [Down.init] to be called. *)
  type state =
    | WaitingBoth (* None of Up.init and [Down.init] has been called. *)
    | WaitingDown of (int * int) (* Only [Up.init] has been called. *)
    | WaitingUp of (int * int) (* Only [Down.init] has been called. *)
    | State of I.t * half_state * half_state (* Both has been called and the canvas are ready. *)

  (* Execute a continuation on an event function if present. *)
  let run_if_event e k =
    match e with
    | None -> I.return ()
    | Some f -> k f

  (* Build a [State _] from both up and down dimensions. *)
  let make_state (x_up, y_up) (x_down, y_down) =
    let open I in
    let dim_x = max x_up x_down in
    let* t = init dim_x (y_up + y_down) in
    let setup shift_y (x, y) = {
      shift_x = (dim_x - x) / 2 ;
      shift_y ;
      width = x ;
      height = y ;
      event_functions = ref none_event_functions
    } in
    let st_up = setup 0 (x_up, y_up) in
    let st_down = setup y_up (x_down, y_down) in
    let* () =
      I.on_click t (fun (x, y) ->
          if y < y_up then
            run_if_event !(st_up.event_functions).event_click (fun f -> f (x, y))
          else
            run_if_event !(st_down.event_functions).event_click (fun f -> f (x, y - y_up))
        ) in
    let* () =
      I.on_move t (fun (x_start, y_start) (x_end, y_end) ->
          if y_start < y_up && y_end < y_up then
            run_if_event !(st_up.event_functions).event_move
              (fun f -> f (x_start, y_start) (x_end, y_end))
          else if y_start >= y_up && y_end >= y_up then
            run_if_event !(st_down.event_functions).event_move
              (fun f -> f (x_start, y_start - y_up) (x_end, y_end - y_up))
          else I.return ()
        ) in
    let* () =
      I.on_key_pressed t (fun d ->
        let* () = run_if_event !(st_up.event_functions).event_key_pressed (fun f -> f d) in
        let* () = run_if_event !(st_down.event_functions).event_key_pressed (fun f -> f d) in
        return ()) in
    return (State (t, st_up, st_down))

  (* Given the setting of [Up.init] and the current state, build the new state. *)
  let set_state_up (width, height) =
    let dim_up = (width, height) in function
    | WaitingBoth -> I.return (WaitingDown dim_up)
    | WaitingDown _ | State _ -> failwith "SplitVertical: setting Up.init twice."
    | WaitingUp dim_down -> make_state dim_up dim_down

  (* Same than for [Down.init]. *)
  let set_state_down (width, height) =
    let dim_down = (width, height) in function
    | WaitingBoth -> I.return (WaitingUp dim_down)
    | WaitingUp _ | State _ -> failwith "SplitVertical: setting Down.init twice."
    | WaitingDown dim_up -> make_state dim_up dim_down

  (* Call a function [f] taking as arguments:
    - The global state [I.t],
    - The [half_state] corresponding to the provided projection ([fst] for [Up] and [snd] for [Down]).
    - Other arguments, returning within the monad [I.m]. *)
  let call proj f = function
    | State (t, st_up, st_down) -> f t (proj (st_up, st_down))
    | _ -> failwith "SplitVertical: waiting for an I.init function."

  module Make (P : sig val proj : 'a * 'a -> 'a end) = struct
    open P

    type t = state

    type 'a m = 'a I.m
    let return = I.return
    let ( let* ) = I.( let* )
    let wait = I.wait

    let init = proj (set_state_up, set_state_down)

    (* We consider that as soon as either [Up.quit] or [Down.quit] is called,
      the other should follow shortly: we just consider that [Up.quit] calls
      [I.quit] whilst the other does nothing. *)
    let quit_up = function
      | State (t, st_up, st_down) ->
        let run st =
          run_if_event !(st.event_functions).event_quit (fun f -> f ()) in
        let* () = run st_up in
        let* () = run st_down in
        I.quit t
      | _ -> failwith "SplitVertical: calling Up.quit before the initialisation finished."

    let quit = proj (quit_up, fun _ -> return ())

    let write =
      call proj (fun t st rgb (x, y) ->
        if x < 0 || y < 0 || x >= st.width || y >= st.height then
          return  ()
        else I.write t rgb (x + st.shift_x, y + st.shift_y))

    let flush = call proj (fun t _st -> I.flush t)

    let on_event update =
      call proj (fun _t st handler ->
        st.event_functions := update !(st.event_functions) handler ;
        return ())


    let on_click = on_event (fun st handler -> { st with event_click = handler })
    let on_move = on_event (fun st handler -> { st with event_move = handler })
    let on_key_pressed = on_event (fun st handler -> { st with event_key_pressed = handler })
    let on_quit = on_event (fun st handler -> { st with event_quit = handler })

  end

  module Up = Make (struct let proj = fst end)
  module Down = Make (struct let proj = snd end)

end

