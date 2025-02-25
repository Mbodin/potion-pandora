module IMap = Map.Make (Integer)
module ISet = Set.Make (Integer)
module EMap = Map.Make (Event)
module ESet = Set.Make (Event)

(* Number of frames per second. *)
let frames_per_second = 30

(* A type to count time, as a number of cycles.
  The type is abstracted away to avoid mixing with the state. *)
module Time : sig

  type t
  val zero : t
  val infinity : t
  val incr : t -> t
  val (<) : t -> t -> bool

  (* This converts a time in seconds into a time in number of frames. *)
  val of_seconds : float -> t

  (* For debug purposes: print the number of frames. *)
  val print : t -> string

end = struct

  type t = int
  let zero = 0
  let infinity = max_int
  let incr t =
    if t = max_int then max_int
    else t + 1
  let (<) = (<)

  let of_seconds s =
    let f = Float.round (s *. Float.of_int frames_per_second) in
    if f >= Float.of_int infinity then infinity
    else Float.to_int f

  let print = string_of_int

end
type time = Time.t

(* The representation of an object state. *)
type state = int

(* The internal automaton of an object.
  Each state is an array index.
  It then stores the associated image, as well as the next step for each event.
  The next state is itself associated *)
type automaton = (Subimage.t * (time * state) Event.map) array

type t = {
  state : state (* The current state of the automaton. *) ;
  time : time (* The number of cycles that were spent on this state without taking any transition. *) ;
  delta : automaton (* How this state responds to events. *)
}

let image t =
  let (image, _next) = t.delta.(t.state) in
  image

let check_size t =
  let dim = Subimage.dimensions (image t) in
  Array.for_all (fun (image, _next) ->
    dim = Subimage.dimensions image) t.delta

let send t e =
  let (_image, next) = t.delta.(t.state) in
  let (time, st) = Event.fetch next e in
  if e = Tau then
    if Time.(t.time < time) then
      { t with time = Time.incr t.time }
    else { t with state = st ; time = Time.zero }
  else
    if Time.(t.time < time) then t
    else { t with state = st }

let listen t e =
  let (_image, next) = t.delta.(t.state) in
  let (time, st) = Event.fetch next e in
  (st <> t.state && not Time.(t.time < time))

let print ?(quiet = true) ?event t =
  let module ImageMap = Map.Make (struct type t = Subimage.t let compare = compare end) in
  let name_of_state =
    let images = ref ImageMap.empty in
    fun st ->
      let (image, _next) = t.delta.(st) in
      let nimage =
        match ImageMap.find_opt image !images with
        | Some n -> n
        | None ->
          let n =
            let rec aux n = function
              | [] -> '_'
              | (c1, c2) :: l ->
                let c1 = Char.code c1 in
                let c2 = Char.code c2 in
                assert (c1 <= c2) ;
                if n <= c2 - c1 then Char.chr (c1 + n)
                else aux (n - (c2 - c1)) l in
            aux (ImageMap.cardinal !images) [('A', 'Z'); ('a', 'z'); ('0', '9')] in
          images := ImageMap.add image n !images ;
          n in
      Printf.sprintf "%c%i" nimage st in
  let aux st =
    let current = name_of_state st in
    let (_image, next) = t.delta.(st) in
    let l =
      List.map (fun e ->
          let (time, st') =  Event.fetch next e in
          let label =
            if quiet then None
            else Some (Printf.sprintf {|label = "%s, %s"|} (Time.print time) (Event.print e)) in
          let style =
            let l =
              (match e with
               | Event.Tau -> ["dashed"]
               | Event.(MoveLeft | MoveRight | Fall) -> ["bold"]
               | Event.(RandomRare | RandomNormal | RandomFrequent | RandomFlicker) -> ["dotted"]
               | _ -> [])
              @ (if event = Some e then ["red"] else []) in
            if l = [] then None else
              Some (Printf.sprintf {|style = "%s"|} (String.concat "," l)) in
          let l = List.filter_map (fun o -> o) [label ; style] in
          let options =
            if l = [] then ""
            else Printf.sprintf "[%s]" (String.concat ", " l) in
          let edge =
            Printf.sprintf "%s -> %s %s\n" current (name_of_state st') options in
          if quiet && st = st' && e <> Event.Tau then "" else edge)
        Event.all in
    String.concat "" l in
  Printf.sprintf "digraph {\n%s}\n"
    (String.concat "" (List.init (Array.length t.delta) aux))

type sequence = (Subimage.t * float) list

let loop s =
  let len = List.length s in
  assert (len > 0) ;
  let automaton =
    Array.of_list (List.mapi (fun index (image, time) ->
      (image, Event.create_map (function
        | Event.Tau -> (Time.of_seconds time, (index + 1) mod len)
        | _ -> (Time.infinity, index)))) s) in {
    delta = automaton ;
    time = Time.zero ;
    state = 0
  }

let static i = loop [(i, infinity)]

(* Create an automaton part encoding a sequence [s].
  The shift represents the first state of the sequence (it is meant to be concatenated at
  the end of the automaton and thus starts its indexes there).
  The set [restart] is a set of event that restart the sequence.
  [state] represents the state targeted after the sequence ended.
  The last argument is to tune a skipping behavior: if it is [None], then this sequence won't
  skip on any event.
  If it is [Some t.delta.(st)] (typically with [st = state]), then it will mimick the provided
  state: if this state immediately reacts on an event, so will the sequence. The [Tau] avent will
  be ignored when this behaviour is enabled. *)
let create_sequence shift restart s state skip =
  let len = List.length s in
  Array.of_list (List.mapi (fun index (image, time) ->
    let self = shift + index in
    let next =
      if index = len - 1 then state
      else (self + 1) in
    (image, Event.create_map (function
      | Event.Tau -> (Time.of_seconds time, next)
      | e ->
        if ESet.mem e restart then (Time.zero, shift)
        else
          (* Reaction of the state after the sequence to this event. *)
          let reaction =
            match skip with
            | None -> None
            | Some (_image, action) ->
              let (time, state') = Event.fetch action e in
              if time = Time.zero && state' <> state then
                (* In this case, [state] would directly jump into [state'] without waiting:
                  as [skip] is true, we must jump there too. *)
                Some state'
              else None in
          match reaction with
          | None -> (Time.infinity, self)
          | Some next -> (Time.zero, next)))) s)

let react t es ?(skip = false) ?(restart = false) s =
  let len = List.length s in
  if len = 0 then t
  else
    let es = ESet.of_list es in
    let shift = Array.length t.delta in
    let sequence =
      let restart = if restart then es else ESet.empty in
      create_sequence shift restart s t.state
        (if skip then Some t.delta.(t.state) else None) in
    let automaton =
      Array.append
        (Array.map (fun (image, next) ->
          (image, Event.create_map (fun e ->
            if ESet.mem e es then (Time.zero, shift)
            else Event.fetch next e))) t.delta)
        sequence in
    { t with delta = automaton }

let change_with t1 es ?(skip = false) s t2 =
  let es = ESet.of_list es in
  (* There are three portions of states in the new automaton:
      - The states of t1.
      - The states of t2 (shifted by [Array.length t1.delta]),
      - The states to encode the sequence s. *)
  let shift = Array.length t1.delta in
  let shift_s = shift + Array.length t2.delta in
  let delta1 =
    Array.map (fun (image, next) ->
      (image, Event.create_map (fun e ->
        if ESet.mem e es then
          if s = [] then (Time.zero, t2.state + shift)
          else (Time.zero, shift_s)
        else Event.fetch next e))) t1.delta in
  let delta2 =
    Array.map (fun (image, f) ->
      (image, Event.create_map (fun e ->
        let (time, state) = Event.fetch f e in
        (time, state + shift)))) t2.delta in
  let delta_s =
    create_sequence shift_s ESet.empty s (t2.state + shift)
      (if skip then Some delta2.(t2.state) else None) in
  let automaton =
    Array.concat [
      delta1 ;
      delta2 ;
      delta_s
    ] in
  { t1 with delta = automaton }

let prefix s t =
  change_with t Event.all s t

let switch t1 es1 ?(skip = false) s1 =
  let skip1 = skip in fun t2 es2 ?(skip = false) s2 ->
  let skip2 = skip in
  let es1 = ESet.of_list es1 in
  let es2 = ESet.of_list es2 in
  (* There are four portions of states in the new automaton:
      - The states of t1,
      - The states of t2 (shifted by [Array.length t1.delta]),
      - The states to encode the sequence s1,
      - The states to encore the sequence s2. *)
  let shift2 = Array.length t1.delta in
  let shift_s1 = shift2 + Array.length t2.delta in
  let shift_s2 = shift_s1 + List.length s1 in
  let delta1 =
    Array.map (fun (image, f) ->
      (image, Event.create_map (fun e ->
        if ESet.mem e es1 then
          if s1 = [] then (Time.zero, t2.state + shift2)
          else (Time.zero, shift_s1)
        else Event.fetch f e))) t1.delta in
  let delta2 =
    Array.map (fun (image, f) ->
      (image, Event.create_map (fun e ->
        if ESet.mem e es2 then
          if s2 = [] then (Time.zero, t1.state)
          else (Time.zero, shift_s2)
        else
          let (time, state) = Event.fetch f e in
          (time, state + shift2)))) t2.delta in
  let delta_s1 =
    create_sequence shift_s1 ESet.empty s1 (t2.state + shift2)
      (if skip1 then Some delta2.(t2.state) else None) in
  let delta_s2 =
    create_sequence shift_s2 ESet.empty s2 t1.state
      (if skip2 then Some t1.delta.(t1.state) else None) in
  let automaton =
    Array.concat [
      delta1 ;
      delta2 ;
      delta_s1 ;
      delta_s2
    ] in
  { t1 with delta = automaton }

let force_same_size t =
  let size =
    Array.fold_left (fun (sizex, sizey) (image, _next) ->
      let (width, height) = Subimage.dimensions image in
      (max sizex width, max sizey height)) (0, 0) t.delta in
  let delta =
    Array.map (fun (image, next) ->
      let image =
        if Subimage.dimensions image = size then image
        else (
          let (width, height) = Subimage.dimensions image in
          assert (width <= fst size && height <= snd size) ;
          Subimage.enlarge image size
        ) in
      (image, next)) t.delta in
  { t with delta }

let transitions (type t0) (init : t0) next =
  let module MMap = Map.Make (struct type t = t0 let compare = compare end) in
  (* This is meant to be rewritten once the state will be implemented. *)
  let dummy_state = -1 in
  (* The automaton is built step by step, for each encountered state.
    Along the process, we carry the following:
      - The current automaton array,
      - A map storing for each visited state its corresponding position in this automaton array,
      - A map storing the to-be-visited states, carrying a list of position (state and event) to
        update once its value will have been assigned. *)
  let rec aux delta visited to_be_visited =
    match MMap.choose_opt to_be_visited with
    | None -> delta
    | Some (st, to_update) ->
      (* FIXME: This is only executed once with the duck, instead of at least 5. *)
      let shift = Array.length delta in
      (* We know that we will place the current state at the end of the the current array.
        As its associated sequence has at least one element, it will indeed be assigned a
        place within the automaton. *)
      let visited = MMap.add st shift visited in
      (* Now that we have allocated a location for this state, we can update all the locations
        that depended on it. *)
      let to_update = List.sort_uniq compare to_update in
      List.iter (fun (st, e) ->
        assert (st < Array.length delta) ;
        let (image, event_map) = delta.(st) in
        let event_map =
          Event.create_map (fun e' ->
            let (time, st') = Event.fetch event_map e' in
            if e = e' then (
              (*if not (st' = dummy_state) then print_endline (Printf.sprintf "DEBUG: %i, %s" st' (Event.print e)) ;*) (* FIXME *)
              (time, shift)
            ) else (time, st')) in
        delta.(st) <- (image, event_map)) to_update ;
      (* Add a list of values into the associated location [to_update] in [to_be_visited]. *)
      let add_to_be_visited to_be_visited st position e =
        if MMap.mem st visited then to_be_visited
        else (
          let l =
            match MMap.find_opt st to_be_visited with
            | Some l -> l
            | None -> [] in
          MMap.add st ((position, e) :: l) to_be_visited
        ) in
      let (s, nxt) = next st in
      let len = List.length s in
      assert (len > 0) (* We need to be able to display at least one image. *) ;
      (* We build all the sequences corresponding to all the calls to each event. *)
      let (event_map, to_be_visited, new_shift, deltas) =
        List.fold_left (fun (event_map, to_be_visited, current_shift, deltas) e ->
            let (s, st') = nxt e in
            (* Here, [s] is the transition sequence between the state [st] and [st'].
              If it is non-empty, then a sequence is allocated and we point to this
              sequence. Otherwise we directly point towards [st']. *)
            let index' =
              match MMap.find_opt st' visited with
              | Some index -> index
              | None -> dummy_state in
            let sequence = create_sequence current_shift ESet.empty s index' None in
            let to_be_visited =
              (* Adding the end of the sequence (if any) as a pointer to [st'].
                If [s] is the empty list, then [st'] is directly pointed from [st]. *)
              if s <> [] then (
                if not (MMap.mem st' visited) then
                  assert (
                    let (_image, event_map) = sequence.(Array.length sequence - 1) in
                    let (_time, st) = Event.fetch event_map e in
                    st = dummy_state) ;
                add_to_be_visited to_be_visited st'
                  (current_shift + Array.length sequence - 1) Event.Tau
              ) else to_be_visited in
            let event_map = EMap.add e (if s = [] then index' else current_shift) event_map in
            let to_be_visited =
              (* Updating the states of [st] that directly point to [st']. *)
              if s = [] then (
                if e = Event.Tau then (
                  (* Only the last state of the main sequence would jump into this state. *)
                  add_to_be_visited to_be_visited st' (shift + len - 1) Event.Tau
                ) else (
                  (* All the states of the main looping sequence would jump into this state. *)
                  List.fold_left (fun to_be_visited position ->
                    add_to_be_visited to_be_visited st' position e)
                      to_be_visited (List.init len (fun i -> i + shift))
                )
              ) else to_be_visited in
            (event_map, to_be_visited, current_shift + Array.length sequence, sequence :: deltas))
          (EMap.empty, to_be_visited, shift + len, []) Event.all in
      let automaton_cell =
        (fst (List.hd s), Event.create_map (fun e ->
            match EMap.find_opt e event_map with
            | None -> assert false
            | Some state -> (Time.zero, state))) in
      (* We finally build the main looping sequence. *)
      let sequence =
        let index_tau =
          match EMap.find_opt Event.Tau event_map with
          | None -> assert false
          | Some index -> index in
        create_sequence shift ESet.empty s index_tau (Some automaton_cell) in
      let delta = Array.concat (delta :: sequence :: List.rev deltas) in
      assert (Array.length delta = new_shift) ;
      aux delta visited (MMap.remove st to_be_visited) in
  let delta = aux [||] MMap.empty (MMap.singleton init []) in
  assert (Array.for_all (fun (_image, event_map) ->
    List.for_all (fun e ->
      let (_time, st) = Event.fetch event_map e in
      st >= 0 && st < Array.length delta) Event.all) delta) ;
  {
    delta ;
    time = Time.zero ;
    state = 0 (* The first picked-up state is necessarily the first. *)
  }

(* Greatest common divisor. *)
let rec gcd a b =
  if b <> 0 then gcd b (a mod b)
  else abs a

let nd_transitions (type t0) (init : t0) next =
  let module MO = struct type t = t0 let compare = compare end in
  let module MMap = Map.Make (MO) in
  let module MSet = Set.Make (MO) in
  (* First, we cache all the results of [next] to know the size of each state,
    as these will need to be duplicated for each call. *)
  let cache =
    let rec aux to_be_visited m =
      match MSet.choose_opt to_be_visited with
      | None -> m
      | Some st ->
        let to_be_visited = MSet.remove st to_be_visited in
        if MMap.mem st m then aux to_be_visited m
        else (
          let (s, nxt) = next st in
          assert (s <> []) (* Each state must be associated with at least one image. *) ;
          let to_be_visited =
            List.fold_left (fun to_be_visited e ->
              List.fold_left (fun to_be_visited (_n, _s, st') -> MSet.add st' to_be_visited)
                to_be_visited (nxt e)) to_be_visited Event.all in
          let event_map =
            Event.create_map (fun e ->
              let l = nxt e in
              assert (List.for_all (fun (n, _s, _st) -> n > 0) l) ;
              l) in
          (* The current state will be divided in [size] substates to account for randomness. *)
          let size =
            List.fold_left (fun n e ->
              let l = Event.fetch event_map e in
              assert (l <> []) ;
              (* The current state needs to have a least [sum] substates to make sense. *)
              let sum = List.fold_left (fun n (n', _s, _st) -> n + n') 0 l in
              (* We compute the least common multiple of all these. *)
              (n * sum) / gcd n sum) 1 Event.all in
          assert (size > 0) ;
          aux to_be_visited (MMap.add st (size, s, event_map) m)
        ) in
    aux (MSet.singleton init) MMap.empty in
  (* We now use the cache instead of the original function, to avoid any nasty side-effects. *)
  let next st =
    match MMap.find_opt st cache with
    | None -> assert false
    | Some (size, s, event_map) -> (size, s, Event.fetch event_map) in
  (* The idea here is do duplicate a state as many times as its size.
    We thus use [t0 * int] as a new state, the integer being its index,
    with a random index each time we want to map the old one. *)
  let new_state old_state =
    match MMap.find_opt old_state cache with
    | None -> assert false
    | Some (size, _s, _event_map) -> (old_state, Random.int size) in
  transitions (new_state init) (fun (st, index) ->
    let (size, s, nxt) = next st in
    (s, fun e ->
      let l = nxt e in
      (* We avoid calling useless randomlessness to avoid overloading the engine. *)
      let new_state =
        (* TODO: Determine which event among RandomFlicker..RandomRare is most relevant for this
          automaton. *)
        if e = Event.RandomFlicker then new_state
        else fun old_state ->
          if old_state = st then
            if e = Event.Tau then
              (* This ensures that the generated automaton is connex. *)
              (st, (index + 1) mod size)
            else
              (* Having the same state makes the engine consider that it won't listen
                to this particular event, and thus won't make the engine work for it. *)
              (st, index)
          else new_state old_state in
      let rec pick i = function
        | [] -> assert false
        | (weight, s, st') :: l ->
          let i' = i - weight in
          if i' < 0 then (s, st')
          else pick i' l in
      let sum = List.fold_left (fun sum (w, _, _) -> sum + w) 0 l in
      let (s, st') = pick (index mod sum) l in
      (s, new_state st')))

let combine tl =
  assert (List.for_all (fun (t, _dxy) -> check_size t) tl) ;
  (* The number of states in the new automaton. *)
  let num_states = List.fold_left (fun n (t, _) -> n * Array.length t.delta) 1 tl in
  (* Given in which state all the automatons in t are, return the corresponding state in the
    resulting automaton. *)
  let state_in sl =
    fst (List.fold_left2 (fun (current, size) (t, _) s ->
      let len = Array.length t.delta in
      assert (s < len) ;
      (current + s * size, size * len)) (0, 1) tl sl) in
  (* The reciprocal of state_in. *)
  let state_from i =
    let (sl, i, size) =
      List.fold_left (fun (sl, i, size) (t, _) ->
        let len = Array.length t.delta in
        assert (size mod len = 0) ;
        let size = size / len in
        let s = i / size in
        assert (s < len) ;
        let i = i mod size in
        (s :: sl, i, size)) ([], i, num_states) (List.rev tl) in
    assert (i = 0) ;
    assert (size = 1) ;
    sl in
  let delta =
    Array.init num_states (fun i ->
      let sl = state_from i in
      let image =
        (* LATER: We might want to cache the result here to avoid recreating the same image twice. *)
        Subimage.combine (List.map2 (fun (t, d) s ->
          let (image, _next) = t.delta.(s) in
          (image, d)) tl sl) in
      let fetch_event e =
        let state =
          state_in (List.map2 (fun (t, _) s ->
            let (_image, next) = t.delta.(s) in
            let (time, st) = Event.fetch next e in
            if time = Time.infinity then s
            else st) tl sl) in
        let time =
          List.fold_left2 (fun n (t, _) s ->
            let (_image, next) = t.delta.(s) in
            let (time, _st) = Event.fetch next e in
            if time = Time.infinity then n
            else max n time) Time.zero tl sl in
        (time, state) in
      (image, Event.create_map fetch_event)) in {
    state = state_in (List.map (fun (t, _) -> t.state) tl) ;
    time = List.fold_left (fun n (t, _) -> max n t.time) Time.zero tl ;
    delta
  }

let decolor ~pattern t =
  { t with delta = Array.map (fun (i, ev) -> (Filter.decolor ~pattern i, ev)) t.delta }

(* This test is here and not in Subimage to prevent a dependency cycle: it requires some
  functions to create images, and Filter is the place containing most of them. *)
let%test "Animation.force_same_size" =
  List.for_all (fun t -> check_size (force_same_size t)) [
    static Filter.(rectangle transparent (10, 12)) ;
    loop Filter.[
      (rectangle transparent (10, 12), 0.1) ;
      (rectangle transparent (1, 20), 0.2) ;
      (rectangle transparent (20, 1), 0.3)
    ] ;
  ]

