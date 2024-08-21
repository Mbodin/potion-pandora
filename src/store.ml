
module IMap = Map.Make (Integer)

(* Generation of new identifiers. *)
module Id : sig

  (* The type of identifiers *)
  type t

  (* Create a new identifier. *)
  val fresh : unit -> t

  (* The comparison function for identifiers. *)
  val compare : t -> t -> int

  (* Print the identifier, for debuging purposes. *)
  val print : t -> string

end = struct

  include Integer

  let fresh =
    let available = ref 0 in
    fun () ->
      let ret = !available in
      incr available ;
      ret

  let print = string_of_int

end

(* Bi-directional arrays. *)
module Biarray : sig

  (* Like array, but accept negative arguments. *)
  type 'a t

  (* Create a new bi-array, of initial size provided, and with default value. *)
  val init : int -> 'a -> 'a t

  (* Read a bi-array at the provided position (possibly returning the default
    value if outside the allocated range).  The position can be negative. *)
  val get : 'a t -> int -> 'a

  (* Modify in-place the array at the provided position (possibly negative) and return the array.
    If it is outside the allocated range, a new array is defined, invalidating the old one. *)
  val set : 'a t -> int -> 'a -> 'a t

  (* Minimum and maximum coordinates at which an object is actually stored. *)
  val min_arg : 'a t -> int
  val max_arg : 'a t -> int

  (* Apply a function to each elements of the biarray. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (* Call a function to each elements of the biarray. *)
  val iter : ('a -> unit) -> 'a t -> unit

end = struct

  type 'a t = {
    default : 'a ;
    positive : 'a array ;
    negative : 'a array
  }

  let init size default = {
    default ;
    positive = Array.make size default ;
    negative = Array.make size default
  }

  let get a i =
    let (t, i) =
      if i >= 0 then
        (a.positive, i)
      else (a.negative, 1 - i) in
    if i < Array.length t then t.(i)
    else a.default

  let set a i v =
    let (t, i, set) =
      if i >= 0 then
        (a.positive, i, fun t -> { a with positive = t })
      else (a.negative, 1 - i, fun t -> { a with negative = t }) in
    if i < Array.length t then (
      t.(i) <- v ;
      a
    ) else (
      let t' = Array.make (1 + max (2 * Array.length t) i) a.default in
      Array.blit t 0 t' 0 (Array.length t) ;
      t'.(i) <- v ;
      set t'
    )

  let min_arg a = 1 - Array.length a.negative
  let max_arg a = Array.length a.positive - 1

  let iter f a =
    Array.iter f a.negative ;
    Array.iter f a.positive

  let map f a = {
    default = f a.default ;
    negative = Array.map f a.negative ;
    positive = Array.map f a.positive
  }

end

let%test "biarray set-get positive" =
  let open Biarray in
  let a = init 10 None in
  let a = set a 42 (Some 18) in
  get a 42 = Some 18

let%test "biarray set-get negative" =
  let open Biarray in
  let a = init 10 None in
  let a = set a (-42) (Some 18) in
  get a (-42) = Some 18

let%test "biarray set-get both" =
  let open Biarray in
  let a = init 10 None in
  let a = set a 42 (Some 18) in
  let a = set a (-42) (Some 12) in
  get a (-42) = Some 12 && get a 42 = Some 18


(* To efficiently store objects, we group them by coordinates: two objects close by group_size
  in both x and y will be stored in the same cell. *)
module Data : sig

  (* The bi-dimensionnal array. *)
  type 'a t

  (* Init the array with a default element. *)
  val init : 'a -> 'a t

  (* Internally, groups are coordinates in the bi-array.
    To avoid mixing them with actual object coordinates, we abstract away its type. *)
  type group

  (* Convert coordinates to its corresponding group. *)
  val get_group : (int * int) -> group

  (* Given two groups, return all the groups in-between (forming a rectangle whose
    diagonal angles are the two provided groups). *)
  val groups_between : group -> group -> group list

  (* Reading the bi-dimensional array. *)
  val get : 'a t -> group -> 'a

  (* A useful combination of get_group and get. *)
  val read : 'a t -> (int * int) -> 'a

  (* Modifying the bi-dimensional array.
    This is an in-place modification, but it may have to reallocate the cells: it immediately
    invalidates its argument array. *)
  val set : 'a t -> group -> 'a -> 'a t

  (* A useful combination of get_group and set. *)
  val write : 'a t -> (int * int) -> 'a -> 'a t

  (* Often get and set are called one after the other to simply change a single value.
    This function performs this combination. *)
  val map_at : 'a t -> group -> ('a -> 'a) -> 'a t

  (* If the stored data is a list, this function adds an element in front of it. *)
  val add_to_list : 'a list t -> group -> 'a -> 'a list t

  (* Smallest and largest x-coordinate at which an object is in practice stored. *)
  val smallest_x : 'a t -> int
  val largest_x : 'a t -> int

  (* Iter through all the objects whose coordinate is within the group of the x-coordinate provided. *)
  val iter_at_x : ('a -> unit) -> 'a t -> int -> unit

end = struct

  let group_size = 16

  type group = int * int

  (* We are taking the euclidean division, so that e.g. [div (-3) 2 = -2]. *)
  let div = Integer.div

  let%test "Data.div" =
    let divg a = div a group_size in
    List.for_all (fun b -> b) [
      divg 0 = divg 1 ;
      divg group_size = divg (group_size + 1) ;
      divg group_size <> divg (group_size - 1) ;
      divg (-1) <> divg 0 ;
      divg (-group_size) <> divg group_size
    ]

  let get_group (x, y) =
    (div x group_size, div y group_size)

  let groups_between (i1, j1) (i2, j2) =
    let (i1, i2) = (min i1 i2, max i1 i2) in
    let (j1, j2) = (min j1 j2, max j1 j2) in
    let rec aux acc i j =
      let acc = (i, j) :: acc in
      if i = i2 then (
        if j = j2 then acc
        else aux acc i1 (j + 1)
      ) else aux acc (i + 1) j in
    aux [] i1 j1

  let%test "groups_between" =
    List.for_all (fun (g1, g2, n) ->
        let r = groups_between g1 g2 in
        List.mem g1 r && List.mem g2 r
        && List.length r = n) [
      ((0, 0), (0, 0), 1) ;
      ((0, 0), (0, 1), 2) ;
      ((0, 0), (1, 0), 2) ;
      ((0, 0), (0, -1), 2) ;
      ((0, 0), (-1, 0), 2) ;
      ((0, 0), (1, 1), 4)
    ]

  type 'a t = 'a Biarray.t Biarray.t

  let init default =
    Biarray.init group_size (Biarray.init 0 default)

  let get a (i, j) =
    Biarray.get (Biarray.get a i) j

  let read a coords = get a (get_group coords)

  let set a (i, j) v =
    let ax = Biarray.get a i in
    let ax = Biarray.set ax j v in
    Biarray.set a i ax

  let write a coords v = set a (get_group coords) v

  let map_at a ij f =
    set a ij (f (get a ij))

  let add_to_list a ij e =
    map_at a ij (fun l -> e :: l)

  let smallest_x a =
    let g = Biarray.min_arg a in
    g * group_size

  let largest_x a =
    let g = Biarray.max_arg a in
    g * group_size

  let iter_at_x f a x =
    let (i, _j) = get_group (x, 0) in
    let ax = Biarray.get a i in
    Biarray.iter f ax

end

(* A counter for random events: we only flip the coin once to know when the event
  will occur, then we decrement the counter each time. *)
module Randomness : sig
  type t

  (* Check whether the event is ready to be fired. *)
  val fire : t -> bool

  (* Check whether it is ticking. *)
  val ticking : t -> bool

  (* Let time pass.
    Warning: if the event was ready to be fired, it won't ever after that, unless resetted. *)
  val step : t -> t

  (* Set the counter to fire again in about the provided number. *)
  val reset : int -> t

  (* A counter that won't ever fire. *)
  val clear : t
end = struct

  type t = int

  let fire n = n = 0

  let ticking n = n >= 0

  let step = function
    | -1 -> -1
    | n -> n - 1

  let reset target = Random.int (max 2 (2 * target))

  let clear = -1

end

type randomness_counter = Randomness.t

(* We have to store for each frequency such a random counter. *)
type randomness = {
  rare : randomness_counter ;
  normal : randomness_counter ;
  frequent : randomness_counter ;
  flicker : randomness_counter
}

let clear_randomness = Randomness.{
  rare = clear ;
  normal = clear ;
  frequent = clear ;
  flicker = clear
}

(* The objects stored. *)
type obj_store = {
  id : Id.t (* A unique identifier. *) ;
  position : int * int (* Its precise position. *) ;
  level : int (* Its level (this is redundant information but it makes processing easier). *) ;
  display : Animation.t (* Its animation automaton. *) ;
  move : ((int * int) * int * bool) option (* When the object is moving, this is its target
    position, speed (in pixel per turn, always positive), and ghostness. *) ;
  random : randomness
}

(* We store a direct reference to the object within the cell list: this enables us to directly
  manipulate it whithout having to update its carrying list. *)
type obj = obj_store ref

let get_coords _store obj =
  !obj.position

let get_level _store obj =
  !obj.level

let get_display _store obj =
  !obj.display

(* A module to store data groups. *)
module GroupSet =
  Set.Make (struct
    type t = Data.group
    let compare = compare
  end)

(* A map for game coordinates. *)
module CoordMap =
  Map.Make (struct
    type t = Projection.game_coords
    let compare = compare
  end)

(* The store maps each level to an array of objects.
  Note that objects are present several times within the data: one for each
  cell in which it is present. *)
type store = {
  data : obj list Data.t IMap.t (* The actual store data, for each level. *) ;
  moving : GroupSet.t ref IMap.t (* The groups containing moving objects, for each level. *) ;
  wind : int list (* The x-coordinates where there is currently wind. *) ;
  birds : Animation.t array (* All the available birds. *) ;
  bird_locations : obj option CoordMap.t IMap.t (* For each bird standing point (level and coordinate), the bird going there, if any. *)
}

(* Return all the groups between the provided screen coordinates at this level. *)
let all_groups_within level min_coord max_coord =
  let min_coord = Projection.from_screen level min_coord in
  let max_coord = Projection.from_screen level max_coord in
  Data.groups_between (Data.get_group min_coord) (Data.get_group max_coord)

(* Given an object, return the list of all cell groups covered by its image. *)
let covered_cells obj =
  let (posx, posy) = !obj.position in
  let max_pos =
    let (dimx, dimy) = Subimage.dimensions (Animation.image !obj.display) in
    (posx + dimx - 1, posy + dimy - 1) in
  all_groups_within !obj.level (posx, posy) max_pos

type t = store ref

let create () =
  ref {
    data = IMap.empty ;
    moving = IMap.empty ;
    wind = [] ;
    birds = [||] ;
    bird_locations = IMap.empty
  }

let add_birds store birds =
  store := { !store with birds = Array.append !store.birds (Array.of_list birds) }

let add_bird_location store ?(level = 0) coord =
  let m =
    match IMap.find_opt level !store.bird_locations with
    | None -> CoordMap.empty
    | Some m -> m in
  let m = CoordMap.add coord None m in
  store := { !store with bird_locations = IMap.add level m !store.bird_locations }

(* Force the creation of the provided level, if not present. *)
let create_level store level =
  let data =
    match IMap.find_opt level store.data with
    | Some _a -> store.data
    | None -> IMap.add level (Data.init []) store.data in
  let moving =
    match IMap.find_opt level store.moving with
    | Some _l -> store.moving
    | None -> IMap.add level (ref GroupSet.empty) store.moving in
  { store with data ; moving }

(* Adding the provided coordinate as one containing moving objects, or at least
  where something changed recently. *)
let add_moving_at store level coords =
  let m =
    match IMap.find_opt level !store.moving with
    | Some m -> m
    | None ->
      (* As the object has been created somehow, its level must be present. *)
      assert false in
  m := GroupSet.add (Data.get_group coords) !m

let add store anim ?(level = 0) pos =
  assert (Animation.check_size anim) ;
  store := create_level !store level ;
  let obj =
    ref {
      id = Id.fresh () ;
      position = pos ;
      level ;
      display = anim ;
      move = None ;
      random = clear_randomness
    } in
  let a = IMap.find level !store.data in
  let a =
    List.fold_left (fun a g ->
      Data.add_to_list a g obj) a (covered_cells obj) in
  store := { !store with data = IMap.add level a !store.data } ;
  add_moving_at store level pos ;
  obj

let remove store obj =
  add_moving_at store !obj.level !obj.position ;
  let a =
    match IMap.find_opt !obj.level !store.data with
    | Some a -> a
    | None ->
      (* As the object has been created somehow, its level must be present. *)
      assert false in
  let g = Data.get_group !obj.position in
  let l = Data.get a g in
  let l = List.filter (fun o -> !o.id <> !obj.id) l in
  let a = Data.set a g l in
  store := { !store with data = IMap.add !obj.level a !store.data }

(* Similar to the all function, but limited to a particular level.
  Instead of a store *)
let all_at_level a min_coords max_coords =
  let groups = Data.groups_between (Data.get_group min_coords) (Data.get_group max_coords) in
  let l = List.flatten (List.map (Data.get a) groups) in
  List.sort (fun o1 o2 -> Id.compare !o1.id !o2.id) l

(* Similar to the all function without a level argument. *)
let all_raw store min_coords max_coords =
  let l =
    IMap.fold (fun level a acc ->
      let p = Projection.from_screen level in
      all_at_level a (p min_coords) (p max_coords) :: acc) !store.data [] in
  List.concat l

let all store ?level min_coords max_coords =
  match level with
  | None -> all_raw store min_coords max_coords
  | Some level ->
    let p = Projection.from_screen level in
    match IMap.find_opt level !store.data with
    | None -> []
    | Some a ->
      all_at_level a (p min_coords) (p max_coords)

(* An internal version of send that only updates the object, without any check. *)
let send_direct obj e =
  obj := { !obj with display = Animation.send !obj.display e }

let send store obj ?(safe = true) e =
  if safe then
    assert (not (List.mem e Event.[
      MoveLeft ; MoveRight ; Fall ;
      Explode ; Touch ;
      Wind ;
      RandomRare ; RandomNormal ; RandomFrequent ; RandomFlicker ;
      Tau])) ;
  send_direct obj e ;
  add_moving_at store !obj.level !obj.position

let move store obj ?(ghost = false) ?(fast = false) pos' =
  let speed = if fast then 3 else 1 in
  obj := { !obj with move = Some (pos', speed, ghost) } ;
  let turns =
    let (x, y) = !obj.position in
    let (x', y') = pos' in
    let divide_round_up a b = - ((-a) / b) in
    divide_round_up (max (abs (x - x')) (abs (y - y'))) speed in
  add_moving_at store !obj.level !obj.position ;
  turns

let explode store ?(level = 0) (x, y) radius =
  let min_x = x - radius in
  let max_x = x + radius in
  let min_y = y - radius in
  let max_y = y + radius in
  store := create_level !store level ;
  let a =
    match IMap.find_opt level !store.data with
    | Some a -> a
    | None -> assert false in
  let m =
    match IMap.find_opt level !store.moving with
    | Some m -> m
    | None -> assert false in
  let groups =
    Data.groups_between
      (Data.get_group (min_x, min_y))
      (Data.get_group (max_x, max_y)) in
  m := GroupSet.union (GroupSet.of_list groups) !m ;
  List.iter (fun g ->
      let objs = Data.get a g in
      List.iter (fun obj ->
        let (x, y) = !obj.position in
        if x >= min_x && x <= max_x && y >= min_y && y <= max_y then
          send_direct obj Event.Explode) objs
    ) groups

(* Compute a new position towards the target position with the provided speed.
  Return the new position as well as an optionnal event dependent of the direction of the move. *)
let move_towards position target speed =
  let closer x tx =
    if x > tx then max (x - speed) tx
    else min (x + speed) tx in
  let x' = closer (fst position) (fst target) in
  let y' = closer (snd position) (snd target) in
  let e =
    if y' < snd position then Some Event.Fall
    else if x' > fst position then Some Event.MoveRight
    else if x' < fst position then Some Event.MoveLeft
    else None in
  ((x', y'), e)

(* Fold over all the objects in a list of groups, but only once per object.
  The f function might update the bidirectionnal array of data along the way:
  it is thus transmitted along the way. *)
let fold_once_over_groups f gs a acc =
  let module ISet = Set.Make (Id) in
  let seen_objs = ref ISet.empty in
  GroupSet.fold (fun g (a, acc) ->
    let objs = Data.get a g in
    List.fold_left (fun (a, acc) obj ->
      if ISet.mem !obj.id !seen_objs then (a, acc)
      else (
        seen_objs := ISet.add !obj.id !seen_objs ;
        f a acc obj
      )) (a, acc) objs) gs (a, acc)

(* Remove an object from a list. *)
let rec remove_obj obj = function
  | [] -> []
  | obj' :: l when !obj.id = !obj'.id -> l
  | obj' :: l -> obj' :: remove_obj obj l

(* Speed of the wind, in screen pixels. *)
let wind_speed = 4

(* Average separation between two wind waves. *)
let wind_separation = 400

(* Speed of birds, in screen pixels. *)
let bird_speed = 2

(* Average separation between two birds. *)
let bird_spawning = 100

let step store min_screen max_screen =
  let module GroupMap = Map.Make (struct type t = Data.group let compare = compare end) in
  (* Send a moving event to the moving objects, as well as moving them. *)
  let data =
    IMap.fold (fun level moving data ->
      let a =
        match IMap.find_opt level data with
        | Some a -> a
        | None -> assert false in
      let gs = !moving in
      (* All the coordinates touched by a moving object.
        We associate [true] or [false] depending on whether several objects
        touches this pixel or not.
        For efficiency reasons, we split the map with groups first. *)
      let touched_pixels =
        let (_a, m) =
          fold_once_over_groups (fun a m obj ->
            match !obj.move with
            | None -> (a, m)
            | Some (_target, _speed, true) -> (a, m)
            | Some (_target, _speed, false) ->
              let m =
                let img = Animation.image !obj.display in
                let (dimx, dimy) = Subimage.dimensions img in
                let (posx, posy) = !obj.position in
                let id v = v in
                List.fold_left (fun m x ->
                  List.fold_left (fun m y ->
                    let (_r, _g, _b, a) = Subimage.read img (x, y) in
                    if a > 0 then (
                      let pos = (x + posx, y + posy) in
                      let g = Data.get_group pos in
                      let mm =
                        match GroupMap.find_opt g m with
                        | None -> CoordMap.empty
                        | Some mm -> mm in
                      let mm = CoordMap.add pos (CoordMap.mem pos mm) mm in
                      GroupMap.add g mm m
                    ) else m) m (List.init dimy id)) m (List.init dimx id) in
              (a, m)) gs a GroupMap.empty in
        m in
      (* The [strong] argument is there to indicate that the object itself
        is moving, and thus that at least two object (itself and another one)
        much touches this pixel for it to be considered touched. *)
      let read_touched_pixels ?(strong = false) pos =
        let g = Data.get_group pos in
        match GroupMap.find_opt g touched_pixels with
        | None -> false
        | Some mm ->
          match CoordMap.find_opt pos mm with
          | Some twice -> if strong then twice else true
          | None -> false in
      (* Whether an object touches a moving object. *)
      let touched obj =
        let img = Animation.image !obj.display in
        let (dimx, dimy) = Subimage.dimensions img in
        let (posx, posy) = !obj.position in
        let strong = !obj.move <> None in
        let id v = v in
        List.exists (fun x ->
          List.exists (fun y ->
            let (_r, _g, _b, a) = Subimage.read img (x, y) in
            if a > 0 then read_touched_pixels ~strong (x + posx, y + posy)
            else false) (List.init dimy id)) (List.init dimx id) in
      (* Considering the birds of this level. *)
      let gs =
        let birds =
          match IMap.find_opt level !store.bird_locations with
          | None -> CoordMap.empty
          | Some m -> m in
        CoordMap.fold (fun _coord o gs ->
          match o with
          | Some obj ->
              if touched obj then ( (* TODO: Currently this will always be the case as the bird is itself moving… *)
              (* This bird will fly away. *)
              let target =
                let max_x = Data.largest_x a in
                let (x, y) = !obj.position in
                let target_x = x + 2 * (max_x - x) in
                (target_x, y + (target_x - x) / 3) in
              obj := { !obj with move = Some (target, bird_speed, false) } ;
              GroupSet.add (Data.get_group !obj.position) gs
            ) else gs
          | None -> gs) birds gs in
      (* All the groups that we are going to consider this turn. *)
      let gs =
        let add delta (x, y) = (x + delta, y + delta) in
        let delta = 20 in
        let min_screen = add (-delta) min_screen in
        let max_screen = add delta max_screen in
        GroupSet.union gs (GroupSet.of_list (all_groups_within level min_screen max_screen)) in
      moving := GroupSet.empty ;
      (* Considering all the objects in the relevant area of this level. *)
      let (a, ()) =
        fold_once_over_groups (fun a () obj ->
          send_direct obj Event.Tau ;
          let trigger_random e deciseconds value set =
            if Randomness.ticking value then (
              if Randomness.fire value then send_direct obj e ;
              let value = Randomness.step value in
              obj := { !obj with random = (set !obj.random value) }
            ) else (
              if Animation.listen (!obj.display) e then
                let value = Randomness.reset ((deciseconds * Animation.frames_per_second) / 10) in
                obj := { !obj with random = (set !obj.random value) }
            ) in
          trigger_random Event.RandomRare 300 !obj.random.rare (fun r v -> { r with rare = v }) ;
          trigger_random Event.RandomNormal 50 !obj.random.normal (fun r v -> { r with normal = v }) ;
          trigger_random Event.RandomFrequent 10 !obj.random.frequent (fun r v -> { r with frequent = v }) ;
          trigger_random Event.RandomFlicker 2 !obj.random.flicker (fun r v -> { r with flicker = v }) ;
          (* Check that this object touches a (non-ghost) moving object.
            Note that this includes the current object itself: moving objects should generally avoid
            reacting to [Event.Touch]. *)
          if Animation.listen !obj.display Event.Touch && touched obj then
            send_direct obj Event.Touch ;
          match !obj.move with
          | None -> (a, ())
          | Some (tpos, speed, _ghost) ->
            let gs_before = covered_cells obj in
            let (position, e) = move_towards !obj.position tpos speed in
            Option.iter (send_direct obj) e ;
            obj := { !obj with position } ;
            if position = tpos then obj := { !obj with move = None } ;
            let gs_after = covered_cells obj in
            let gs_after_set = GroupSet.of_list gs_after in
            let (gs_inter, gs_before_only) =
              List.partition (fun g -> GroupSet.mem g gs_after_set) gs_before in
            let gs_after_only =
              let gs_inter_set = GroupSet.of_list gs_inter in
              List.filter (fun g -> not (GroupSet.mem g gs_inter_set)) gs_after in
            let a =
              List.fold_left (fun a g -> Data.map_at a g (remove_obj obj)) a gs_before_only in
            let a =
              List.fold_left (fun a g -> Data.add_to_list a g obj) a gs_after_only in
            if !obj.move <> None then moving := GroupSet.union gs_after_set !moving ;
            (a, ())) gs a () in
      IMap.add level a data) !store.moving !store.data in
  store := { !store with data } ;
  (* Minimum and maximum coordinates with something. *)
  let (min_x, max_x) =
    IMap.fold (fun _level a (min_x, max_x) ->
      (min min_x (Data.smallest_x a), max max_x (Data.largest_x a))) !store.data (0, 0) in
  (* Move wind, and trigger its associated events. *)
  List.iter (fun wind_x ->
    IMap.iter (fun _level a ->
      Data.iter_at_x (List.iter (fun obj ->
        send_direct obj Event.Wind)) a wind_x) !store.data) !store.wind ;
  let wind =
    List.filter_map (fun wind_x ->
      if wind_x > max_x then None
      else Some (wind_x + wind_speed)) !store.wind in
  store := { !store with wind } ;
  (* Create new wind. *)
  let () =
    if Random.int wind_separation = 0 then
      store := { !store with wind = min_x :: !store.wind } in
  (* Removing birds getting too far away. *)
  let () =
    let bird_locations =
      IMap.map (fun m ->
        CoordMap.map (fun o ->
          match o with
          | Some obj ->
            let pos = !obj.position in
            if fst pos > max_x - 30 then (
              remove store obj ;
              None
            ) else Some obj
          | None -> None) m) !store.bird_locations in
    store := { !store with bird_locations } in
  (* Creating new birds. *)
  let () =
    if Array.length !store.birds > 0 && Random.int bird_spawning = 0 then (
      let i = Random.int (Array.length !store.birds) in
      let bird = !store.birds.(i) in
      let bindings = Array.of_list (IMap.bindings !store.bird_locations) in
      if Array.length bindings > 0 then (
        let i = Random.int (Array.length bindings) in
        let (level, m) = bindings.(i) in
        let bindings = Array.of_list (CoordMap.bindings m) in
        if Array.length bindings > 0 then (
          let i = Random.int (Array.length bindings) in
          let (coord, o) = bindings.(i) in
          match o with
          | Some _ -> () (* There is already a bird for this place. *)
          | None ->
            let start =
              let (x, y) = coord in
              let min_x = min_x + 30 in (* This ad hoc constant avoid the reallocation of the data. *)
              (min_x, y + (x - min_x) / 3) in
            let obj = add store bird ~level start in
            obj := { !obj with move = Some (coord, bird_speed, false) } ;
            let m = CoordMap.add coord (Some obj) m in
            store := { !store with bird_locations = IMap.add level m !store.bird_locations }
        )
      )
    ) in
  ()

