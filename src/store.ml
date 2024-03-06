
(* Module declaration for integers. *)
module Integer = struct
  type t = int
  let compare = compare
end
module IMap = Map.Make (Integer)

(* Generation of new identifiers. *)
module Id : sig
  type t
  val fresh : unit -> t
end = struct
  type t = int
  let fresh =
    let available = ref 0 in
    fun () ->
      let ret = !available in
      incr available ;
      ret
end

(* The objects stored. *)
type obj_store = {
  id : Id.t (* A unique identifier. *) ;
  position : int * int (* Its precise position. *) ;
  display : Animation.t (* Its animation automaton. *) ;
  move : ((int * int) * int * bool) option (* When the object is moving, this is its target
    position, speed (in pixel per turn, always positive), and ghostness. *)
}

(* The store array groups objects by their x-coordinates, but not every x-coordinates is associated
  by a cell. Instead objects are grouped by several x-coordinates into a single cell. *)
let group_size = 12

(* The store maps each level to an array of objects. *)
type store = {
  data : (obj_store ref list array) IMap.t (* The actual store, for each level. *) ;
  wind : int list (* The groups where there is currently wind *)
}

type t = store ref

(* Get the index within the store array corresponding to an x-coordinate. *)
let get_group x =
  if x < 0 then
    1 + 2 * ((-x) / group_size)
  else 2 * (x / group_size)

let%test "get_group" =
  List.for_all (fun b -> b) [
    get_group 0 = get_group 1 ;
    get_group group_size = get_group (group_size + 1) ;
    get_group group_size <> get_group (group_size - 1) ;
    get_group (-1) <> get_group 0 ;
    get_group (-group_size) <> get_group (1 - group_size) ;
    get_group (-group_size) <> get_group group_size
  ]

(* We store a direct reference to the object within the cell list: this enables us to directly
  manipulate it whithout having to update its carrying list.
  We also attach the level to be able to refind it into the store. *)
type obj = int * obj_store ref

(* Write an array (returning it) at the provided index if it fits the array.
  Otherwise a new array is created, copied, with the rest filled as empty lists,
  and with the newly available index written up. *)
let write a i v =
  assert (i >= 0) ;
  let a =
    if i < Array.length a then (
      a
    ) else (
      let b = Array.make (1 + 2 * Array.length a) [] in
      Array.blit a 0 b 0 (Array.length a) ;
      b
    ) in
  a.(i) <- v ;
  a

(* As for write, this fetches the list of objects in an array, returning the default
  empty list if out of bounds. *)
let read a i =
  if i < Array.length a then a.(i)
  else []

let create () =
  ref {
    data = IMap.empty ;
    wind = []
  }

let add store anim ?(level = 0) pos =
  let a =
    match IMap.find_opt level !store.data with
    | Some a -> a
    | None -> Array.make 10 [] in
  let obj =
   ref {
     id = Id.fresh () ;
     position = pos ;
     display = anim ;
     move = None
   } in
  let i = get_group (fst pos) in
  let l = read a i in
  let a = write a i (obj :: l) in
  store := { !store with data = IMap.add level a !store.data } ;
  (level, obj)

let remove store (level, obj) =
  let a =
    match IMap.find_opt level !store.data with
    | Some a -> a
    | None -> assert false in
  let i = get_group (fst !obj.position) in
  assert (i < Array.length a) ;
  let l = read a i in
  let l = List.filter (fun o -> !o.id <> !obj.id) l in
  let a = write a i l in
  store := { !store with data = IMap.add level a !store.data }

let send _store (_level, obj) ?(safe = true) e =
  if safe then
    assert (not (List.mem e Event.[
      MoveLeft ; MoveRight ; Fall ;
      Explode ; Touch ;
      Wind ;
      RandomRare ; RandomNormal ; RandomFrequent ; RandomFlicker ;
      Tau])) ;
  obj := { !obj with display = Animation.send !obj.display e }

let move _store (_level, obj) ?(ghost = false) ?(fast = false) pos' =
  let speed = if fast then 3 else 1 in
  obj := { !obj with move = Some (pos', speed, ghost) } ;
  let turns =
    let (x, y) = !obj.position in
    let (x', y') = pos' in
    let divide_round_up a b = - ((-a) / b) in
    divide_round_up (max (abs (x - x')) (abs (y - y'))) speed in
  turns

let explode store ?(level = 0) (x, y) radius =
  let a =
    match IMap.find_opt level !store.data with
    | Some a -> a
    | None ->
      let a = Array.make 10 [] in
      store := { !store with data = IMap.add level a !store.data } ;
      a in
  let min_x = x - radius in
  let max_x = x + radius in
  let min_y = y - radius in
  let max_y = y + radius in
  let all_groups =
    let all_x = List.init (max_x - min_x + 1) (fun x -> x + min_x) in
    let all_groups = List.map get_group all_x in
    List.sort_uniq compare all_groups in
  List.iter (fun i ->
    List.iter (fun obj ->
      let (x, y) = !obj.position in
      if x >= min_x && x <= max_x && y >= min_y && y <= max_y then
        send store (level, obj) ~safe:false Event.Explode) (read a i)
    ) all_groups

let step store =
  (* TODO: Move wind, and trigger its associated events. *)
  (* TODO: Create new wind, as well as the randomly occuring events. *)
  (* TODO: Send a moving event to the moving objects, as well as moving them. *)
  ignore store (* TODO *)

