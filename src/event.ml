
type t =
  | MoveLeft
  | MoveRight
  | Fall
  | Explode
  | Touch
  | LookDown
  | LookUp
  | LookBehind
  | Wind
  | RandomRare
  | RandomNormal
  | RandomFrequent
  | RandomFlicker
  | Tau
  [@@deriving enum]
(* Note: the deriving above shadows min and max. *)

let compare e1 e2 =
  compare (to_enum e1) (to_enum e2)

(* Total maps are implemented as an array based on the index given by to_int. *)
type 'a map = 'a array

let create_map f =
  Array.init (max + 1) (fun i ->
    match of_enum i with
    | None -> assert false
    | Some e -> f e)

let fetch m e = m.(to_enum e)

