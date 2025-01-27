
(* We use a reader monad within this file.
 It takes as argument the string being read, the current index, and returns the current index. *)
type 'a monad = string -> int -> 'a * int

let return (type t) (x : t) : t monad = fun _str index -> (x, index)

let bind (type a b) (m : a monad) (f : a -> b monad) : b monad =
  fun str index ->
    let (a, index) = m str index in
    f a str index

(* Read the current character. *)
let read : char monad =
  fun str index ->
    assert (index < String.length str) ;
    (str.(index), index + 1)

let rec decode_monad : 'a Save.t -> 'a monad = function
  | Save.Unit -> return ()
  | Save.Bool ->
    let* c = read in
    (match c with
     | "t" -> true
     | "f" -> false
     | _ -> assert false)
  | _ -> failwith "NYI"

let decode (type t) (s : t Save.t) str =
  let (r, index) = decode_monad s str 0 in
  assert (index = String.length str) ;
  r

