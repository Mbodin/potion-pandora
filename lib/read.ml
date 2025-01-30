
(* We use a reader monad within this file.
 It takes as argument the string being read, the current index, and returns the current index. *)
type 'a monad = string -> int -> 'a * int

let return (type t) (x : t) : t monad = fun _str index -> (x, index)

let bind (type a b) (m : a monad) (f : a -> b monad) : b monad =
  fun str index ->
    let (a, index) = m str index in
    f a str index

let ( let* ) = bind

(* Read the current character. *)
let read : char monad =
  fun str index ->
    assert (index < String.length str) ;
    (str.[index], index + 1)

(* Read a string of size n. *)
let readn n : string monad =
  fun str index ->
    assert (index + n <= String.length str) ;
    (String.sub str index n, index + n)

(* Check that no characters are left. *)
let eof : unit monad =
  fun str index ->
    assert (index = String.length str) ;
    ((), index)

(* Forces the reader to fully read its input string. *)
let ( ~$ ) (type t) (reader : t monad) : t monad =
  let* r = reader in
  let* () = eof in
  return r


let char_to_bits c =
  let i = Char.code c in
  let rec aux acc i = function
    | 0 -> assert (i = 0) ; acc
    | k -> aux ((i mod 2 = 1) :: acc) (i / 2) (k - 1) in
  aux [] i 8

(* A monad to read individual bits.
 The input list is strictly bounded by 8: it is the leftover of a previous read. *)
type 'a monadbit = bool list -> ('a * bool list) monad

let returnbit (type t) (x : t) : t monadbit = fun bl -> return (x, bl)

let bindbit (type a b) (m : a monadbit) (f : a -> b monadbit) : b monadbit =
  fun bl ->
    assert (List.length bl < 8) ;
    let* (a, bl) = m bl in
    f a bl

let readbit : bool monadbit = function
  | b :: bl -> return (b, bl)
  | [] ->
    let* c = read in
    match char_to_bits c with
    | [] -> assert false
    | b :: bl -> return (b, bl)

let readnbits : int -> bool list monadbit =
  let rec aux acc = function
  | 0 -> returnbit (List.rev acc)
  | n ->
    bindbit (readbit) (fun b ->
      aux (b :: acc) (n - 1)) in
  aux []

(* Check that no bits are left. *)
let eofbits : unit monadbit = fun bl ->
  (* There might have been additionnal bits, for padding, but they should all be false. *)
  assert (List.for_all (not) bl) ;
  let* () = eof in
  return ((), [])


(* Decompresses a string. *)
let deflate_string ?(level=4) str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let allocate bits = De.make_window ~bits in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ; len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  match Zl.Higher.uncompress ~allocate ~refill ~flush i o with
  | Ok () -> Buffer.contents r
  | Error (`Msg err) -> failwith ("deflate_string: " ^ err)


let decode_int : int monad =
  let rec aux () =
    let* c = read in
    let i = Char.code c in
    if i < 128 then return i
    else (
      let r = i - 128 in
      let* d = aux () in
      return (r + 128 * d)
    ) in
  let* c = read in
  let i = Char.code c in
  if i < 128 then (
    (* Positive value *)
    if i < 64 then return i
    else (
      let r = i - 64 in
      let* d = aux () in
      return (r + 64 * d)
    )
  ) else (
    (* Negative value *)
    let i = i - 128 in
    if i < 64 then return (-i - 1)
    else (
      let r = i - 64 in
      let* d = aux () in
      return (-(r + 64 * d) - 1)
    )
  )

let rec decode_monad : type t. t Save.t -> t monad =
  let decode_list (type t) (s : t Save.t) : t list monad =
    let* size = decode_int in
    let rec aux (acc : t list) : int -> t list monad = function
      | 0 ->
        (* Lists are already stored reversed, so there is no need to reverse it there. *)
        return acc
      | n ->
        let* x = decode_monad s in
        aux (x :: acc) (n - 1) in
    aux [] size in function
  | Save.Unit -> return ()
  | Save.Bool ->
    let* c = read in
    (match c with
     | 't' -> return true
     | 'f' -> return false
     | _ -> assert false)
  | Save.Int -> decode_int
  | Save.String ->
    let* l = decode_int in
    readn l
  | Save.Seq (s1, s2) ->
    let* a = decode_monad s1 in
    let* b = decode_monad s2 in
    return (a, b)
  | Save.List s -> decode_list s
  | Save.Array s ->
    let* l = decode_list s in
    return (Array.of_list l)
  | Save.Base64 s ->
    let* size = decode_int in
    let* str = readn size in
    let str = Base64.decode_exn str in
    let (r, _index) = ~$ (decode_monad s) str 0 in
    return r
  | Save.Compress s ->
    let* size = decode_int in
    let* str = readn size in
    let str = deflate_string str in
    let (r, _index) = ~$ (decode_monad s) str 0 in
    return r
  | _ -> failwith "NYI" (* TODO *)

let decode (type t) (s : t Save.t) str =
  let (r, _index) = ~$ (decode_monad s) str 0 in
  r

