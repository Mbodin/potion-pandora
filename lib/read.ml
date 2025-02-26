
let log2 n =
  let rec aux p k = (* p = 2^k *)
    if n <= p then k
    else aux (2 * p) (1 + k) in
  aux 1 0

let%test "log2 1" = log2 1 = 0
let%test "log2 2" = log2 2 = 1
let%test "log2 3" = log2 3 = 2
let%test "log2 4" = log2 4 = 2
let%test "log2 5" = log2 5 = 3
let%test "log2 32" = log2 32 = 5
let%test "log2 63" = log2 63 = 6
let%test "log2 64" = log2 64 = 6
let%test "log2 80" = log2 80 = 7
let%test "log2 123" = log2 123 = 7
let%test "log2 128" = log2 128 = 7


type colors = Save.color array

let initial_colors : colors = Array.make 1 (0, 0, 0, 0)

(* We use a reader monad within this file.
 It takes as argument the string being read, the current index, as well as a mapping of colors,
 and returns the current index. *)
type 'a monad = string -> int -> colors -> 'a * int

let return (type t) (x : t) : t monad = fun _str index _colors -> (x, index)

let bind (type a b) (m : a monad) (f : a -> b monad) : b monad =
  fun str index colors ->
    let (a, index) = m str index colors in
    f a str index colors

let ( let* ) = bind

let ( %% ) (type t) : unit monad -> t monad -> t monad =
  fun m1 m2 -> bind m1 (fun () -> m2)

(* Read the current character. *)
let read : char monad =
  fun str index _colors ->
    assert (index < String.length str) ;
    (str.[index], index + 1)

(* Read a string of size n. *)
let readn n : string monad =
  fun str index _colors ->
    assert (index + n <= String.length str) ;
    (String.sub str index n, index + n)

(* Check that no characters are left. *)
let eof : unit monad =
  fun str index _colors ->
    assert (index = String.length str) ;
    ((), index)

(* Forces the reader to fully read its input string. *)
let ( ~$ ) (type t) (reader : t monad) : t monad =
  let* r = reader in
  eof %%
  return r

(* Get the [i]th saved color. *)
let get_color i : Save.color monad =
  fun _str index colors ->
    assert (i < Array.length colors) ;
    (colors.(i), index)

(* Add a color to the current palette. *)
let add_color r g b a (m : 'a monad) : 'a monad =
  fun str index colors ->
    let colors = Array.append colors (Array.make 1 (r, g, b, a)) in
    m str index colors

(* Get the current number of registered colors. *)
let num_colors : int monad =
  fun _str index colors ->
    (Array.length colors, index)

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

let ( let** ) = bindbit

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
    let** b = readbit in
    aux (b :: acc) (n - 1) in
  aux []

(* Check that no bits are left when closing the monad. *)
let read_and_close_bits (type t) (k : t monadbit) : t monad =
  let* (r, bl) = k [] in
  (* There might have been additionnal bits, for padding, but they should all be false. *)
  assert (List.for_all (not) bl) ;
  return r

let _eofbits : unit monadbit = fun bl ->
  assert (List.for_all (not) bl) ;
  eof %%
  return ((), [])

let get_color_bits i : Save.color monadbit =
  fun bl ->
    let* color = get_color i in
    return (color, bl)


(* Decompresses a string. *)
let deflate_string (*?(level=4)*) str =
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


(* Decode a positive integer. *)
let rec decode_positive : int monad =
  fun str -> ( (* This is just a trick to make the compiler accept this [let rec] here. *)
    let* c = read in
    let i = Char.code c in
    if i < 128 then return i
    else (
      let r = i - 128 in
      let* d = decode_positive in
      return (r + 128 * d)
    )
  ) str

(* Decode an integer. *)
let decode_int : int monad =
  let* c = read in
  let i = Char.code c in
  if i < 128 then (
    (* Positive value *)
    if i < 64 then return i
    else (
      let r = i - 64 in
      let* d = decode_positive in
      return (r + 64 * d)
    )
  ) else (
    (* Negative value *)
    let i = i - 128 in
    if i < 64 then return (-i - 1)
    else (
      let r = i - 64 in
      let* d = decode_positive in
      return (-(r + 64 * d) - 1)
    )
  )

let rec decode_monad : type t. t Save.t -> t monad =
  let decode_list (type t) (s : t Save.t) : t list monad =
    let* size = decode_positive in
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
    let* l = decode_positive in
    readn l
  | Save.Seq (s1, s2) ->
    let* a = decode_monad s1 in
    let* b = decode_monad s2 in
    return (a, b)
  | Save.Option s ->
    let* c = read in
    (match c with
     | 'n' -> return None
     | 's' ->
        let* r = decode_monad s in
        return (Some r)
     | _ -> assert false)
  | Save.List s -> decode_list s
  | Save.Array s ->
    let* l = decode_list s in
    return (Array.of_list l)
  | Save.AddColor s ->
    let* r = read in
    let r = Char.code r in
    let* g = read in
    let g = Char.code g in
    let* b = read in
    let b = Char.code b in
    let* a = read in
    let a = Char.code a in
    let* data = add_color r g b a (decode_monad s) in
    return ((r, g, b, a), data)
  | Save.Image ->
    let* width = decode_positive in
    let* height = decode_positive in
    let* num_colors = num_colors in
    let num_bits = log2 num_colors in
    let img = Image.create_rgb ~alpha:true width height in
    let rec decode_data x y : unit monadbit =
      if x = width then decode_data 0 (y + 1)
      else if y = height then returnbit ()
      else (
        let** bl = readnbits num_bits in
        let rec to_int w acc = function
          | [] -> acc
          | b :: bl -> to_int (2 * w) (acc + if b then w else 0) bl in
        let** (r, g, b, a) = get_color_bits (to_int 1 0 bl) in
        Image.write_rgba img x y r g b a ;
        decode_data (x + 1) y
      ) in
    read_and_close_bits (decode_data 0 0) %%
    return img
  | Save.Base64 s ->
    let* size = decode_positive in
    let* str = readn size in
    let str = Base64.decode_exn str in
    let (r, _index) = ~$ (decode_monad s) str 0 initial_colors in
    return r
  | Save.Compress s ->
    let* size = decode_positive in
    let* str = readn size in
    let str = deflate_string str in
    let (r, _index) = ~$ (decode_monad s) str 0 initial_colors in
    return r

let decode (type t) (s : t Save.t) str =
  let (r, _index) = ~$ (decode_monad s) str 0 initial_colors in
  r

