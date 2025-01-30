
(* We use a writer monad within this file.
  It stores a list of substrings to be written, in reverse order. *)
type 'a monad = 'a * string list

let return (type t) (x : t) : t monad = (x, [])

let bind (type a b) ((a, l1) : a monad) (f : a -> b monad) : b monad =
  let (r, l2) = f a in
  (r, l2 @ l1)

let ( let* ) = bind

let ( %% ) (type t) : unit monad -> t monad -> t monad =
  fun m1 m2 -> bind m1 (fun () -> m2)

let write str : unit monad = ((), [str])

let escape (type t) : t monad -> t * string =
  fun (t, l) -> (t, String.concat "" (List.rev l))

(* Converts 8 bits to a character. *)
let bits_to_char b1 b2 b3 b4 b5 b6 b7 b8 =
  let i =
    List.fold_left (fun acc b -> 2 * acc + if b then 1 else 0) 0
      [b1; b2; b3; b4; b5; b6; b7; b8] in
  Char.chr i

(* A monad to write individual bits (in forward order). *)
type 'a monadbit = ('a * bool list) monad

let returnbit (type t) (x : t) : t monadbit = return (x, [])

let bindbit (type a b) (m : a monadbit) (f : a -> b monadbit) : b monadbit =
  let* (a, bl1) = m in
  let* (b, bl2) = f a in
  let rec aux = function
    | b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: bl ->
      let c = bits_to_char b1 b2 b3 b4 b5 b6 b7 b8 in
      write (String.make 1 c) %%
      aux bl
    | bl -> return (b, bl) in
  aux (bl1 @ bl2)

let writebit (b : bool) : unit monadbit = return ((), [b])

let writebits (lb : bool list) : unit monadbit = return ((), lb)

let escapebits (type t) (m : t monadbit) : t * string =
  let m =
    (* This flushes the waiting bits if possible. *)
    bindbit m returnbit in
  let ((r, bl), str) = escape m in
  let rec aux = function
    | [] -> (r, str)
    | [b1; b2; b3; b4; b5; b6; b7; b8] ->
      (r, str ^ String.make 1 (bits_to_char b1 b2 b3 b4 b5 b6 b7 b8))
    | _b1 :: _b2 :: _b3 :: _b4 :: _b5 :: _b6 :: _b7 :: _b8 :: _ -> assert false
    | bl -> aux (bl @ [false]) in
  aux bl


(* Compresses a string. *)
let deflate_string ?(level=4) str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ; len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  Zl.Higher.compress ~level ~dynamic:true ~w ~q ~refill ~flush i o ;
  Buffer.contents r


(* The first bit encodes the sign, then each group of one character starts with a bit
 stating whether there will be another group, and the rest is coding. *)
let encode_int i : unit monad =
  let one_char i = write (String.make 1 (Char.chr i)) in
  let rec aux i =
    assert (i >= 0) ;
    if i < 128 then
      one_char i
    else (
      one_char (128 + (i mod 128)) %%
      aux (i / 128)
    ) in
  if i < 0 then (
    let i = i + 1 in
    one_char (128 + (if -i < 64 then 0 else 64) + ((-i) mod 64)) %%
    if -i < 64 then return ()
    else aux ((-i) / 64)
  ) else (
    one_char ((if i < 64 then 0 else 64) + (i mod 64)) %%
    if i < 64 then return ()
    else aux (i / 64)
  )

let rec encode_monad : type t. t Save.t -> t -> unit monad =
  let encode_list (type t) (s : t Save.t) (l : t list) : unit monad =
    encode_int (List.length l) %%
    let rec iter = function
      | [] -> return ()
      | x :: l -> encode_monad s x %% iter l in
    (* We store list reversed to help their read. *)
    iter (List.rev l) in function
  | Save.Unit -> fun () -> return ()
  | Save.Bool -> fun b -> if b then write "t" else write "f"
  | Save.Int -> encode_int
  | Save.String -> fun str ->
    encode_int (String.length str) %%
    write str
  | Save.Seq (s1, s2) -> fun (a, b) ->
    encode_monad s1 a %%
    encode_monad s2 b
  | Save.List s -> encode_list s
  | Save.Array s -> fun a -> encode_list s (Array.to_list a)
  | Save.Base64 s -> fun x ->
    let ((), str) = escape (encode_monad s x) in
    let str = Base64.encode_exn str in
    encode_int (String.length str) %%
    write str
  | Save.Compress s -> fun x ->
    let ((), str) = escape (encode_monad s x) in
    let str = deflate_string str in
    encode_int (String.length str) %%
    write str
  | _ -> failwith "NYI" (* TODO *)

let encode (type t) (s : t Save.t) (v : t) : string =
  let ((), str) = escape (encode_monad s v) in
  str


(* Unit tests. *)

let test_bits_to_char b1 b2 b3 b4 b5 b6 b7 b8 =
  Read.char_to_bits (bits_to_char b1 b2 b3 b4 b5 b6 b7 b8) =
    [b1; b2; b3; b4; b5; b6; b7; b8]

let%test "bits_to_char char_to_bits false" =
  test_bits_to_char false false false false false false false false
let%test "bits_to_char char_to_bits true" =
  test_bits_to_char true true true true true true true true
let%test "bits_to_char char_to_bits one false 2" =
  test_bits_to_char true false true true true true true true
let%test "bits_to_char char_to_bits one false 6" =
  test_bits_to_char true true true true true false true true


let check (type t) (s : t Save.t) (e : t) =
  e = Read.decode s (encode s e)

let%test "unit" = check Unit ()

let%test "bool true" = check Bool true
let%test "bool false" = check Bool false

let%test "int 0" = check Int 0
let%test "int 1" = check Int 1
let%test "int 10" = check Int 10
let%test "int 80" = check Int 80
let%test "int 127" = check Int 127
let%test "int 128" = check Int 128
let%test "int 200" = check Int 200
let%test "int 255" = check Int 255
let%test "int 256" = check Int 256
let%test "int 1024" = check Int 1024
let%test "int max_int - 1" = check Int (max_int - 1)
let%test "int max_int" = check Int max_int
let%test "int -1" = check Int (-1)
let%test "int -10" = check Int (-10)
let%test "int -80" = check Int (-80)
let%test "int -127" = check Int (-127)
let%test "int -128" = check Int (-128)
let%test "int -200" = check Int (-200)
let%test "int -255" = check Int (-255)
let%test "int -256" = check Int (-256)
let%test "int -1024" = check Int (-1024)
let%test "int min_int + 1" = check Int (min_int + 1)
let%test "int min_int" = check Int min_int

let%test "string empty" = check String ""
let%test "string single" = check String "1"
let%test "string newline" = check String "\n"
let%test "string abc" = check String "abc"

let%test "list []" = check (List Int) []
let%test "list [1]" = check (List Int) [1]
let%test "list [1; 2; 3]" = check (List Int) [1; 2; 3]
let%test "list [1; -2; 3]" = check (List Int) [1; -2; 3]
let%test "list [[]; [1]; [1; 2]]" = check (List (List Int)) [[]; [1]; [1; 2]]
let%test "list [abc; d; ef]" = check (List String) ["abc"; "d"; "ef"]

let%test "base64" = check (Base64 (List Int)) [-1; 2; -3]
let%test "compress" = check (Compress (List Int)) [-1; 2; -3]

let%test "compress base64 list base64 compress int" =
  check (Compress (Base64 (List (Base64 (Compress Int))))) [1; -2; 3]

