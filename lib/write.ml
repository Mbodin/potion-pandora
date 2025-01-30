
(* We use a writer monad within this file.
  It stores a list of substrings to be written, in reverse order. *)
type 'a monad = 'a * string list

let return (type t) (x : t) : t monad = (x, [])

let bind (type a b) ((a, l) : a monad) (f : a -> b monad) : b monad =
  let (r, l') = f a in
  (r, l' @ l)

let ( let* ) = bind

let ( %% ) (type t) : unit monad -> t monad -> t monad =
  fun m1 m2 -> bind m1 (fun () -> m2)

let write str : unit monad = ((), [str])

let escape (type t) : t monad -> t * string =
  fun (t, l) -> (t, String.concat "" (List.rev l))


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

let check (type t) (s : t Save.t) (e : t) =
  e = Read.decode s (encode s e)

let%test "unit" = check Unit ()

let%test "bool true" = check Bool true
let%test "bool false" = check Bool false

let%test "int 0" = check Int 0
let%test "int 1" = check Int 1
let%test "int 10" = check Int 10
let%test "int 127" = check Int 127
let%test "int 128" = check Int 128
let%test "int 200" = check Int 200
let%test "int 255" = check Int 255
let%test "int 256" = check Int 256
let%test "int 1024" = check Int 1024
let%test "int max_int" = check Int max_int
let%test "int -1" = check Int (-1)
let%test "int -10" = check Int (-10)
let%test "int -127" = check Int (-127)
let%test "int -128" = check Int (-128)
let%test "int -200" = check Int (-200)
let%test "int -255" = check Int (-255)
let%test "int -256" = check Int (-256)
let%test "int -1024" = check Int (-1024)
let%test "int min_int" = check Int min_int

let%test "float 0." = check Float 0.
let%test "float 1." = check Float 1.
let%test "float 2." = check Float 2.
let%test "float -1." = check Float (-1.)
let%test "float 0.5" = check Float 0.5
let%test "float 1/3" = check Float (1./.3.)
let%test "float -1/3" = check Float (-1./.3.)

let%test "string empty" = check String ""
let%test "string single" = check String "1"
let%test "string newline" = check String "\n"
let%test "string abc" = check String "abc"

let%test "list []" = check (List Int) []
let%test "list [1]" = check (List Int) [1]
let%test "list [1; 2; 3]" = check (List Int) [1; 2; 3]
let%test "list [1; -2; 3]" = check (List Int) [1; -2; 3]
let%test "list [[]; [1]; [1; 2]]" = check (List (List Int)) [[]; [1]; [1; 2]]
let%test "list [abc, d, ef]" = check (List String) ["abc", "d", "ef"]

let%test "base64" = check (Base64 (List Int)) [-1; 2; -3]
let%test "compress" = check (Compress (List Int)) [-1; 2; -3]

