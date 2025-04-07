
module ColorMap =
  Map.Make (struct
    type t = Save.color
    let compare = compare
  end)

(* We use a writer monad within this file.
 It stores a list of substrings to be written, in reverse order.
 It also gets the current color mapping. *)
type 'a monad = int ColorMap.t -> 'a * string list

let return (type t) (x : t) : t monad = fun _colors -> (x, [])

let bind (type a b) (o : a monad) (f : a -> b monad) : b monad =
  fun colors ->
    let (a, l1) = o colors in
    let (r, l2) = f a colors in
    (r, l2 @ l1)

let ( let* ) = bind

let ( %% ) (type t) : unit monad -> t monad -> t monad =
  fun m1 m2 -> bind m1 (fun () -> m2)

let write str : unit monad = fun _colors -> ((), [str])

(* The first identifier is always [0]. *)
let transparent = 0

(* Escape the monad. *)
let escape (type t) : t monad -> t * string =
  fun m ->
    let colors = ColorMap.add (0, 0, 0, 0) transparent ColorMap.empty in
    let (t, l) = m colors in
    (t, String.concat "" (List.rev l))

(* Get the identifier of a color. *)
let get_color_id r g b a : int monad =
  fun colors ->
    if a = 0 then (transparent, [])
    else
      match ColorMap.find_opt (r, g, b, a) colors with
      | Some id -> (id, [])
      | None -> assert false

(* Add a color to the current palette. *)
let add_color r g b a (m : 'a monad) : 'a monad =
  fun colors ->
    let id = ColorMap.cardinal colors in
    let colors = ColorMap.add (r, g, b, a) id colors in
    m colors

(* Get the current number of registered colors. *)
let num_colors : int monad =
  fun colors -> (ColorMap.cardinal colors, [])

(* Write a single character. *)
let write_char i = write (String.make 1 i)

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

let ( %%% ) (type t) : unit monadbit -> t monadbit -> t monadbit =
  fun m1 m2 -> bindbit m1 (fun () -> m2)

let writebits (lb : bool list) : unit monadbit = return ((), lb)

let writebit (b : bool) : unit monadbit = writebits [b]

let escapebits (type t) (m : t monadbit) : t monad =
  let m =
    (* This flushes the waiting bits if possible. *)
    bindbit m returnbit in
  let* (r, bl) = m in
  let rec aux = function
    | [] -> return r
    | [b1; b2; b3; b4; b5; b6; b7; b8] ->
      write_char (bits_to_char b1 b2 b3 b4 b5 b6 b7 b8) %%
      return r
    | _b1 :: _b2 :: _b3 :: _b4 :: _b5 :: _b6 :: _b7 :: _b8 :: _ -> assert false
    | bl -> (* We fill-in the remaining bits with false. *)
      aux (bl @ [false]) in
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

(* Encode a positive integer. *)
let rec encode_positive i =
  assert (i >= 0) ;
  if i < 128 then
    write_char (Char.chr i)
  else (
    write_char (Char.chr (128 + (i mod 128))) %%
    encode_positive (i / 128)
  )

(* The first bit encodes the sign, then each group of one character starts with a bit
 stating whether there will be another group, and the rest is coding. *)
let encode_int i : unit monad =
  if i < 0 then (
    let i = i + 1 in
    write_char (Char.chr (128 + (if -i < 64 then 0 else 64) + ((-i) mod 64))) %%
    if -i < 64 then return ()
    else encode_positive ((-i) / 64)
  ) else (
    write_char (Char.chr ((if i < 64 then 0 else 64) + (i mod 64))) %%
    if i < 64 then return ()
    else encode_positive (i / 64)
  )

let rec encode_monad : type t. t Save.t -> t -> unit monad =
  let encode_list (type t) (s : t Save.t) (l : t list) : unit monad =
    encode_positive (List.length l) %%
    let rec iter = function
      | [] -> return ()
      | x :: l -> encode_monad s x %% iter l in
    (* We store list reversed to help their read. *)
    iter (List.rev l) in function
  | Save.Unit -> fun () -> return ()
  | Save.Bool -> fun b -> if b then write "t" else write "f"
  | Save.Int -> encode_int
  | Save.String -> fun str ->
    encode_positive (String.length str) %%
    write str
  | Save.Seq (s1, s2) -> fun (a, b) ->
    encode_monad s1 a %%
    encode_monad s2 b
  | Save.Option s -> (function
    | None -> write "n"
    | Some x ->
      write "s" %%
      encode_monad s x)
  | Save.List s -> encode_list s
  | Save.Array s -> fun a -> encode_list s (Array.to_list a)
  | Save.AddColor s -> fun ((r, g, b, a), data) ->
    write (Printf.sprintf "%c%c%c%c" (Char.chr r) (Char.chr g) (Char.chr b) (Char.chr a)) %%
    add_color r g b a (encode_monad s data)
  | Save.Image -> fun img ->
    encode_positive img.Image.width %%
    encode_positive img.Image.height %%
    let* num_colors = num_colors in
    let num_bits = Read.log2 num_colors in
    assert (num_bits > 0) ;
    let rec encode_data x y : unit monadbit =
      if y = img.Image.height then returnbit ()
      else if x = img.Image.width then encode_data 0 (y + 1)
      else (
        assert (x >= 0 && x < img.Image.width) ;
        assert (y >= 0 && y < img.Image.height) ;
        let (r, g, b, a) = Image.read_rgba img x y (fun r g b a -> (r, g, b, a)) in
        let* color_id = get_color_id r g b a in
        let rec encode_id k m =
          assert (m >= 0) ;
          if k = 0 then (
            assert (m = 0) ;
            returnbit ()
          ) else (
            assert (k > 0) ;
            writebit (m mod 2 = 1) %%%
            encode_id (k - 1) (m / 2)
          ) in
        encode_id num_bits color_id %%%
        encode_data (x + 1) y
      ) in
    escapebits (encode_data 0 0)
  | Save.Base64 s -> fun x ->
    let ((), str) = escape (encode_monad s x) in
    let str = Base64.encode_exn str in
    encode_positive (String.length str) %%
    write str
  | Save.Compress s -> fun x ->
    let ((), str) = escape (encode_monad s x) in
    let str = deflate_string str in
    encode_positive (String.length str) %%
    write str

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

let%test "None" = check (Option String) None
let%test "Some None" = check (Option (Option Int)) (Some None)
let%test "Some Some abc" = check (Option (Option String)) (Some (Some ("abc")))

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

let%test "image" =
  let img = Image.create_rgb ~alpha:true 1 2 in
  Image.fill_rgb ~alpha:255 img 1 2 3 ;
  let t = Save.(AddColor Image) in
  let color = (1, 2, 3, 255) in
  let (color', img') = Read.decode t (encode t (color, img)) in
  let check_pixel x y =
    Image.read_rgba img x y (fun r g b a ->
      Image.read_rgba img' x y (fun r' g' b' a' ->
        r = r' && g = g' && b = b' && a = a')) in
  color = color'
  && img.Image.width = img'.Image.width
  && img.Image.height = img'.Image.height
  && check_pixel 0 0
  && check_pixel 0 1

