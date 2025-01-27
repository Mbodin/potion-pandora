
let rec encode_int i =
  let one_char i = String.make 1 (Char.chr i) in
  if i < 0 then (
    one_char (255 + (i mod 128)) (* i mod 128 < 0 *)
    ^ encode_int (i / 128 + (min_int / 128))
  ) else if i < 128 then
    one_char i
  else (
    one_char (128 + (i mod 128))
    ^ encode_int (i / 128)
  )

let rec encode = function
  | Save.Unit -> fun () -> ""
  | Save.Bool -> fun b -> if b then "t" else "f"
  | Save.Int -> encode_int
  | _ -> failwith "NYI"

(* For tests. *)
let check (type t) (s : t Save.t) (e : t) =
  e = Read.decode s (Write.encode s e)

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

