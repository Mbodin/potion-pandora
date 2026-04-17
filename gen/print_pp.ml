
(* A program to dump the generated pp.ml files. *)

let dump in_path =
  let ast = Pparse.read_ast Structure in_path in
  Format.printf "%a\n" Pprintast.structure ast

let () =
  if Array.length Sys.argv = 2 then
    dump Sys.argv.(1)
  else
    print_endline "Example usage: esy dune exec gen/print_pp.exe _build/default/lib/read.pp.ml"

