
(* A program to dump the generated pp.ml files. *)

let dump in_path =
  let ast = Pparse.read_ast Structure in_path in
  Format.printf "%a\n" Pprintast.structure ast

let () =
  assert (Array.length Sys.argv = 2) ;
  dump Sys.argv.(1)

