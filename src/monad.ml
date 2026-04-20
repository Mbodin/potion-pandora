
module type T = sig
  type _ m

  (* The monadic operators. *)
  val return : 'a -> 'a m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
end

module Ops (M : T) = struct

  let rec for_ min max f =
    let open M in
    if min > max then return ()
    else
      let* () = f min in
      for_ (min + 1) max f

  let iter_ f l =
    let open M in
    List.fold_left (fun u v ->
      let* () = u in
      f v) (return ()) l

end

