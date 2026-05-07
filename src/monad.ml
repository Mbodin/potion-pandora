
module type T = sig
  type _ m

  (* The monadic operators. *)
  val return : 'a -> 'a m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
end

module Ops (M : T) = struct

  include M

  let ( %% ) a b =
    let* () = a in b

  let rec for_ min max f =
    if min > max then return ()
    else
      f min %%
      for_ (min + 1) max f

  let iter_ f l =
    List.fold_left (fun u v ->
      u %% f v) (return ()) l

  let iteri_ f l =
    let* _length =
      List.fold_left (fun i v ->
        let* i = i in
        let i = i + 1 in
        f i v %%
        return i) (return 0) l in
    return ()

  let list_map_ f l =
    let* l =
      List.fold_left (fun l a ->
        let* l in
        let* b = f a in
        return (b :: l)) (return []) l in
    return (List.rev l)

  let list_mapi_ f l =
    let* (l, _length) =
      List.fold_left (fun li a ->
        let* (l, i) = li in
        let i = i + 1 in
        let* b = f i a in
        return (b :: l, i)) (return ([], 0)) l in
    return (List.rev l)

end

