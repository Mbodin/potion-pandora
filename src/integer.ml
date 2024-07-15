
type t = int

let compare = compare

let div a b =
  if (a >= 0) = (b >= 0) then (
    (* They have the same sign, so the truncating division is fine. *)
    a / b
  ) else (
    (* They have opposite signs, so we need to compute back the euclidean division. *)
    let d = a / b in
    d - 1
  )

