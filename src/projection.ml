
type screen_coords = int * int
type game_coords = int * int

(* A percentage for each level of how much pixels move compared to the base level.
  If the ratio is exactly 100, then pixels move the same way than in the base level.
  If 0, then pixels basically never move.
  If 50, then for each time that the base level move of two pixels, then this level
  move only one. *)
let ratio level =
  let level = min level 100 in
  100 - level

let%test "ratio 0" =
  ratio 0 = 100

let%test "positive ratio" =
  List.for_all (fun level -> ratio level >= 0)
    [ -100 ; -10 ; -5 ; -1 ; 0 ; 1 ; 5 ; 10 ; 50 ; 100 ]

let%test "decreasing ratio" =
  List.for_all (fun (level1, level2) -> ratio level1 >= ratio level2)
    [ (-100, -10) ; (-10, -1) ; (-1, 0) ; (0, 1) ; (1, 5) ; (5, 10) ; (50, 100) ]

let to_screen level (x, y) =
  let proj x =
    let r = ratio level in
    Integer.div (r * x) 100 in
  (proj x, proj y)

let from_screen level (x, y) =
  let f x =
    let r = ratio level in
    Integer.div (100 * x) r in
  (f x, f y)

