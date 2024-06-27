
(* There are several interfaces programmed for this program.
  They are not located in the src/ subfolder, but a specific one for each interface.
  Each interface should implement the module below. *)

(* Cardinal directions (to represent keystrokes). *)
type direction =
  | North
  | West
  | South
  | East

module type T = sig

  (* The type with which one should interact with the interface. *)
  type t

  (* Initialise the interface of a given width and height.
    Called only once. *)
  val init : int -> int -> t

  (* Display the (r, g, b) pixel at position (x, y). *)
  val write : t -> (int * int * int) -> (int * int) -> unit

  (* Flushes the interface to actually display all the pixels to the user.
    Note that depending on the implementations, pixels may be displayed as soon as write. *)
  val flush : t -> unit

  (* Attach a function to be called when the user clicks on the interface.
    The callback function takes as argument the coordinates.
    Note that calls to on_click resets the previous attached functions. *)
  val on_click : t -> ((int * int) -> unit) -> unit

  (* Same than on_click, but for a move event (the user held the click while moving the cursor).
    The callback function takes the initial position and the final position of the move. *)
  val on_move : t -> ((int * int) -> (int * int) -> unit) -> unit

  (* Called on a key pressed.
    If it was an arrow key, it provides the callback function with the corresponding direction.
    Otherwise, the callback function only receives None. *)
  val on_key_pressed : t -> (direction option -> unit) -> unit

end

