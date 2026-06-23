
(* Functors to organise the screen. *)

(* This functor provides useful functions for interfaces. *)
module Ops (I : Interface.T) : sig

  (* Display a Subimage.t into the interface with its upper left corner at
    the provided coordinates. *)
  val display_image : I.t -> Subimage.t -> (int * int) -> unit I.m

  (* Display an horizontal line of this color at the given y-coordinate, and between the two
    x-coordinates (included). *)
  val horizontal_line : I.t -> (int * int * int) -> y:int -> int -> int -> unit I.m

  (* Display a vertical line of this color at the given x-coordinate, and between the two
    y-coordinates (included). *)
  val vertical_line : I.t -> (int * int * int) -> x:int -> int -> int -> unit I.m

  (* Draw an (empty) rectangle of this color around these dimensions. *)
  val rectangle : I.t -> (int * int * int) -> (int * int) -> (int * int) -> unit I.m

  (* Fill a rectangle of this color around these dimensions. *)
  val fill : I.t -> (int * int * int) -> (int * int) -> (int * int) -> unit I.m

end


(* This functor splits the screen into two independent interfaces.
  Both initialisations should happen before any other operation. *)
module SplitVertical (I : Interface.T) : sig
    module Up : Interface.T with type 'a m = 'a I.m
    module Down : Interface.T with type 'a m = 'a I.m
  end

module SplitHorizontal (I : Interface.T) : sig
    module Left : Interface.T with type 'a m = 'a I.m
    module Right : Interface.T with type 'a m = 'a I.m
  end

(* Actions associated to each button.
  The 'unit type argument is used to encode monadic returns. *)
type 'unit button_actions = {
  on_press : bool -> 'unit (* Called when a button is pressed.  The boolean states whether it was triggerred by the user or by internal functions. *) ;
  on_release : bool -> 'unit (* Called when a button is released. *) ;
  set_toggle : (bool -> 'unit) -> 'unit (* Called once with a function to set a button (true then means pressed).  Calling the provided function will call on_press or on_release with false. *)
}

(* A description of buttons and their actions. *)
module type ButtonInputs = sig
  type _ m

  (* The size of the interface used to draw the buttons. *)
  val width : int
  val height : int

  (* A description of all the buttons. *)
  val buttons : (Subimage.t * unit m button_actions) list m
end

(* Given a list of buttons and target width and height, fit these buttons to the screen.
  To deal with their effects, functions must be provided to react when these buttons are
  pressed or released, or to toggle their state. *)
module Buttons (I : Interface.T) :
  (ButtonInputs with type 'a m = 'a I.m) ->
  sig
    (* This is just a token to be chained.  Every action is dealt within the button actions. *)
    val u : unit I.m
  end

(* Similar to Buttons, but forcing exactly one button to be pressed at the same time.
  The initial pressed button is the first of the list, and its associated on_press event is
  triggered right away (with false). *)
module SelectButtons (I : Interface.T) :
  (ButtonInputs with type 'a m = 'a I.m) ->
  sig
    (* Again, just a token *)
    val u : unit I.m
  end

