
(* Functors to organise the screen. *)

(* This functor splits the screen into two independent interfaces.
  Both initialisations should happen before any other operation. *)
module SplitVertical : Interface.T -> sig
    module Up : Interface.T
    module Down : Interface.T
  end


type button_actions = {
  on_press : bool -> unit (* Called when a button is pressed.  The boolean states whether it was triggerred by the user or by internal functions. *) ;
  on_release : bool -> unit (* Called when a button is released. *) ;
  set_toggle : (bool -> unit) -> unit (* Called once with a function to set a button (true then means pressed).  Calling the provided function will call on_press or on_release with false. *)
}

(* Given a list of buttons and target width and height, divide the
  screen such to fit these buttons.
  To deal with their effects, functions must be provided to react
  when these buttons are pressed or released, or to toggle their
  state. *)
module Buttons : sig
    val width : int
    val height : int
    val buttons : (Subimage.t * button_actions) list
  end -> Interface.T -> sig (* Empty, as everything is dealt with the button actions. *) end

(* Similar to Buttons, but forcing exactly one button to be pressed at the same time.
  The initial pressed button is the first of the list, and its associated on_press event is
  triggered right away (with false). *)
module SelectButtons : sig
    val width : int
    val height : int
    val buttons : (Subimage.t * button_actions) list
  end -> Interface.T -> sig end

