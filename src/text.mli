
module Make (M : Libsave.Monad.T) : sig

  (* Render a chain of character provided a maximum width. *)
  val render : string -> int -> Subimage.t M.m

end

