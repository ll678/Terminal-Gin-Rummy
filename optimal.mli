


(** The abstract type of representing an optimal move in the game. *)
type move = Discard of Deck.card | Draw of string | Knock 



(** [get_optimal st] is [move] ...*)
val get_optimal : State.t -> move