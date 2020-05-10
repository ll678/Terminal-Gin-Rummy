

(** The abstract type of representing an optimal move in the game. *)
type move = Discard of Deck.card | Draw of string | Knock | Cards of Deck.t | Match | Pass


(** [get_optimal st] is an optimal move [move]. Specifically
    if the player is able to draw, [move] is Draw of string.
    If the player is able to discard, [move] is Discard of Deck.card.
    If the player is able knock, [move] is Knock.
    If the player is able to match cards, [move] is Cards of Deck.t 
    If the player is able to match, [move] is Match
    If the player is able to pass, [move] is Pass*)
val get_optimal : State.t -> move