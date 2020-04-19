(** 
   Representation of the game players.

   This module represents the two players playing a game and inlcudes functions
   that change the hands and score of either player.
*)
type player

type t

val remove_card : int -> Deck.card -> t