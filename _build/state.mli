(** 
   Representation of dynamic game state.

   This module represents the state of a game as it is being played,
   including each player's hand and score, the cards in the discard pile, 
   and the cards remaining in the deck.
*)

(** The abstract type of players. *)
type p

(** The abstract type of values representing the game state. *)
type t 

(** The result of a new game state. *)
type result = Legal of t | Illegal

val init_state : int * int -> int -> t

(* val discard : Deck.card -> t -> t *)