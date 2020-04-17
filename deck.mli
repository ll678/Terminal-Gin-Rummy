(** 
   Representation of static game data.

   This module represents the data stored in the deck of cards and handles
   functions that allow other modules to access card information.
*)

(** The type of card suits. *)
type suit = Club | Diamond | Heart | Spade

(** The type of card numbers. *)
type number

(** The type of cards. *)
type card = number * suit

val init_deck : card list