(** 
   Representation of static game data.

   This module represents the data stored in the deck of cards and handles
   functions that allow other modules to access card information.
*)

(** The type of card suits. *)
type suit

(** The type of card numbers. *)
type number

(** The type of cards. *)
type card = number * suit

(** The abstract type of values representing card decks. *)
type t

(** [init_deck] is the initialized and sorted standard 52-card deck. *)
val init_deck : t

(** [shuffle deck] randomizes the order of [deck]. *)
val shuffle : t -> t

(** [suit_sort hand] sorts [hand] by suit first, then number. *)
val suit_sort : t -> t

(** [intersect l1 l2] contains only the elements that are elements of [l1]
    and elements of [l2]. *)
val intersect : t -> t -> t

(** [best_meld hand] returns a list of the meld combinations of [hand] that 
    results in the lowest deadwood value. *)
val best_meld : t -> t list

(** [deadwood hand] returns a list of the deadwood cards corresponding to
    the best meld. *)
val deadwood : t -> t

(** [deadwood_value hand] is the total int value of the deadwood in [hand]. *)
val deadwood_value : t -> int

(** [meld_value hand] is the total int value of the meld in [hand]. *)
val meld_value : t -> int

(** [start_cards] creates an initialized and shuffled deck and returns a list
    that contains a list of stock pile cards, a list of discard pile cards, 
    and each player's starting hand. *)
val start_cards : t list

(** [push card deck] pushes [card] onto the top of [deck]. *)
val push : card -> t -> t

(** [push_deck from_deck to_deck] pushes [from_deck] onto the top of [to_deck].
*)
val push_deck : t -> t -> t

(** [mem card deck] is true when [card] is in [deck], else false. *)
val mem : card -> t -> bool

(** [is_empty deck] is true when [deck] is empty, else false. *)
val is_empty : t -> bool

(** [remove card deck] is the deck without [card] in [deck].
    Throws: Failure "remove failure: card not in deck." if [card] not in [deck].
*)
val remove : card -> t -> t

(** [remove_deck rm_deck deck] is the deck without the cards in [rm_list].
    Throws: Failure "remove failure: card not in deck." if [card] not in [deck].
*)
val remove_deck : t -> t -> t

(** [string_of_card card] is the string representation of [card]. *)
val string_of_card : card -> string

(** [string_of_card_short card] is a compact string representation of [card]. *)
val string_of_card_short : card -> string

(** [string_of_deck deck] is the list of stringified cards in [deck]. *)
val string_of_deck : t -> string list