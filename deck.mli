(** 
   Representation of static game data.

   This module represents the data stored in the deck of cards and handles
   functions that allow other modules to access card information.
*)

(** The type of card suits. *)
type suit

(** The type of card ranks. *)
type rank

(** The type of cards. *)
type card = rank * suit

(** The abstract type of values representing card decks. *)
type t

(** The exception of malformed cards. *)
exception Malformed

(** [init_deck] is the initialized and sorted standard 52-card deck. *)
val init_deck : t

(** [get_list n l] is the first n cards in the deck l *)
val get_list : int -> t -> t

(** [difference l1 l2] contains only the elements that are elements of [l1]
    but not elements of [l2]. *)
val difference : t -> t -> t

(** [shuffle deck] randomizes the order of [deck]. *)
val shuffle : t -> t

(** [suit_sort hand] sorts [hand] by suit first, then rank. *)
val suit_sort : t -> t

(** [suit_sort hand] is the score of [hand]. *)
val value_of_hand : t -> int

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

(** [knock_deadwood_value hand] is the int value of the deadwood in [hand]
    with the implied discard of the highest value card in the deadwood. *)
val knock_deadwood_value : t -> int

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

(** [nth deck idx] is the element in [deck] with index [idx]. *)
val nth : 'a list -> int -> 'a

(** [hd deck] is the first card in [deck]. *)
val hd : t -> card

(** [tl deck] is the tail of [deck]. *)
val tl : t -> t

(** [length deck] is the length of [deck]. *)
val length : t -> int

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

(** [string_of_deck_short deck] is the list of stringified cards in [deck]
    with shortened representations of suit and rank. *)
val string_of_deck_short : t -> string list

(** [card_of_string string] is the card of [string]. *)
val card_of_string : string -> card

(** [deck_of_string string] is deck of a string of cards [string]. *)
val deck_of_string : string -> t

(** [rankstring_of_string string] is the shortened string representation
    of the string card rank [string]. *) 
val rankstring_of_string : string -> string

(** [suitstring_of_string string] is the shortened string representation
    of the string card suit [string]. *) 
val suitstring_of_string : string -> string

(** [string_of_hd deck] is a string list of the hd of [deck]. If
    the deck is empty, the empty list is returned. *)
val string_of_hd : t -> string list

(** [test_empty_hand] is an empty hand of cards used strictly for testing. *)
val test_hand_empty : t

(** [test_hand] and [test_hand2] are hands of cards used strictly for 
    testing. *)
val test_hand : t

val test_hand2 : t

(** [test_hand3] is [test_hand2] with the six of spades removed, used
    strictly for testing. *)
val test_hand3 : t

(** [test_hand4] is [test_hand3] with the king of hearts pushed on top, used
    strictly for testing. *)
val test_hand4 : t

val get_worst : t -> card * int

(** [test_meld] and [test_meld2] are lists of the melds of [test_hand] and
    [test_hand2], respectively, used strictly for testing. *)
val test_meld : t list

val test_meld2 : t list

(** [sorted_test_hand2] is [test_hand2] sorted by suit first, then rank, 
    used strictly for testing. *) 
val sorted_test_hand2 : t

(** [test_deadwood] and [test_deadwood2] are lists of the deadwood of 
    [test_hand] and [test_hand2], respectively, used strictly for testing. *)
val test_deadwood : t

val test_deadwood2 : t