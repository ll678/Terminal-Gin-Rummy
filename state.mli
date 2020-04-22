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
type result = Legal of t | Illegal | Null of t

val init_state : int * int -> int -> (string * string) -> t

val get_stock : t -> Deck.t

val get_discard: t -> Deck.t

val get_current_player : t -> int

(** [get_current_player_hand st] is the hand of the current player. *)
val get_current_player_hand : t -> Deck.t

val get_players : t -> p list

val get_last_move : t -> (Command.command * Deck.card) option

(* val remove_top_card : 'a list -> 'a list

   val get_top_card : 'a list -> 'a

   val update_player : p -> t -> p *)

(** [discard card st] is [r] if attempting to discard [card] from the hand
    of the current player in [st] results in [r]. If [card] can be discarded,
    [r] is [Legal st'], where in [st'] the card is now located in the discard.
    Otherwise, the result is [Illegal].
    - removes [card] from current_player's hand
    - adds [card] to discard
    - switches [st.current_player]
    - updates [st.last_move]
*)
val discard : Deck.card -> t -> result

(** [knock_declare st] is [r] if an attempt to knock by the current player in [st]
    results in [r]. If the current player has less than 10 in deadwood,
    [r] is [Legal st']. Otherwise, the result is [Illegal]. 
    - Switches current player.
    - This function is mainly for checking legality of Knock
*)
val knock_declare : t -> result

(** [knock_match st] is [r] if an attempt by the current player in [st] to
    match their selected deadwood with opponent's cards results in [r]. It is
    the reinitialized game for the next round. If the current player lists cards
    that all form melds, [r] is [Legal st'].
    Otherwise, the result is [Illegal]. 
    - Determine legality of input deck.
    - Calculate value of deadwood for knocker pre-matching, value of deadwood
      for matcher post-matching
    - Determine winner and calculate scores accordingly
    - Initialize new state
*)
val knock_match : Deck.t -> t -> result