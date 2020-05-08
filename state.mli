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

(** The abstract type of representing a move in the game. *)
type move

(** The result of a new game state. *)
type result = Legal of t | Illegal of string | Null of t | RoundEnd of t * Deck.t * Deck.t * int


(** [init_state s p n] is the initial state of the game when playing Terminal 
    Gin Runnmy. In that state the player p starts. Players have the names n and
     the scores s. *)
val init_state : (int * int) -> int -> (string * string) -> t

(** [get_stock st] is the current stock pile for the state st. *)
val get_stock : t -> Deck.t

(** [get_discard st] is the current discard pile for the state st. *)
val get_discard: t -> Deck.t

(** [get_curreent_player st] is the current player for the state st. *)
val get_current_player : t -> int

(** [get_current_player_name st] is the name of the current player. *)
val get_current_player_name : t -> string

(** [get_opponent_player_name st] is the name of the current player. *)
val get_opponent_player_name : t -> string

(** [get_current_player_hand st] is the hand of the current player. *)
val get_current_player_hand : t -> Deck.t

(** [get_opponent_player_hand st] is the hand of the opponent player. *)
val get_opponent_player_hand : t -> Deck.t

(** [get_current_player_score st] is the score of the current player. *)
val get_current_player_score : t -> int

(** [get_opponent_player_score st] is the score of the current player. *)
val get_opponent_player_score : t -> int

(** [get_moves st] is the last two moves of the current player. *)
val get_moves : t -> (move*move)

(** [draw location st] is [Illegal] if stock pile<2. It is illegal if draw 
    location is not "Stock" or "Discard". Otherwise result is [Legal st'], where
    in [st'] is different from [st] in that it:
    - removes [card] from location
    - adds [card] to players pile
    - switches [st.current_player]
    - updates [st.last_move]
*)
val draw : string -> t -> result


(** [sort st] is [r] Legal of [st'] where the current player's deck is sorted *)
val sort : t -> result

val get_last_moves_type : t -> Command.command option

(** [pass st] is [Illegal] if one of the last moves isn't None otherwise
    Legal of [st'] where the current player's is swapped *)
val pass : t -> result

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

(** [knock_declare st] is [(r,winner's deadwood, loser's deadwood, winner's
    deadwood value, loser's deadwood value, winner's points gained)] if an
    attempt to knock by the current player in [st]
    results in [r]. If the current player has less than 10 in deadwood,
    [r] is [Legal st']. Otherwise, the result is [Illegal]. 
    - Switches current player.
    - This function is mainly for checking legality of Knock
    - 
*)
(* val knock_declare : t -> (result * Deck.t * Deck.t * int) *)
val knock_declare : t -> result


val knock_match_declare : t -> result


(** [knock_match st] is [r] if an attempt by the current player in [st] to
    match their selected deadwood with opponent's cards results in [r]. It is
    the reinitialized game for the next round. If the current player lists cards
    that all form melds, [r] is [Legal st'].
    Otherwise, the result is [Illegal]. 
    - Determine legality of input deck.
    - Calculate value of deadwood for players
    - Determine winner and calculate scores accordingly
    - Initialize new state
*)
val knock_match : Deck.t -> t -> result

(** [prompt_command st] returns a string to prompt for the appropriate command
    based on the current [st]. *)
val prompt_command : t -> string