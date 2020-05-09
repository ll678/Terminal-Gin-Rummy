(** 
   Representation of dynamic game state.

   This module represents the state of a game as it is being played,
   including each player's hand and score, the cards in the discard pile, 
   and the cards remaining in the deck.
*)

(** The abstract type of players. *)
type p

(** The abstract type of representing a move in the game. *)
type move

(** The abstract type of values representing the game state. *)
type t

(** The result of a new game state. *)
type result =
  | Legal of t
  | Illegal of string
  | Null of t
  | RoundEnd of t * Deck.t * Deck.t * int

(** [init_state players_starting_scores start_player names] is the initial state
    of a round. In that state the player [start_player] starts. Players have the
    names [names] and the scores [players_starting_scores], where the first 
    element in each tuple is for player 1 and the second element is for player
    2. *)
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
val get_moves : t -> (move * move)

(** [get_last_card_drawn st] is an option of the last card drawn. 
    If there wasn't a last card drawn then None *)
val get_last_card_drawn : t -> Deck.card option

(** [draw location st]is [r] if attempting to draw from [location] by the
    current player in [st] results in [r].
    [r] is [Null st'] if there are less than 2 cards in stock, where [st'] a
    new round with no score changes and [current_player] is 0.
    [r] is [Illegal str] if the draw location is not "stock" or "discard", or
    if a draw from stock is attempted on the first move, or if a draw is 
    attempted after a non-discard move.
    Otherwise, [r] is [Legal st'], where [st'] differs from [st] in that:
    - [card] is removed from [location]
    - [card] is added to the current player's hand
    - [last_moves] is updated
*)
val draw : string -> t -> result

val get_last_moves_type : t -> Command.command option

(** [discard card st] is [r] if attempting to discard [card] from the hand
    of the current player in [st] results in [r].
    [r] is [Illegal str] if [card] is the same card that was just drawn from
    the discard pile, or if a discard is attempted after any non-draw move, or
    if [card] is not in the current player's hand.
    Otherwise, [r] is [Legal st'], where [st'] differs from [st] in that:
    - [card] is removed from current player's hand
    - [card] is added to discard
    - [current_player] is switched
    - [last_moves] is updated
*)
val discard : Deck.card -> t -> result

(** [knock_declare st] is [r] if an attempt to knock by the current player in
    [st] results in [r].
    [r] is [Illegal str] if a knock is attempted after any non-draw move, 
    or if the current player does not have less than 10 in deadwood.
    [r] is [RoundEnd (st',winner_hand,loser_hand,round_score)] where [st'] is
    a new round with the winner's score updated by [round_score], and 
    [winner_hand] and [loser_hand] were the ending hands of the winner and loser
    respectively.
    Otherwise, [r] is [Legal st'], where [st'] differs from [st] in that:
    - [current_player] is switched
    - [last_moves] is updated
*)
val knock_declare : t -> result

(** [knock_match_declare st] is [r] if an attempt to match by the current player
    in [st] results in [r].
    [r] is [Illegal str] if a match is attempted after any non-knock move.
    Otherwise, [r] is [Legal st].
*)
val knock_match_declare : t -> result

(** [knock_match match_deck st] is [r] if an attempt by the current player in
    [st]to match the cards in [match_deck] with their opponent's melds results
    in [r].
    [r] is [Illegal str] if the cards in [match_deck] do not extend the
    opponent's melds.
    Otherwise, [r] is [RoundEnd (st',winner_hand,loser_hand,round_score)] where:
    - [st'] is a new round with the scores updated, starting from the winner
      of the previous round
    - [winner_hand] and [loser_hand] are the respective ending hands of the
      winner and loser from the previous round
    - [round_score] is the amount of points the winner gained
*)
val knock_match : Deck.t -> t -> result

(** [pass st] is [r] if an attempt to pass by the current player in
    [st] results in [r].
    [r] is [Illegal str] if the pass is attempted on a non-first move.
    Otherwise, [r] is [Legal st'], where [st'] differs from [st] in that:
    - [current_player] is switched
    - [last_moves] is updated
*)
val pass : t -> result

(** [sort st] is [Legal st'] where the current player's hand is sorted. *)
val sort : t -> result

(** [prompt_command st] is a string to prompt for the appropriate command
    based on [st]. *)
val prompt_command : t -> string