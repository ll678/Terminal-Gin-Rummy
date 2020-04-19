open Deck

exception Malformed

(* TODO: replace [unit] with a type of your own design. *)

type p = {
  hand: Deck.t;
  score: int;
}

type t = {
  stock_pile: Deck.t;
  discard_pile : Deck.t;
  players : p list;
  current_player : p;
  dealer : int;
  last_move: command;
  last_state: t;
}

type result = Legal of t | Illegal

let init_state (d:Deck.t) = {
  stock_pile = start_stock d;
  discard_pile = start_discard d;
  players = [];
  current_player = 0;
  dealer = 0;
  last_move = None;
  last_state = t;
}

let init_player (p:player) (d:deck) = {
  hand = start_player_hand d;
  score = 0;
}

let current_stock_pile st =
  st.stock_pile

let get_discard_pile st =
  st.discard_pile

let get_current_player st = 
  st.current_player

let get_players st = 
  st.players

let get_dealer st =
  st.dealer

let get_last_move st =
  st.last_move

let get_last_st st =
  st.last_state 

(* We need to decide if the discard pile is ordered (which we would want in this case to get the faceup card) *)
let remove_top_card deck =
  match deck with
  | [] -> None
  | h::t ->t

let get_top_card deck =
  match deck with
  | [] -> None
  | h::t ->h

let update_player player st =
  if (st.current_player == 0) then 
    let player_hand = card::((fst st.p).hand) in 
    {
      hand = player_hand;
      score = get_score hand
    }
  else 
    let player_hand = card::((snd st.p).hand) in 
    {
      hand = player_hand;
      score = get_score player_hand
    }

let get_new_draw_state st deck location =
  let current_stock  = current_stock_pile st in
  let current_discard = get_discard_pile st in
  let current_player = get_current_player st in
  let card = if (location == Stock) then get_top_card current_stock else get_top_card current_discard in
  {
    stock_pile = if (location==Stock) then (remove_top_card current_stock) 
      else current_stock;
    discard_pile = if (location==Discard) then (remove_top_card current_discard)
      else current_discard;
    players = update_player player st;
    current_player = if (current_player == 0) then 1 else 0;
    dealer = get_dealer st;
    last_move = Draw location;
    last_state = st;
  }


let draw_deck location deck st =
  let options = get_options deck in
  if (List.mem location options) then  (
    let new_st = get_new_draw_state st deck location
    in
    Legal new_st else Illegal


let discard card prev_st = 
  if prev_st.last_move = Draw "discard" and
  (List.hd prev_st.discard_pile) = card then Illegal else
  Legal ({
      (* todo: insert other parameters of state *)
      discard = card :: prev_st.discard;
      last_move = Discard (card);
      last_state = prev_st;
      players = Players.remove_card prev_st.current_player card
    })