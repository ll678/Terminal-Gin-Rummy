exception Malformed

type p = {
  name : string;
  hand : Deck.t;
  score : int;
}

type t = {
  stock_pile : Deck.t;
  discard_pile : Deck.t;
  players : (p * p);
  current_player : int;
  last_move: (Command.command * Deck.card) option;
}

type result = Legal of t | Illegal | Null of t


let init_players starting_cards starting_scores names = 
  {
    name = fst names;
    hand = List.nth starting_cards 3 ;
    score = fst starting_scores;
  },{
    name = snd names;
    hand = List.nth starting_cards 4 ;
    score = snd starting_scores;
  }

let init_state players_starting_scores start_player names starting_cards = 
  {
    stock_pile = List.nth starting_cards 1;
    discard_pile = List.nth starting_cards 2;
    players = init_players starting_cards players_starting_scores names;
    current_player = start_player;
    last_move = None;
  }

let get_stock st =
  st.stock_pile

let get_discard st =
  st.discard_pile

(*Note: used below*)
let get_current_player st = 
  st.current_player

let get_current_player_string st = 
  if (st.current_player=0) then (fst (st.players)).name else (snd (st.players)).name

let get_current_player_hand st = 
  if (st.current_player=0) then (fst (st.players)).hand else (snd (st.players)).hand

let get_players st = 
  st.players

let get_last_move st =
  st.last_move

let remove_top_card deck =
  match deck with
  | [] -> None
  | h::t -> Some t

let get_top_card deck =
  match deck with
  | [] -> None
  | h::t -> h

let update_player player st =
  if (st.current_player == 0) then 
    let player_hand = card::((fst st.p).hand) in 
    {
      name = (fst st.players).name;
      hand = player_hand;
      score = value_of_hand player_hand
    }
  else 
    let player_hand = card::((snd st.p).hand) in 
    {
      name = (snd st.players).name;
      hand = player_hand;
      score = value_of_hand player_hand
    }

(* TODO: draw should return a Null of t result if < 2 cards in stock *)

let get_new_draw_state st deck location =
  let current_stock  = current_stock_pile st in
  let current_discard = get_discard_pile st in
  let current_player = get_current_player st in
  let card = if (location = "Stock") then get_top_card current_stock else get_top_card current_discard in
  {
    stock_pile = if (location="Stock") then (remove_top_card current_stock) 
      else current_stock;
    discard_pile = if (location="Discard") then (remove_top_card current_discard)
      else current_discard;
    players = update_player player st;
    current_player = if (current_player = 0) then 1 else 0;
    last_move = (Draw location,card);
  }

let draw_deck location deck st =
  if (List.mem location ["Stock","Discard"]) then  
    (let new_st = get_new_draw_state st deck location
     in
     Legal new_st) 
  else Illegal

(** [discard_player card player] is [player] but with [card] removed.
    Precondition: [card] is in [player.hand].
*)
let discard_player card player =
  {
    name = player.name;
    hand = Deck.remove card player.hand;
    score = player.score;
  }

let discard card st = 
  if st.last_move = Some (Draw ["discard"],card) then Illegal
  else
    let p_ind = st.current_player in
    let p = List.nth st.players p_ind in
    if not (Deck.mem card p.hand) then Illegal
    else
      let opp_ind = (p_ind + 1) mod 2 in
      let opp = List.nth st.players opp_ind in
      Legal ({
          stock_pile = st.stock_pile;
          discard_pile = Deck.push card st.discard_pile;
          players =
            if p_ind = 0 then [discard_player card p;opp]
            else [opp;discard_player card p];
          current_player = opp_ind;
          dealer = st.dealer;
          last_move = Some (Discard ["discard"], card);
        })

let sort_player_hand current_player players =
  if (current_player = 0) then 
    let player_hand = Deck.suit_sort ((fst players).hand) in 
    let new_player = {
      name = (fst players).name;
      hand = player_hand;
      score = (fst players).score
    } in
    ( new_player, snd players)
  else 
    let player_hand = Deck.suit_sort ((snd players).hand) in 
    let new_player = {
      name = (snd players).name;
      hand = player_hand;
      score = (snd players).score
    } in
    (fst players, new_player)

(** Sort does not and should not update last_move even though it returns a different state. *)
let sort st =
  {
    stock_pile = st.stock_pile;
    discard_pile = st.discard_pile;
    players = sort_player_hand st.current_player st.players;
    current_player = st.current_player;
    last_move = st.last_move;
  }

let knock_declare st = 
  (* 1. Determine whether curr_p can knock. *)
  match st.last_move with
  | Some (Discard _,_) | Some (Pass,_) -> Illegal
  | _ ->
    let knocker = List.nth st.players st.current_player in
    if Deck.deadwood_value knocker.hand > 10 then Illegal
    else Legal ({
        stock_pile = st.stock_pile;
        discard_pile = st.discard_pile;
        players = st.players;
        current_player = (st.current_player + 1) mod 2;
        dealer = st.dealer;
        last_move = st.last_move;
      })

let knock_match deck st = 
  failwith "todo"