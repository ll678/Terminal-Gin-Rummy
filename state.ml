type p = {
  name : string;
  hand : Deck.t;
  score : int;
}

type move = (Command.command * Deck.card option) option

type t = {
  stock_pile : Deck.t;
  discard_pile : Deck.t;
  players : (p * p);
  current_player : int;
  last_moves: (move * move);
}

type result =
  | Legal of t
  | Illegal of string
  | Null of t
  | RoundEnd of t * Deck.t * Deck.t * int

(** [init_players starting_cards starting_scores names] is a pair of [p]
    representing the two players of the game, with names [names], scores
    [starting_scores], and hands [starting_cards]. *)
let init_players starting_cards starting_scores names = (
  {
    name = fst names;
    hand = Deck.nth starting_cards 2;
    score = fst starting_scores;
  },{
    name = snd names;
    hand = Deck.nth starting_cards 3;
    score = snd starting_scores;
  })

let init_state players_starting_scores start_player names = 
  let starting_cards = Deck.start_cards (Deck.init_deck) in
  {
    stock_pile = Deck.nth starting_cards 0;
    discard_pile = Deck.nth starting_cards 1;
    players = init_players starting_cards players_starting_scores names;
    current_player = start_player;
    last_moves = (None, None);
  }

let get_stock st =
  st.stock_pile

let get_discard st =
  st.discard_pile

let get_current_player st = 
  st.current_player

let check_knock st =
  let last_move = fst st.last_moves in 
  match last_move with 
  | Some (Knock,a) -> true
  | _ -> false


(* Returns the type of the last move *)
let get_last_moves_type st  : Command.command option =

  match st.last_moves with 
  (* There have been no moves made *)
  | None,None -> None
  (* Someone has drawn a card *)
  |(Some (Draw x, z),_) -> (Some (Draw x))
  (* Someone has passed *)
  |(Some (Pass,_),None) -> Some Pass
  (* Someone has discarded *)
  |(Some (Discard x, b),a) -> Some (Discard x)
  (* Someone has knocked*)
  |(Some (Knock, b),a)-> Some (Knock)
  (* Someone has passed *)
  |(Some (Pass, b),a) -> Some (Pass)

  | _ -> failwith "Not sure how you got here"


let get_current_player_name st = if st.current_player = 0
  then (fst (st.players)).name else (snd (st.players)).name

let get_opponent_player_name st = if st.current_player = 0
  then (snd (st.players)).name else (fst (st.players)).name

let get_current_player_hand st = if st.current_player = 0
  then (fst (st.players)).hand else (snd (st.players)).hand

let get_opponent_player_hand st = if st.current_player = 0
  then (snd (st.players)).hand else (fst (st.players)).hand

let get_current_player_score st = if st.current_player = 0
  then (fst (st.players)).score else (snd (st.players)).score

let get_opponent_player_score st = if st.current_player = 0
  then (snd (st.players)).score else (fst (st.players)).score

let get_moves st = 
  st.last_moves

let get_last_card_drawn st =
  let last_move = fst st.last_moves in 
  match last_move with 
  | Some (Draw a, Some b) -> Some b
  | _ ->  None

(* (Command.command * Deck.card option) option *)


(** [update_player_draw st card] is the pair of players after the current
    player in [st] adds [card] to their hand. *)
let update_player_draw st card =
  let drawer = if (st.current_player = 0) then fst else snd in
  let p_updated = 
    {
      name = (drawer st.players).name;
      hand = Deck.push card ((drawer st.players).hand);
      score = (drawer st.players).score
    }
  in if (st.current_player = 0)
  then (p_updated, snd st.players) else (fst st.players, p_updated)

(** [draw_state_next st location] is the state resulting from a draw from pile
    [location] starting from state [st].
    Precondition: [location] is either "stock" or "discard". *)
let draw_state_next st location =
  let current_stock  = get_stock st in
  let current_discard = get_discard st in
  let card = if (location = "stock")
    then Deck.hd current_stock else Deck.hd current_discard in
  {
    stock_pile = if (location = "stock")
      then (Deck.tl current_stock) else current_stock;
    discard_pile = if (location = "discard")
      then (Deck.tl current_discard) else current_discard;
    players = update_player_draw st card;
    current_player = st.current_player;
    last_moves = (Some (Draw [location], Some card), fst st.last_moves);
  }

let draw location st =
  if not (List.mem location ["stock"; "discard"]) then
    Illegal "You can only draw from \"stock\" or \"discard\". Try again."
  else match st.last_moves,location with
    | (None,None),"discard" | (Some (Pass,_),None),"discard"
    | (Some (Discard _,_),_),_ | (Some (Pass,_),Some (Pass,_)),_-> 
      if location = "discard" && (Deck.length st.discard_pile) = 0 then
        Illegal "The discard pile is empty. Try again."
      else
        (let new_st = draw_state_next st location in
         if (Deck.length new_st.stock_pile) <= 2
         then Null (init_state ((fst st.players).score, (snd st.players).score)
                      0 ((fst st.players).name, (snd st.players).name))
         else Legal new_st)
    | (None,None),"stock" | (Some (Pass,_),None),"stock" ->
      Illegal "You cannot draw from stock now; \
               type \"draw discard\" or \"pass\"."
    | (Some (Knock,_),_),_ ->
      Illegal "You cannot draw now; type \"match\" or \"help\"."
    | _ -> Illegal "You cannot draw now."

(** [discard_player card player] is [player] but with [card] removed.
    Precondition: [card] is in [player.hand]. *)
let discard_player card player =
  {
    name = player.name;
    hand = Deck.remove card player.hand;
    score = player.score;
  }

let discard card st = 
  if fst st.last_moves = Some (Draw ["discard"], Some card) then 
    Illegal "You cannot discard the card you just drew \
             from the discard pile. Try another one."
  else 
    match fst st.last_moves with
    | Some (Draw _, _) ->
      let p_ind = st.current_player in
      let p = if p_ind = 0 then fst st.players else snd st.players in
      if not (Deck.mem card p.hand) then
        Illegal "You do not have this card. Try again."
      else
        let opp_ind = (p_ind + 1) mod 2 in
        let opp = if opp_ind = 0 then fst st.players else snd st.players in
        Legal ({
            stock_pile = st.stock_pile;
            discard_pile = Deck.push card st.discard_pile;
            players =
              if p_ind = 0 then (discard_player card p, opp)
              else (opp, discard_player card p);
            current_player = opp_ind;
            last_moves =
              (Some (Discard ["discard"], Some card),fst st.last_moves);
          })
    | Some (Knock,_) ->
      Illegal "You cannot discard now; type \"match\" or \"help\"."
    | _ -> Illegal "You cannot discard now."

let knock_declare st = 
  match fst st.last_moves with
  | Some (Draw _,_) ->
    let knocker = if st.current_player = 0
      then fst st.players else snd st.players in
    let k_deadwood = Deck.knock_deadwood_value knocker.hand in
    if k_deadwood > 10 then Illegal "You do not have less than 10 deadwood."
    else if Deck.deadwood_value knocker.hand = 0 then 
      let p0,p1 = st.players in
      let k_ind = st.current_player in
      let names = (p0.name,p1.name) in
      let matcher = if k_ind = 0 then p1 else p0 in
      let m_deadwood = Deck.deadwood_value matcher.hand in
      let round_score = m_deadwood + 20 in
      if k_ind = 0 then
        let p0_score = p0.score + round_score in
        let next_st = init_state (p0_score,p1.score) 0 names in
        RoundEnd (next_st,p0.hand,p1.hand,round_score)
      else
        let p1_score = p1.score + round_score in
        let next_st = init_state (p0.score,p1_score) 1 names in
        RoundEnd (next_st,p1.hand,p0.hand,round_score)
    else if k_deadwood = 0 then 
      let p0,p1 = st.players in
      let k_ind = st.current_player in
      let names = (p0.name,p1.name) in
      let matcher = if k_ind = 0 then p1 else p0 in
      let m_deadwood = Deck.deadwood_value matcher.hand in
      let round_score = m_deadwood + 20 in
      let k_hand = if st.current_player = 0 then p0.hand else p1.hand in
      let k_max_deadwood = Deck.max_deadwood_card k_hand in
      let k_hand_removed = Deck.remove k_max_deadwood k_hand in
      if k_ind = 0 then
        let p0_score = p0.score + round_score in
        let next_st = init_state (p0_score,p1.score) 0 names in
        RoundEnd (next_st,k_hand_removed,p1.hand,round_score)
      else
        let p1_score = p1.score + round_score in
        let next_st = init_state (p0.score,p1_score) 1 names in
        RoundEnd (next_st,k_hand_removed,p0.hand,round_score)
    else 
      let opp = if (st.current_player + 1) mod 2 = 0 then fst st.players 
        else snd st.players in
      let card = Deck.max_deadwood_card knocker.hand in
      Legal ({
          stock_pile = st.stock_pile;
          discard_pile = st.discard_pile;
          players =               
            if st.current_player = 0 then (discard_player card knocker, opp)
            else (opp, discard_player card knocker);
          current_player = (st.current_player + 1) mod 2;
          last_moves = (Some (Knock, None),fst st.last_moves);
        })
  | Some (Knock,_) ->
    Illegal "You cannot knock now; type \"match\" or \"help\"."
  | _ -> Illegal "You cannot knock now; try again after a draw."

let knock_match_declare st = 
  match fst st.last_moves with
  | Some (Knock,None) -> Legal st
  | _ ->
    Illegal "You cannot match now; try again after your opponent has knocked."

(** [knock_match_result st p0_new_hand p1_new_hand deadwood_diff] is [r] where
    [r] is [RoundEnd (st',winner_hand,loser_hand,round_score)], where:
    - [st'] is a new round with the scores updated, starting from the winner
      of the previous round
    - [winner_hand] and [loser_hand] are the respective ending hands of the
      winner and loser from the previous round
    - [round_score] is the amount of points the winner gained
*)
let knock_match_result st p0_new_hand p1_new_hand deadwood_diff = 
  let k_ind = (st.current_player + 1) mod 2 in
  let names = ((fst st.players).name,(snd st.players).name) in
  let p0_score_orig,p1_score_orig =
    (fst st.players).score,(snd st.players).score in
  if deadwood_diff < 0 then (
    if k_ind = 0 then
      let round_score = (-deadwood_diff) in
      let p0_score = p0_score_orig+round_score in
      let next_st = init_state (p0_score,p1_score_orig) 0 names in
      RoundEnd (next_st,p0_new_hand,p1_new_hand,round_score)
    else
      let round_score = (-deadwood_diff)+10 in
      let p0_score = p0_score_orig+round_score in
      let next_st = init_state (p0_score,p1_score_orig) 0 names in
      RoundEnd (next_st,p0_new_hand,p1_new_hand,round_score)
  ) else if deadwood_diff > 0 then (
    if k_ind = 0 then
      let round_score = deadwood_diff+10 in
      let p1_score = p1_score_orig+round_score in
      let next_st = init_state (p0_score_orig,p1_score) 1 names in
      RoundEnd (next_st,p1_new_hand,p0_new_hand,round_score)
    else
      let round_score = deadwood_diff in
      let p1_score = p1_score_orig+round_score in
      let next_st = init_state (p0_score_orig,p1_score) 1 names in
      RoundEnd (next_st,p1_new_hand,p0_new_hand,round_score)
  ) else (
    if k_ind = 0 then
      let round_score = deadwood_diff+10 in
      let p1_score = p1_score_orig+round_score in
      let next_st = init_state (p0_score_orig,p1_score) 1 names in
      RoundEnd (next_st,p1_new_hand,p0_new_hand,round_score)
    else
      let round_score = deadwood_diff+10 in
      let p0_score = p0_score_orig+round_score in
      let next_st = init_state (p0_score_orig,p0_score) 0 names in
      RoundEnd (next_st,p0_new_hand,p1_new_hand,round_score)
  )

let knock_match match_deck st = 
  let m_ind = st.current_player in
  let k,m = if m_ind = 0
    then snd st.players, fst st.players
    else fst st.players, snd st.players in
  let k_orig_hand = k.hand in
  let k_melds = Deck.best_meld k_orig_hand in 
  if not (Deck.valid_match match_deck k_melds) then
    Illegal "Not all these cards form melds. Try again."
  else
    let k_new_hand = Deck.push_deck match_deck k_orig_hand in
    let m_new_hand = Deck.remove_deck match_deck m.hand in
    let p0_new_hand, p1_new_hand = 
      if m_ind = 0 then m_new_hand,k_new_hand else k_new_hand,m_new_hand in
    let deadwood_diff =
      Deck.deadwood_value p0_new_hand - Deck.deadwood_value p1_new_hand in
    knock_match_result st p0_new_hand p1_new_hand deadwood_diff

let pass st =
  match st.last_moves with
  | (None,None) | (Some (Pass,_),None) ->
    Legal
      {
        stock_pile = st.stock_pile;
        discard_pile = st.discard_pile;
        players = st.players;
        current_player = if (st.current_player = 0) then 1 else 0;
        last_moves =  (Some (Pass,None),fst st.last_moves);
      }
  | _ -> Illegal "You can only pass in the beginning of the round."

(** [sort_player_hand current_player players] is [players] but with the hand
    of [current_player] sorted. *)
let sort_player_hand current_player players =
  let sorter = if (current_player = 0) then fst else snd in
  let p_sorted =
    {
      name = (sorter players).name;
      hand = Deck.suit_sort ((sorter players).hand);
      score = (sorter players).score
    }
  in if (current_player = 0)
  then (p_sorted, snd players) else (fst players, p_sorted)

let sort st =
  Legal 
    {
      stock_pile = st.stock_pile;
      discard_pile = st.discard_pile;
      players = sort_player_hand st.current_player st.players;
      current_player = st.current_player;
      last_moves = st.last_moves;
    }

let prompt_command st =
  match (get_moves st) with
  | (None, None) | (Some (Pass, _), None) -> 
    "You can either draw from the discard pile or pass your turn." 
  | Some (Draw _, _), _ -> 
    (match (knock_declare st) with 
     | Illegal _ -> "It is your turn to discard."
     | Legal _ -> "You can either discard or knock."
     | RoundEnd _ -> "You can call gin."
     | Null _ -> failwith "prompt_command fail: null")
  | Some (Discard _, _), _ | Some (Pass, _ ), _ -> "It is your turn to draw."
  | Some (Knock, _), _ -> "Please type \"match\" to begin laying off cards."
  | _ -> "Please enter a command."