type p = {
  name : string;
  hand : Deck.t;
  score : int;
}

(** The abstract type of values representing a move. *)
type move = (Command.command * Deck.card option) option

type t = {
  stock_pile : Deck.t;
  discard_pile : Deck.t;
  (* fst players is player 0, snd players is player 1 *)
  players : (p * p);
  current_player : int;
  (* fst last_moves is the last move, snd last_moves is move before last move *)
  last_moves: (move * move);
}

type result = Legal of t | Illegal | Null of t | Win of t

let init_players starting_cards starting_scores names = (
  {
    name = fst names;
    hand = List.nth starting_cards 2;
    score = fst starting_scores;
  },{
    name = snd names;
    hand = List.nth starting_cards 3;
    score = snd starting_scores;
  })

let init_state players_starting_scores start_player names = 
  let starting_cards = Deck.start_cards in
  {
    stock_pile = List.nth starting_cards 0;
    discard_pile = List.nth starting_cards 1;
    players = init_players starting_cards players_starting_scores names;
    current_player = start_player;
    last_moves = (None, None);
  }

let get_stock st =
  st.stock_pile

let get_discard st =
  st.discard_pile

(*Note: used below*)
let get_current_player st = 
  st.current_player

let get_current_player_name st = if st.current_player = 0
  then (fst (st.players)).name else (snd (st.players)).name

let get_opponent_player_name st = if st.current_player = 0
  then (snd (st.players)).name else (fst (st.players)).name

let get_current_player_hand st = if st.current_player = 0
  then (fst (st.players)).hand else (snd (st.players)).hand

let get_current_player_score st = if st.current_player = 0
  then (fst (st.players)).score else (snd (st.players)).score

let get_opponent_player_score st = if st.current_player = 0
  then (snd (st.players)).score else (fst (st.players)).score

let get_moves st = 
  st.last_moves

let update_player st card =
  if (st.current_player == 0) then 
    let player_hand = Deck.push card ((fst st.players).hand) in 
    let tmp = 
      {
        name = (fst st.players).name;
        hand = player_hand;
        score = (fst st.players).score
      }
    in (tmp, snd st.players)
  else 
    let player_hand = Deck.push card ((snd st.players).hand) in 
    let tmp =
      {
        name = (snd st.players).name;
        hand = player_hand;
        score = (snd st.players).score
      } in (fst st.players, tmp)

let get_new_draw_state st location =
  let current_stock  = get_stock st in
  let current_discard = get_discard st in
  let card = if (location = "Stock") then Deck.hd current_stock else Deck.hd current_discard in
  {
    stock_pile = if (location="Stock") then (Deck.tl current_stock) 
      else current_stock;
    discard_pile = if (location="Discard") then (Deck.tl current_discard)
      else current_discard;
    players = update_player st card;
    current_player = st.current_player;
    last_moves = (Some (Draw [location], Some card),fst st.last_moves);
  }

let draw location st =
  if (List.mem location ["Stock"; "Discard"]) then
    if location = "Discard" && (Deck.length st.discard_pile) = 0 then Illegal else
      (let new_st = get_new_draw_state st location in
       if (Deck.length new_st.stock_pile) <= 2
       then Null (init_state ((fst st.players).score,(snd st.players).score) 0
                    ((fst st.players).name,(snd st.players).name))
       else match (fst st.last_moves) with
         | None -> Legal new_st
         | Some (Draw _,_) -> Illegal
         | _ -> Legal new_st) 
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
  if fst st.last_moves = Some (Draw ["Discard"], Some card) then Illegal
  else match fst st.last_moves with
    | Some (Draw _, _) ->
      let p_ind = st.current_player in
      let p = if p_ind = 0 then fst st.players else snd st.players in
      if not (Deck.mem card p.hand) then Illegal
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
            last_moves = (Some (Discard ["discard"], Some card),fst st.last_moves);
          })
    | _ -> Illegal

let knock_declare st = 
  (* 1. Determine whether curr_p can knock. *)
  match fst st.last_moves with
  | Some (Discard _,_) | Some (Pass,_) -> Illegal
  | _ ->
    let knocker = if st.current_player = 0
      then fst st.players else snd st.players in
    if Deck.deadwood_value knocker.hand > 10 then Illegal
    else Legal ({
        stock_pile = st.stock_pile;
        discard_pile = st.discard_pile;
        players = st.players;
        current_player = (st.current_player + 1) mod 2;
        last_moves = st.last_moves;
      })

let knock_match match_deck st = 
  let m_ind = st.current_player in
  (* 2. Determine whether match_deck is valid *)
  let k,m = if m_ind = 0
    then snd st.players, fst st.players
    else fst st.players, snd st.players in
  let k_new_hand = Deck.push_deck match_deck k.hand in
  let match_dead = 
    Deck.intersect (Deck.deadwood k_new_hand) match_deck in
  if not (Deck.is_empty match_dead) then Illegal else
    (* 3. Calculate scores *)
    let k_ind = (m_ind + 1) mod 2 in
    let m_new_hand = Deck.remove_deck match_deck m.hand in
    let p0_deadwood_val,p1_deadwood_val = if m_ind = 0
      then Deck.deadwood_value m_new_hand,Deck.deadwood_value k_new_hand
      else Deck.deadwood_value k_new_hand,Deck.deadwood_value m_new_hand
    in
    let names = ((fst st.players).name,(snd st.players).name) in
    let p0_score_orig,p1_score_orig = (fst st.players).score,(snd st.players).score in
    let deadwood_diff = p0_deadwood_val - p1_deadwood_val in
    if deadwood_diff < 0 then (
      if k_ind = 0 then
        let p0_score = p0_score_orig+(-deadwood_diff) in
        let next_st = init_state (p0_score_orig+(-deadwood_diff),p1_score_orig) 0 names in
        if p0_score > 100 then Win next_st
        else Legal next_st
      else
        let p0_score = p0_score_orig+(-deadwood_diff)+10 in
        let next_st = init_state (p0_score_orig+(-deadwood_diff)+10,p1_score_orig) 0 names in
        if p0_score > 100 then Win next_st
        else Legal next_st
    ) else (
      if k_ind = 0 then
        let p1_score = p0_score_orig+deadwood_diff+10 in
        let next_st = init_state (p0_score_orig,p1_score_orig+(-deadwood_diff)+10) 1 names in
        if p1_score > 100 then Win next_st
        else Legal next_st
      else
        let p1_score = p0_score_orig+deadwood_diff+10 in
        let next_st = init_state (p0_score_orig,p1_score_orig+(-deadwood_diff)) 1 names in
        if p1_score > 100 then Win next_st
        else Legal next_st
    )

let sort_player_hand current_player players =
  if (current_player = 0) then 
    let sorted_hand = Deck.suit_sort ((fst players).hand) in 
    let sorted_player = {
      name = (fst players).name;
      hand = sorted_hand;
      score = (fst players).score
    } in
    (sorted_player, snd players)
  else 
    let sorted_hand = Deck.suit_sort ((snd players).hand) in 
    let sorted_player = {
      name = (snd players).name;
      hand = sorted_hand;
      score = (snd players).score
    } in
    (fst players, sorted_player)

(** [Sort st], sorts the cards of the current player. 
    Sort does not and should not update last_move even though it returns a 
    different state. *)
let sort st =
  Legal 
    {
      stock_pile = st.stock_pile;
      discard_pile = st.discard_pile;
      players = sort_player_hand st.current_player st.players;
      current_player = st.current_player;
      last_moves = st.last_moves;
    }

(* (Command.command * Deck.card option) option *)

let pass_valid st = 
  match st.last_moves with
  | (None, None) -> true
  | ((Some p), None) -> if (fst p) = Pass then true else false
  | _ -> false

let pass st =
  if pass_valid st then
    Legal
      {
        stock_pile = st.stock_pile;
        discard_pile = st.discard_pile;
        players = st.players;
        current_player = if (st.current_player = 0) then 1 else 0;
        last_moves =  (Some (Pass , None),fst st.last_moves);
      }
  else Illegal
