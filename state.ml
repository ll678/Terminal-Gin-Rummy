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

(* TODO: Illegal could be of string to display a helpful message *)
type result = Legal of t | Illegal of string | Null of t | RoundEnd of t * Deck.t * Deck.t * int

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
  let starting_cards = Deck.start_cards in
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

(*Note: used below*)
let get_current_player st = 
  st.current_player

let get_last_move_type st  : Command.command option =
  let last_move =  fst (st.last_moves) in 
  match last_move with 
  | None -> None
  |(Some (Draw x, b)) -> Some (Draw x)
  |(Some (Discard x, b)) -> Some (Discard x)
  |(Some (Knock, b))-> Some (Knock)
  |(Some (Pass, b)) -> Some (Pass)
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

let update_player st card =
  if (st.current_player = 0) then 
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
  let card = if (location = "stock") then Deck.hd current_stock else Deck.hd current_discard in
  {
    stock_pile = if (location = "stock") then (Deck.tl current_stock) 
      else current_stock;
    discard_pile = if (location = "discard") then (Deck.tl current_discard)
      else current_discard;
    players = update_player st card;
    current_player = st.current_player;
    last_moves = (Some (Draw [location], Some card), fst st.last_moves);
  }

let draw location st =
  if not (List.mem location ["stock"; "discard"]) then Illegal "You can only draw from \"stock\" or \"discard\". Try again."
  else match st.last_moves,location with
    | (None,None),"discard" | (Some (Pass,_),None),"discard"
    | (Some (Discard _,_),_),_ | (Some (Pass,_),Some (Pass,_)),_-> 
      if location = "discard" && (Deck.length st.discard_pile) = 0 then Illegal "The discard pile is empty. Try again."
      else
        (let new_st = get_new_draw_state st location in
         if (Deck.length new_st.stock_pile) <= 2
         then Null (init_state ((fst st.players).score, (snd st.players).score) 0
                      ((fst st.players).name, (snd st.players).name))
         else Legal new_st)
    | (None,None),"stock" | (Some (Pass,_),None),"stock" -> Illegal "You cannot draw from stock now; type \"draw discard\" or \"pass\"."
    | (Some (Knock,_),_),_ -> Illegal "You cannot draw now; type \"match\" or \"help\"."
    | _ -> Illegal "You cannot draw now."

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
  if fst st.last_moves = Some (Draw ["discard"], Some card) then 
    Illegal "You cannot discard the card you just drew from the discard pile. Try another one."
  else 
    match fst st.last_moves with
    | Some (Draw _, _) ->
      let p_ind = st.current_player in
      let p = if p_ind = 0 then fst st.players else snd st.players in
      if not (Deck.mem card p.hand) then Illegal "You do not have this card. Try again."
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
    | Some (Knock,_) -> Illegal "You cannot discard now; type \"match\" or \"help\"."
    | _ -> Illegal "You cannot discard now."

let knock_declare st = 
  (* 1. Determine whether curr_p can knock. *)
  match fst st.last_moves with
  | Some (Draw _,_) ->
    let knocker = if st.current_player = 0
      then fst st.players else snd st.players in
    let k_deadwood = Deck.knock_deadwood_value knocker.hand in
    if k_deadwood > 10 then Illegal "You do not have less than 10 deadwood."
    else if k_deadwood = 0 then 
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
    else Legal ({
        stock_pile = st.stock_pile;
        discard_pile = st.discard_pile;
        players = st.players;
        current_player = (st.current_player + 1) mod 2;
        last_moves = (Some (Knock, None),fst st.last_moves);
      })
  | Some (Knock,_) -> Illegal "You cannot knock now; type \"match\" or \"help\"."
  | _ -> Illegal "You cannot knock now; try again after a draw."


let knock_match_declare st = 
  match fst st.last_moves with
  | Some (Knock,None) -> Legal st
  | _ -> Illegal "You cannot match now; try again after your opponent has knocked."


let knock_player_update players new_hands new_scores =
  ({
    name = (fst players).name;
    hand = fst new_hands;
    score = fst new_scores;
  },
    {
      name = (snd players).name;
      hand = snd new_hands;
      score = snd new_scores;
    })

let knock_state_update st new_hands new_scores winner = 
  {
    stock_pile = st.stock_pile;
    discard_pile = st.discard_pile;
    players = knock_player_update st.players new_hands new_scores;
    current_player = winner;
    last_moves = (None,None);
  }

let knock_match match_deck st = 
  if not (fst st.last_moves = Some (Knock, None)) 
  then failwith "something went wrong."
  else
    let m_ind = st.current_player in
    (* 2. Determine whether match_deck is valid *)
    let k,m = if m_ind = 0
      then snd st.players, fst st.players
      else fst st.players, snd st.players in
    let k_orig_hand = k.hand in
    let k_melds = Deck.best_meld k_orig_hand in 
    if not (Deck.valid_match match_deck k_melds) 
    then Illegal "Not all these cards form melds. Try again."
    else
      (* 3. Calculate scores *)
      let k_ind = (m_ind + 1) mod 2 in
      let k_new_hand = Deck.push_deck match_deck k_orig_hand in
      let m_new_hand = Deck.remove_deck match_deck m.hand in
      let p0_new_hand, p1_new_hand = 
        if m_ind = 0 then m_new_hand,k_new_hand else k_new_hand,m_new_hand
      in
      let p0_deadwood_val,p1_deadwood_val = 
        Deck.deadwood_value p0_new_hand,Deck.deadwood_value p1_new_hand
      in
      print_string ("***DEBUG: deadwood value p0: "^string_of_int p0_deadwood_val^"\n");
      print_string ("***DEBUG: deadwood value p1: "^string_of_int p1_deadwood_val^"\n");
      let names = ((fst st.players).name,(snd st.players).name) in
      let p0_score_orig,p1_score_orig = (fst st.players).score,(snd st.players).score in
      let deadwood_diff = p0_deadwood_val - p1_deadwood_val in
      if deadwood_diff < 0 then (
        if k_ind = 0 then
          let round_score = (-deadwood_diff) in
          let p0_score = p0_score_orig+round_score in
          print_string("1: p0_score: "^(string_of_int p0_score)^"\n");
          let next_st = init_state (p0_score,p1_score_orig) 0 names in
          RoundEnd (next_st,p0_new_hand,p1_new_hand,round_score)
        else
          let round_score = (-deadwood_diff)+10 in
          let p0_score = p0_score_orig+round_score in
          print_string("2: p0_score: "^(string_of_int p0_score)^"\n");
          let next_st = init_state (p0_score,p1_score_orig) 0 names in
          RoundEnd (next_st,p0_new_hand,p1_new_hand,round_score)
      ) else if deadwood_diff > 0 then (
        if k_ind = 0 then
          let round_score = deadwood_diff+10 in
          let p1_score = p1_score_orig+round_score in
          print_string("3: p1_score: "^(string_of_int p1_score)^"\n");
          let next_st = init_state (p0_score_orig,p1_score) 1 names in
          RoundEnd (next_st,p1_new_hand,p0_new_hand,round_score)
        else
          let round_score = deadwood_diff in
          let p1_score = p1_score_orig+round_score in
          print_string("4: p1_score: "^(string_of_int p1_score)^"\n");
          let next_st = init_state (p0_score_orig,p1_score) 1 names in
          RoundEnd (next_st,p1_new_hand,p0_new_hand,round_score)
      ) else (
        if k_ind = 0 then
          let round_score = deadwood_diff+10 in
          let p1_score = p1_score_orig+round_score in
          print_string("5: p1_score: "^(string_of_int p1_score)^"\n");
          let next_st = init_state (p0_score_orig,p1_score) 1 names in
          RoundEnd (next_st,p1_new_hand,p0_new_hand,round_score)
        else
          let round_score = deadwood_diff+10 in
          let p0_score = p0_score_orig+round_score in
          print_string("6: p0_score: "^(string_of_int p0_score)^"\n");
          let next_st = init_state (p0_score_orig,p0_score) 0 names in
          RoundEnd (next_st,p0_new_hand,p1_new_hand,round_score)
      )

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