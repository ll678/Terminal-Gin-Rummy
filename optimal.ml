
type move = Discard of Deck.card | Draw of string | Knock | Cards of Deck.t | Match

let last_card_drawn st =
  State.get_last_card_drawn st 

let optimal_sort hand st =
  let deadwood = Deck.deadwood hand in 
  let tmp = Deck.get_worst deadwood in 
  match (last_card_drawn st) with
  | Some x ->
    if (fst (fst tmp)) = x then Discard (fst (snd tmp)) else Discard (fst(fst tmp))
  | None ->
    Discard (fst (fst tmp))

let optimal_discard st =
  let hand = State.get_current_player_hand st in 
  optimal_sort hand st

let optimal stock discard hand =
  let first = Deck.push stock hand in
  let second = Deck.push discard hand in 
  let value_one = Deck.value_of_hand first in 
  let value_two = Deck.value_of_hand second in
  if value_one = value_two then Draw "Discard" else
  if value_one > value_two then Draw "Stock" else
    Draw "Discard"

let optimal_draw st = 
  let stock = State.get_stock st in 
  let discard = State.get_discard st in 
  let hand = State.get_current_player_hand st in 
  if Deck.length stock <2 then
    Draw "Discard" else
  if Deck.length discard <=0 then Draw "Stock" else
    optimal (Deck.hd stock) (Deck.hd discard) hand 

(* Plan: we know that we can knock so now we want to see if we were to knock if we would win
   In order to do this we need to take the current players dead wood value and set that to a 
   variable. After doing that we want to add all of the opposing players deadwood to the knocking players hand
   Then we want to get the deadwood of this new hand and calculate the value of this deadwood hand. 
   After doing that we want to compare the vlaues. *)
let check_optimal_knock st x =
  let current_deadwood_value = Deck.deadwood_value (State.get_current_player_hand st) in
  let combined_hand = Deck.push_deck (Deck.deadwood (State.get_opponent_player_hand st)) (State.get_current_player_hand st) in
  let left_over_deadwood = Deck.deadwood combined_hand in
  let new_opposing_value = Deck.deadwood_value left_over_deadwood in
  if current_deadwood_value < new_opposing_value then Knock else 
  if x = ["Discard"] then Draw "Discard" else optimal_discard st

(* Plan: In order to know what to match we want to match and see what melds would be taken.
   In order to do this we first want to create a (combined) hand with the matcher's deadwood and the opponents hand.
   Then we want to get the deadwood from this combined hand and get rid of the original deadwood that we had *)
let optimal_match st =
  let combined_hand = Deck.push_deck (Deck.deadwood (State.get_current_player_hand st)) (State.get_opponent_player_hand st) in
  let left_over_deadwood = Deck.difference (Deck.deadwood combined_hand) (Deck.deadwood (State.get_opponent_player_hand st)) in
  let tmp = Deck.difference  (left_over_deadwood) (Deck.deadwood (State.get_opponent_player_hand st)) in 
  Deck.difference (Deck.deadwood (State.get_current_player_hand st)) (tmp)




let add_valid_deadwood meld first second =
  let sorted = Deck.suit_sort meld in 
  if Deck.is_set meld then 



    Deck.push_deck (Deck.find_deadwood_with_rank first (Deck.hd meld)) meld
  else 

    let f = Deck.add_run meld first  in
    let s = Deck.add_run meld first  in

    Deck.union f s




let rec match_deadwood matcher_deadwood knocker_melds acc =
  let first = Deck.suit_sort matcher_deadwood in
  let second = Deck.rev_sort  matcher_deadwood in
  match knocker_melds with
  | [] -> acc
  | h::t -> 
    let new_meld = add_valid_deadwood h first second in 
    match_deadwood (Deck.difference matcher_deadwood new_meld) (t) ((Deck.difference new_meld h)::acc)


(* list of matchers deadwood 
   melds of knockers hand - list of knocker's best melds (best melds function in Deck)
   iterate over different melds, if the meld is a set then look for same rank and suit
   if the meld is a run then sort deadwood of the same suit, and 
   For match go through each meld  *)
let optimal_match_fix st =
  let matcher_deadwood = Deck.deadwood (State.get_current_player_hand st) in
  let knocker_melds = Deck.best_meld (State.get_opponent_player_hand st) in
  Deck.flatten_deck (match_deadwood matcher_deadwood knocker_melds [])

let get_optimal st =
  (* Pass is not implemented *)
  match State.get_last_moves_type st with
  | None -> Draw "Discard"
  | Some Draw x -> 
    (match (State.knock_declare st) with 
     (* if x = ["Discard"] then Draw "Discard" else *)
     | Illegal _ -> optimal_discard st
     | Legal _ -> check_optimal_knock st x
     | RoundEnd _ -> Knock
     | Null _ -> failwith "Null Game")
  | Some Discard x -> if x = ["Discard"] then Draw "Discard" else optimal_draw st
  | Some Pass -> optimal_draw st
  | Some Knock -> Cards (optimal_match_fix st)
  | _ -> failwith "Not sure how you got here 1"



