
type move = Discard of Deck.card | Draw of string | Knock | Cards of Deck.t | Match

let optimal_sort hand =
  let deadwood = Deck.deadwood hand in 
  let tmp = Deck.get_worst deadwood in 
  Discard (fst tmp)

let optimal_discard st =
  let hand = State.get_current_player_hand st in 
  optimal_sort hand

let optimal stock discard hand =
  let first = Deck.push stock hand in
  let second = Deck.push discard hand in 
  let value_one = Deck.value_of_hand first in 
  let value_two = Deck.value_of_hand second in
  if value_one = value_two then Draw "Either" else
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
  Deck.difference (Deck.deadwood (State.get_opponent_player_hand st)) (left_over_deadwood)


let get_optimal st =
  (* Pass is not implemented *)
  match State.get_last_moves_type st with
  | None -> Draw "Discard"
  | Some Draw x -> 
    (match (State.knock_declare st) with 
     | Illegal _ -> if x = ["Discard"] then Draw "Discard" else optimal_discard st
     | Legal _ -> check_optimal_knock st x
     | RoundEnd _ -> Knock
     | Null _ -> failwith "Null game")
  | Some Discard x -> if x = ["Discard"] then Draw "Discard" else optimal_draw st
  | Some Pass -> optimal_draw st
  | Some Knock -> Match
  | Some Match -> Cards (optimal_match st)
  | _ -> failwith "Not sure how you got here 1"



