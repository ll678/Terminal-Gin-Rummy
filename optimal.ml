
type move = Discard of Deck.card | Draw of string | Knock | Cards of Deck.t 
          | Match | Pass

(** [last_card_drawn st] is the last card drawn in [st] *) 
let last_card_drawn st =
  State.get_last_card_drawn st 

(** [optimal_sort hand st] is an optimal [move] *) 
let optimal_sort hand st =
  let deadwood = Deck.deadwood hand in 
  let tmp = Deck.get_worst deadwood in 
  match (last_card_drawn st) with
  | Some x ->
    if (fst (fst tmp)) = x then Discard (fst (snd tmp)) else Discard 
        (fst(fst tmp))
  | None ->
    Discard (fst (fst tmp))

(** [optimal_discard st] is the optimal Discard move *) 
let optimal_discard st =
  let hand = State.get_current_player_hand st in 
  optimal_sort hand st

(** [optimal st] is the optimal draw move *) 
let optimal stock discard hand =
  let first = Deck.push stock hand in
  let second = Deck.push discard hand in 
  (* We want to compare the two deadwood values (whatever has the 
     lowest deadwood value) *)
  let value_one = Deck.deadwood_value first in 
  let value_two = Deck.deadwood_value second in



  if value_one = value_two then Draw "Discard" else
  if value_one > value_two then Draw "Discard" else
    Draw "Stock"


(** [print_string_list lst] prints the string list [lst] in the
    command shell. *)
let rec print_string_list = function 
    [] -> ()
  | h::t -> print_string h ; print_string " " ; print_string_list t



(** [optimal_draw st] is the optimal draw move *) 
let optimal_draw st = 
  let stock = State.get_stock st in 
  let discard = State.get_discard st in 
  let hand = State.get_current_player_hand st in 
  if Deck.length stock <2 then
    Draw "Discard" else
  if Deck.length discard <=0 then Draw "Stock" else
    optimal (Deck.hd stock) (Deck.hd discard) hand 


(** [add_valid_deadwood meld first second] is Meld with extending 
    deadwood added on *) 
let add_valid_deadwood meld first second =
  (* let sorted = Deck.suit_sort meld in  *)
  if Deck.is_set meld then 
    Deck.push_deck (Deck.find_deadwood_with_rank first (Deck.hd meld)) meld
  else 
    (* Checking to see what we can add in both directiions *)
    let f = Deck.add_run meld first  in
    let s = Deck.add_run meld second  in

    (* print_string ("****DEBUG Beggining of first****");
       print_string_list (Deck.string_of_deck f);
       print_string ("****DEBUG Beggining of second****");
       print_string_list (Deck.string_of_deck s);
       print_string ("****DEBUG END****"); *)
    Deck.union f s

(** [match_deadwood matcher_deadwood knocker_melds acc] is the Deck.t list
    of valid matching deadwood *) 
let rec match_deadwood matcher_deadwood knocker_melds acc =
  let first = Deck.suit_sort matcher_deadwood in
  let second = Deck.rev_sort  matcher_deadwood in
  match knocker_melds with
  | [] -> acc
  | h::t -> 
    let new_meld = add_valid_deadwood h first second in 
    match_deadwood (Deck.difference matcher_deadwood new_meld) (t) 
      ((Deck.difference new_meld h)::acc)

(** [optimal_match st] is the Deck.t with valid matching cards *) 
let optimal_match st =
  let matcher_deadwood = Deck.deadwood (State.get_current_player_hand st) in
  let knocker_melds = Deck.best_meld (State.get_opponent_player_hand st) in
  Deck.flatten_deck (match_deadwood matcher_deadwood knocker_melds [])

(** [optimal_match st] is the Deck.t with valid matching cards, if the 
    opponent was matching *) 
let optimal_opponent_match st =
  let matcher_deadwood = Deck.deadwood (State.get_opponent_player_hand st) in
  let knocker_melds = Deck.best_meld (State.get_current_player_hand st) in
  Deck.flatten_deck (match_deadwood matcher_deadwood knocker_melds [])

(** [check_pass st] is [Draw "discard"] if drawing adds to the player's score.
    [Pass] otherwise *) 
let check_pass st =
  let current = Deck.value_of_hand (State.get_current_player_hand st) in
  let check = Deck.push (Deck.hd (State.get_discard st)) 
      (State.get_current_player_hand st) in 
  if (Deck.value_of_hand check) > current then Draw "discard" else Pass

(** [check_optimal_knock st x] is [Knock] if the player can knock and 
    [Discard of string] otherwise *) 
let check_optimal_knock st x =
  (* The current player's deadwood value *)
  let current_deadwood_value = Deck.deadwood_value 
      (State.get_current_player_hand st) in
  let difference = Deck.difference (Deck.deadwood 
                                      (State.get_opponent_player_hand st)) 
      (optimal_opponent_match st) in
  let new_opposing_value = Deck.deadwood_value difference in
  if current_deadwood_value < new_opposing_value then Knock else 
  if x = ["Discard"] then Draw "Discard" else optimal_discard st
(* let combined_hand = Deck.push_deck (Deck.deadwood 
   (State.get_opponent_player_hand st)) (State.get_current_player_hand st) in
   let left_over_deadwood = Deck.deadwood combined_hand in *)

(* let matching_cards = optimal_match st in
   let new_opposing_value = Deck.deadwood_value left_over_deadwood in


   if current_deadwood_value < new_opposing_value then Knock else 
   if x = ["Discard"] then Draw "Discard" else optimal_discard st *)


let get_optimal st =
  match State.get_last_moves_type st with
  | None -> Draw "Discard"
  | Some Draw x -> 
    (match (State.knock_declare st) with 
     (* if x = ["Discard"] then Draw "Discard" else *)
     | Illegal _ -> optimal_discard st
     | Legal _ -> check_optimal_knock st x
     | RoundEnd _ -> Knock
     | Null _ -> failwith "Null Game")
  | Some Discard x -> if x = ["Discard"] then Draw "Discard" else 
      optimal_draw st
  | Some Pass -> check_pass st
  | Some Knock -> Cards (optimal_match st)
  | _ -> failwith "Not sure how you got here 1"



