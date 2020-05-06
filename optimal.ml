

type move = Discard of Deck.card | Draw of string | Knock 

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
  optimal (Deck.hd stock) (Deck.hd discard) hand


let get_optimal st =
  match State.get_last_move_type st with
  | None -> optimal_draw st
  | Some Draw x -> optimal_discard st
  | Some Discard x -> optimal_draw st
  | Some Pass -> optimal_draw st
  | _ -> failwith "Not sure how you got here"