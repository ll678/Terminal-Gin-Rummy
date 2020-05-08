type suit = Clubs | Diamonds | Hearts | Spades

type rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King

type card = rank * suit

type t = card list

exception Malformed

let init_deck = 
  [
    (Ace, Clubs); (Ace, Diamonds); (Ace, Hearts); (Ace, Spades);
    (Two, Clubs); (Two, Diamonds); (Two, Hearts); (Two, Spades);
    (Three, Clubs); (Three, Diamonds); (Three, Hearts); (Three, Spades);
    (Four, Clubs); (Four, Diamonds); (Four, Hearts); (Four, Spades);
    (Five, Clubs); (Five, Diamonds); (Five, Hearts); (Five, Spades);
    (Six, Clubs); (Six, Diamonds); (Six, Hearts); (Six, Spades);
    (Seven, Clubs); (Seven, Diamonds); (Seven, Hearts); (Seven, Spades);
    (Eight, Clubs); (Eight, Diamonds); (Eight, Hearts); (Eight, Spades);
    (Nine, Clubs); (Nine, Diamonds); (Nine, Hearts); (Nine, Spades);
    (Ten, Clubs); (Ten, Diamonds); (Ten, Hearts); (Ten, Spades);
    (Jack, Clubs); (Jack, Diamonds); (Jack, Hearts); (Jack, Spades);
    (Queen, Clubs); (Queen, Diamonds); (Queen, Hearts); (Queen, Spades);
    (King, Clubs); (King, Diamonds); (King, Hearts); (King, Spades);
  ]

let push card deck = 
  card :: deck

let push_deck from_deck to_deck = 
  from_deck @ to_deck

let mem card deck = 
  List.mem card deck

let rec nth deck idx = 
  match deck with
  | [] -> failwith "nth failure"
  | h :: t -> if idx = 0 then h else nth t (idx-1)

let hd deck = 
  match deck with
  | [] -> failwith "no head"
  | h :: _ -> h

let tl deck = 
  match deck with
  | [] -> failwith "no tail"
  | _ :: t -> t

let rec length deck = 
  match deck with
  | [] -> 0
  | _ :: t -> 1 + length t

let is_empty deck = 
  deck = []

let rec remove card deck = 
  match deck with
  | [] -> failwith "remove failure: card not in deck."
  | h :: t -> if h = card then t else h :: remove card t

let rec remove_deck rm_deck deck = 
  match rm_deck with
  | [] -> deck
  | h :: t -> remove_deck t (remove h deck)

(** [value_of_card num] returns the int value corresponding to 
    a card of [num]. *)
let value_of_card rank =
  match rank with
  | Ace -> 1 | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6
  | Seven -> 7 | Eight -> 8 | Nine -> 9 | Ten -> 10 | Jack -> 10 
  | Queen -> 10 | King -> 10

(** [value_of_hand hand] returns the total int value of [hand]. *)
let value_of_hand hand =
  List.fold_right (fun (rank, _) -> (+) (value_of_card rank)) hand 0

let shuffle deck =
  let int_dict = List.map (fun c -> (Random.self_init(); 
                                     Random.int (length deck), c)) deck in
  let sort_dict = List.sort compare int_dict in
  sort_dict |> List.split |> snd

let suit_sort hand =
  let swap = List.map (fun (rank, suit) -> (suit, rank)) hand in
  let sort = List.sort compare swap in 
  List.map (fun (rank, suit) -> (suit, rank)) sort

(** [set_melds hand] creates a list of all possible set melds 
    in [hand]. *)
let set_melds hand =
  let aces = List.filter (fun (rank, _) -> rank = Ace) hand in
  let twos = List.filter (fun (rank, _) -> rank = Two) hand in
  let threes = List.filter (fun (rank, _) -> rank = Three) hand in
  let fours = List.filter (fun (rank, _) -> rank = Four) hand in
  let fives = List.filter (fun (rank, _) -> rank = Five) hand in
  let sixes = List.filter (fun (rank, _) -> rank = Six) hand in
  let sevens = List.filter (fun (rank, _) -> rank = Seven) hand in
  let eights = List.filter (fun (rank, _) -> rank = Eight) hand in
  let nines = List.filter (fun (rank, _) -> rank = Nine) hand in
  let tens = List.filter (fun (rank, _) -> rank = Ten) hand in
  let jacks = List.filter (fun (rank, _) -> rank = Jack) hand in
  let queens = List.filter (fun (rank, _) -> rank = Queen) hand in
  let kings = List.filter (fun (rank, _) -> rank = King) hand in
  let set = [aces; twos; threes; fours; fives; sixes; sevens; eights; nines; 
             tens; jacks; queens; kings] in
  List.filter (fun lst -> List.length lst > 2) set

(** [run_melds_helper lst acc] takes a deck [lst] and an accumulator deck [acc]
    and returns a deck of set melds. 
    This is a helper function for [run_melds]. *)
let rec run_melds_helper lst acc =
  match lst with
  | [] -> acc
  | (n1, s1)::(n2, s2)::(n3, s3)::t -> (match n1 with
      | Ace when n2 = Two && n3 = Three -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Two when n2 = Three && n3 = Four -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Three when n2 = Four && n3 = Five -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Four when n2 = Five && n3 = Six -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Five when n2 = Six && n3 = Seven -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Six when n2 = Seven && n3 = Eight -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t)
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Seven when n2 = Eight && n3 = Nine -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Eight when n2 = Nine && n3 = Ten -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Nine when n2 = Ten && n3 = Jack -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Ten when n2 = Jack && n3 = Queen -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | Jack when n2 = Queen && n3 = King -> 
        run_melds_helper ((n2, s2)::(n3, s3)::t) 
          ((n1, s1)::(n2, s2)::(n3, s3)::acc)
      | _ -> run_melds_helper ((n2, s2)::(n3, s3)::t) acc)
  | _ -> acc

(** [run_melds hand] creates a list of all possible run melds in [hand]. *)
let run_melds hand =
  let clubs = List.sort compare 
      (List.filter (fun (_, suit) -> suit = Clubs) hand) in
  let clubs_run = List.sort_uniq compare (run_melds_helper clubs []) in
  let spades = List.sort compare 
      (List.filter (fun (_, suit) -> suit = Spades) hand) in
  let spades_run = List.sort_uniq compare (run_melds_helper spades []) in
  let hearts = List.sort compare 
      (List.filter (fun (_, suit) -> suit = Hearts) hand) in
  let hearts_run = List.sort_uniq compare (run_melds_helper hearts []) in
  let diamonds = List.sort compare 
      (List.filter (fun (_, suit) -> suit = Diamonds) hand) in
  let diamonds_run = List.sort_uniq compare (run_melds_helper diamonds []) in
  let runs = [clubs_run; spades_run; hearts_run; diamonds_run] in
  List.filter (fun lst -> lst <> []) runs

(** [union l1 l2] contains only the elements that are elements of [l1]
    or elements of [l2]. This function was inspired by code previously
    used in A4: Search. *)
let rec union l1 l2 =
  List.fold_right 
    (fun card acc -> if List.mem card l2
      then acc else card::acc) l1 l2

let intersect l1 l2 =
  List.fold_right 
    (fun card acc -> if (List.mem card l1) && (List.mem card l2) 
      then card::acc else acc) (union l1 l2) []

(** [difference l1 l2] contains only the elements that are elements of [l1]
    but not elements of [l2]. This function was inspired by code previously
    used in A4: Search. *)
let difference l1 l2 =
  List.fold_right 
    (fun card acc -> if (List.mem card l1) && not (List.mem card l2) 
      then card::acc else acc) (union l1 l2) []

(** [combs r list] finds all possible combinations of the elements of [list]
    in a list of length [r]. This is a helper function for [all_combs]. *)
let rec combs r list =
  if r < 1 then [[]]
  else match list with
    | [] -> []
    | h::t ->
      let hd = List.map (fun a -> h::a) (combs (r - 1) t) in
      let tl = combs r t in
      List.concat [hd; tl]

(** [all_combs r list acc] finds all possible combinations of the elements 
    of [list] in lists ranging from length 0 to length [r]. 
    This is a helper function for [meld_combs]. *)
let rec all_combs r list acc =
  if r < 0 then acc
  else all_combs (r - 1) list ((combs r list)::acc)

(** [meld_combs hand] gets all combinations of a list of the intersect 
    run and set meld cards of [hand]. *)
let meld_combs hand =
  let runs = run_melds hand |> List.flatten in 
  let sets = set_melds hand |> List.flatten in 
  let inter = intersect runs sets in
  let combs = List.flatten (all_combs (List.length inter) inter []) in combs

(** [diff_melds hand] returns a tuple of the run meld cards and the set meld 
    cards of [hand] without the intersect meld cards. *)
let diff_melds hand =
  let runs = run_melds hand |> List.flatten in 
  let sets = set_melds hand |> List.flatten in 
  (difference runs (intersect runs sets), difference sets (intersect runs sets))

(** [add meld_cards list tup acc] combines each element of [list] with the
    first element of [tup] and the difference between each element of [list] 
    and[meld_cards] with the second element of [tup] in a tuple and appends
    each tuple to a list. *)
let rec add meld_cards list tup acc =
  match list with
  | [] -> acc
  | h::t -> 
    add meld_cards t tup 
      ((List.concat [h; (fst tup)], 
        List.concat [(difference meld_cards h); (snd tup)])::acc)

(** [get_melds hand] creates a list of pairs of all possible set and run meld 
    combinations in hand. *)
let get_melds hand =
  let runs = run_melds hand |> List.flatten in 
  let sets = set_melds hand |> List.flatten in 
  let inter = intersect runs sets in
  let list = add inter (meld_combs hand) (diff_melds hand) [] in
  List.map (fun (runs, sets) -> (run_melds runs, set_melds sets)) list

(** [sum_melds l1 l2] sums the value of each card element of [l1] and [l2]
    such that the sum of the first elements in [l1] and [l2] are added to a
    new list, and then sum of the second elements, etc. *)
let sum_melds l1 l2 =
  (List.fold_right (fun (num, _) -> (+) (value_of_card num)) l1 0)
  +
  (List.fold_right (fun (num, _) -> (+) (value_of_card num)) l2 0)

(** [calculate_meld_value list acc] calculates the total value of each run 
    and set meld combination in [list], which contains a tuple of sets and
    melds. *)
let rec calculate_meld_value list acc =
  match list with
  | [] -> acc
  | (a, b)::t ->
    calculate_meld_value t ((sum_melds (List.flatten a) (List.flatten b))::acc)

(** [list_traversal i n list] returns the index of [n] in [list]. *)
let rec list_traversal i n list =
  if (nth list i) = n then i else list_traversal (i+1) n list

let best_meld hand =
  let melds = get_melds hand in
  let meld_vals = List.rev (calculate_meld_value melds []) in
  let largest = function
    | [] -> 0
    | x::xs -> List.fold_left max x xs
  in
  let max_val = largest meld_vals in
  let max_index = list_traversal 0 max_val meld_vals in
  let best_meld = nth melds max_index in
  List.flatten [fst best_meld; snd best_meld]

let deadwood hand =
  difference hand (List.flatten (best_meld hand))

let rec card_score (card:card) (hand:t) (acc:int) =
  match hand with 
  | [] -> acc
  | h::t -> 

    (* let acc = if fst h = fst card then acc +1 else acc in 
       let acc = if 


    *)

    (* if fst h = fst card then
       card_score card t (acc +1) 

       else  card_score card t (acc) *)

    failwith "unfinished"


let get_value (card:card) (hand:t) =
  let dif = difference hand [card] in 
  (card, card_score card hand 0)

let rec least (lst:((card*int) list)) (acc) =
  match lst with 
  | [] -> acc
  | h::t -> if snd h < snd acc then (least t h) else least t acc

let rec get_values deadwood i acc =
  if i >= List.length deadwood then acc else
    let value = (get_value (nth deadwood i) deadwood) in
    get_values deadwood (i-1) ((value)::acc)  

let get_worst deadwood =
  let vals = get_values deadwood 0 [] in 
  least vals (hd vals)

let deadwood_value hand =
  hand |> deadwood |> value_of_hand

let meld_value hand =
  (hand |> value_of_hand) - (hand |> deadwood_value)

let knock_deadwood_value hand =
  let d = deadwood hand in
  match d with
  | [] -> 0
  | _ ->
    let vals = List.map (fun (rank, _) -> value_of_card rank) d in
    let largest = function
      | [] -> 0
      | x::xs -> List.fold_left max x xs
    in
    let max_val = largest vals in
    let max_index = list_traversal 0 max_val vals in
    let max_card = nth d max_index in
    let final_d = remove max_card d in
    value_of_hand final_d

(** [valid_melds card melds] returns true if [card] extends any of the melds
    in [melds]. *)
let rec valid_melds card melds =
  match melds with
  | [] -> false
  | h::t -> let hand = push card h in
    if is_empty (deadwood hand) then true else valid_melds card t

let rec valid_match match_deck melds =
  match match_deck with
  | [] -> true
  | h::t -> if (valid_melds h melds) then valid_match t melds else false

let rec get_list n l = 
  if n=0 then [] else
    match l with 
    | []-> []
    | h::t ->  h::(get_list (n-1) t)

let start_cards =
  (* let temp = shuffle init_deck in   *)
  let temp = init_deck in

  let fst = get_list 31 temp  in
  let snd = get_list 1 (difference temp fst) in
  let trd = get_list 10 (difference (difference temp fst) snd) in
  let fth = 
    get_list 10 (difference (difference (difference temp fst) snd) trd) in
  [fst; snd; trd; fth]

let string_of_card card = 
  let suit = match snd card with
    | Clubs -> "Clubs"
    | Diamonds -> "Diamonds"
    | Hearts -> "Hearts"
    | Spades -> "Spades"
  in
  let rank = match fst card with
    | Ace -> "Ace"
    | Two -> "Two"
    | Three -> "Three"
    | Four -> "Four"
    | Five -> "Five"
    | Six -> "Six"
    | Seven -> "Seven"
    | Eight -> "Eight"
    | Nine -> "Nine"
    | Ten -> "Ten"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
  in
  rank ^ " of " ^ suit

let string_of_card_short card = 
  let suit = match snd card with
    | Clubs -> "♣"
    | Diamonds -> "♦"
    | Hearts -> "♥"
    | Spades -> "♠"
  in
  let rank = match fst card with
    | Ace -> "A"
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
  in
  rank ^ suit

let rec string_of_deck deck = 
  match deck with
  | [] -> []
  | h :: t -> string_of_card h :: string_of_deck t

let rec string_of_deck_short deck = 
  match deck with
  | [] -> []
  | h :: t -> string_of_card_short h :: string_of_deck_short t

let card_of_string string = 
  let str_lst = String.split_on_char ' ' string |> Command.remove_emptys_lower
  in
  if length str_lst < 3 then raise Malformed else
    let fst = nth str_lst 0 in 
    let rank = 
      if fst = "ace" then Ace
      else if fst = "two" then Two
      else if fst = "three" then Three
      else if fst = "four" then Four
      else if fst = "five" then Five
      else if fst = "six" then Six
      else if fst = "seven" then Seven
      else if fst = "eight" then Eight
      else if fst = "nine" then Nine
      else if fst = "ten" then Ten
      else if fst = "jack" then Jack
      else if fst = "queen" then Queen
      else if fst = "king" then King
      else raise Malformed
    in
    let snd = nth str_lst 2 in
    let suit = 
      if snd = "clubs" then Clubs
      else if snd = "diamonds" then Diamonds
      else if snd = "hearts" then Hearts 
      else if snd = "spades" then Spades 
      else raise Malformed
    in
    (rank, suit)

let rankstring_of_string string = 
  match string with
  | "Ace" -> "A"
  | "Two" -> "2"
  | "Three" -> "3"
  | "Four" -> "4"
  | "Five" -> "5"
  | "Six" -> "6"
  | "Seven" -> "7"
  | "Eight" -> "8"
  | "Nine" -> "9"
  | "Ten" -> "10"
  | "Jack" -> "J"
  | "Queen" -> "Q"
  | "King" -> "K"
  | _ -> ""

let suitstring_of_string string = 
  match string with
  | "Clubs" -> "♣"
  | "Diamonds" -> "♦"
  | "Hearts" -> "♥"
  | "Spades" -> "♠"
  | _ -> ""

let deck_of_string string = 
  if string = "" then [] else
    let rec deck_of_string_helper lst =
      match lst with
      | [] -> []
      | h::t -> (card_of_string h)::(deck_of_string_helper t) in
    let cards = List.map (String.trim) (String.split_on_char ',' string) in
    deck_of_string_helper cards

let string_of_hd deck = 
  match deck with
  | [] -> []
  | h :: _ -> [string_of_card h]


(* The values below are used strictly for testing Deck functions. *)

let test_hand_empty = []

let test_hand = [(Queen, Hearts);(Six, Clubs); (Four, Hearts); 
                 (Four, Clubs); (Four, Spades); (Two, Clubs); (Two, Hearts); 
                 (Ace, Spades); (Three, Spades); (Two, Spades)]

let test_hand2 = [(Five, Spades); (Six, Diamonds); (Three, Spades); 
                  (Four, Diamonds); (Six, Spades); (Five, Diamonds); 
                  (Two, Spades); (Five, Clubs); (Four, Spades); (Six, Clubs)]

let test_hand3 = [(Five, Spades); (Six, Diamonds); (Three, Spades); 
                  (Four, Diamonds); (Five, Diamonds); (Two, Spades); 
                  (Five, Clubs); (Four, Spades); (Six, Clubs)]

let test_hand4 = [(King, Hearts); (Five, Spades); (Six, Diamonds); 
                  (Three, Spades); (Four, Diamonds); (Five, Diamonds); 
                  (Two, Spades); (Five, Clubs); (Four, Spades); (Six, Clubs)]

let test_hand5 = [(Five, Spades); (Six, Diamonds); (Three, Spades); 
                  (Five, Diamonds); (Two, Spades); (Five, Clubs); (Six, Clubs)]

let test_meld = [
  [(Ace, Spades); (Two, Spades); (Three, Spades)];
  [(Four, Spades); (Four, Hearts); (Four, Clubs)]; 
]

let test_meld2 = [
  [(Two, Spades); (Three, Spades); (Four, Spades)];
  [(Five, Spades); (Five, Diamonds); (Five, Clubs)];
  [(Six, Diamonds); (Six, Spades); (Six, Clubs)]
]

let sorted_test_hand2 = [(Five, Clubs); (Six, Clubs); (Four, Diamonds); 
                         (Five, Diamonds); (Six, Diamonds); (Two, Spades); 
                         (Three, Spades); (Four, Spades);(Five, Spades); 
                         (Six, Spades)]

let test_deadwood = [(Queen, Hearts); (Six, Clubs); (Two, Clubs); (Two, Hearts)]

let test_deadwood2 = [(Four, Diamonds)]