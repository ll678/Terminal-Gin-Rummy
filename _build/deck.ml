type suit = Clubs | Diamonds | Hearts | Spades

type number = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine
            | Ten | Jack | Queen | King

type card = number * suit

let init_deck = 
  [(Ace, Clubs); (Ace, Diamonds); (Ace, Hearts); (Ace, Spades);
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

let value_of_card num =
  match num with
  | Ace -> 1 | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6
  | Seven -> 7 | Eight -> 8 | Nine -> 9 | Ten -> 10 | Jack -> 10 
  | Queen -> 10 | King -> 10

let shuffle deck =
  let int_dict = List.map (fun c -> (Random.bits (), c)) deck in
  let sort_dict = List.sort compare int_dict in
  sort_dict |> List.split |> snd

(** Sorts a hand by suit first, then number. *)
let suit_sort hand =
  let swap = List.map (fun (num, suit) -> (suit, num)) hand in
  let sort = List.sort compare swap in 
  List.map (fun (num, suit) -> (suit, num)) sort

(** Sorts a hand by number first, then suit. *)
let num_sort hand =
  List.sort compare hand

(** Creates a list of all set melds in card list [hand] *)
let set_melds hand =
  let aces = List.filter (fun (num, _) -> num = Ace) hand in
  let twos = List.filter (fun (num, _) -> num = Two) hand in
  let threes = List.filter (fun (num, _) -> num = Three) hand in
  let fours = List.filter (fun (num, _) -> num = Four) hand in
  let fives = List.filter (fun (num, _) -> num = Five) hand in
  let sixes = List.filter (fun (num, _) -> num = Six) hand in
  let sevens = List.filter (fun (num, _) -> num = Seven) hand in
  let eights = List.filter (fun (num, _) -> num = Eight) hand in
  let nines = List.filter (fun (num, _) -> num = Nine) hand in
  let tens = List.filter (fun (num, _) -> num = Ten) hand in
  let jacks = List.filter (fun (num, _) -> num = Jack) hand in
  let queens = List.filter (fun (num, _) -> num = Queen) hand in
  let kings = List.filter (fun (num, _) -> num = King) hand in
  let set = [aces; twos; threes; fours; fives; sixes; sevens; eights; nines; 
             tens; jacks; queens; kings] in
  List.filter (fun lst -> List.length lst > 2) set

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

(** Creates a list of all run melds in card list [hand] *)
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

let rec intersect runs sets =
  List.fold_right 
    (fun card acc -> if List.mem card runs then acc else card::acc) runs sets

(* find intersect of both melds
   create deck with run_meld with combination of union cards and set_meld without combination of union card
   (run_meld (hand minus union cards))
   calculate deadwood of each deck
*)

let combs hand =
  let runs = run_melds hand |> List.flatten in 
  let sets = set_melds hand |> List.flatten in 
  intersect runs sets

let hand = [(Queen, Hearts);(Six, Clubs); (Four, Hearts); (Four, Clubs); (Four, Spades); (Two, Clubs); (Two, Hearts); (Ace, Spades); (Three, Spades); (Two, Spades)];;