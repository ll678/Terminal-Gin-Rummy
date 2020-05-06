(** This code was inspired by the adventure game we created in A2 and A3 *)

let rec print_list lst =
  match lst with
  | [] -> ()
  | h::t -> print_string h; 
    print_string " "; 
    print_list t

let if_red suit =
  if suit = "Hearts" || suit = "Diamonds" then true else false

let rec print_cards_top lst =
  match lst with
  | [] -> print_string ""
  | h::t -> print_string " ___   "; print_cards_top t

let rec print_cards_rank1 lst = 
  match lst with
  | [] -> print_string ""
  | h::t -> let rank = Deck.nth (String.split_on_char ' ' h) 0 in
    let suit = Deck.nth (String.split_on_char ' ' h) 2 in
    if (if_red suit) then
      (if rank = "Ten" then
         (print_string ("|");
          ANSITerminal.(print_string [red] (Deck.rankstring_of_string rank));
          print_string (" |  "); 
          print_cards_rank1 t)
       else
         (print_string ("|");
          ANSITerminal.(print_string [red] (Deck.rankstring_of_string rank));
          print_string ("  |  "); 
          print_cards_rank1 t))
    else
      (if rank = "Ten" then
         (print_string ("|" ^ Deck.rankstring_of_string rank ^ " |  "); 
          print_cards_rank1 t)
       else
         (print_string ("|" ^ Deck.rankstring_of_string rank ^ "  |  "); 
          print_cards_rank1 t))

let rec print_cards_suit lst = 
  match lst with
  | [] -> print_string ""
  | h::t -> let suit = Deck.nth (String.split_on_char ' ' h) 2 in
    if (if_red suit) then
      (print_string ("| ");
       ANSITerminal.(print_string [red] (Deck.suitstring_of_string suit));
       print_string (" |  ");
       print_cards_suit t)
    else 
      (print_string ("| ");
       print_string (Deck.suitstring_of_string suit);
       print_string (" |  ");
       print_cards_suit t)

let rec print_cards_rank2 lst = 
  match lst with
  | [] -> print_string ""
  | h::t -> let rank = Deck.nth (String.split_on_char ' ' h) 0 in
    let suit = Deck.nth (String.split_on_char ' ' h) 2 in
    if (if_red suit) then
      (if rank = "Ten" then
         (print_string ("| ");
          ANSITerminal.(print_string [red] (Deck.rankstring_of_string rank));
          print_string ("|  "); 
          print_cards_rank2 t)
       else
         (print_string ("|  ");
          ANSITerminal.(print_string [red] (Deck.rankstring_of_string rank));
          print_string ("|  "); 
          print_cards_rank2 t))
    else
      (if rank = "Ten" then
         (print_string ("| " ^ Deck.rankstring_of_string rank ^ "|  "); 
          print_cards_rank2 t)
       else
         (print_string ("|  " ^ Deck.rankstring_of_string rank ^ "|  "); 
          print_cards_rank2 t))

let rec print_cards_bottom lst = 
  match lst with
  | [] -> print_string ""
  | h::t -> print_string " ‾‾‾   "; print_cards_bottom t

let print_cards lst =
  print_cards_top lst; print_string "\n";
  print_cards_rank1 lst; print_string "\n";
  print_cards_suit lst; print_string "\n";
  print_cards_rank2 lst; print_string "\n";
  print_cards_bottom lst; print_string "\n"

let print_piles lst =
  if List.length lst = 0 then
    (print_string "                           ___  \n";
     print_string "                          |░░░| \n";
     print_string "                          |░░░| \n";
     print_string "                          |░░░| \n";
     print_string "                           ‾‾‾  \n")
  else
    (print_cards_top lst; print_string "                    ___ \n";
     print_cards_rank1 lst; print_string "                   |░░░| \n";
     print_cards_suit lst; print_string "                   |░░░| \n";
     print_cards_rank2 lst; print_string "                   |░░░| \n";
     print_cards_bottom lst; print_string "                    ‾‾‾ \n")

let rec print_melds lst =
  match lst with
  | [] -> ()
  | h::t -> print_cards (Deck.string_of_deck h);
    print_string "\n"; 
    print_melds t

let rec print_deadwood lst =
  match lst with
  | [] -> ()
  | h::t -> let rank = Deck.nth (String.split_on_char ' ' h) 0 in
    let suit = Deck.nth (String.split_on_char ' ' h) 2 in
    if (if_red suit) then
      (ANSITerminal.(print_string [red] (Deck.rankstring_of_string rank));
       ANSITerminal.(print_string [red] (Deck.suitstring_of_string suit));
       print_string "  ";)
    else 
      (print_string (Deck.rankstring_of_string rank);
       print_string (Deck.suitstring_of_string suit);
       print_string "  ");
    print_deadwood t

let change (new_st : State.result) (st : State.t) = 
  match new_st with 
  | Legal t -> t
  | Illegal str -> print_string (str^"\n"); st
  | Null t -> 
    print_string 
      "Less than two cards in stock. Game is null. New round starting... \n"; 
    st
  | Win t -> failwith "you shouldn't have won"

let handle_score (st : State.t) = 
  print_string "Your score is: " ; 
  print_int (State.get_current_player_score st);
  print_endline "\n"

let rec do_nothing st =
  print_string "> ";
  match read_line () with 
  | "resume" | "Resume" -> st
  | _ -> print_endline "Invalid command. Please type \"resume\""; do_nothing st

let print_help st = 
  print_endline "How to play Gin Rummy:\n";
  print_string 
    "Basic Gameplay

     It is your turn to make a move. Each turn consists of a draw and a discard.
     Enter \"draw Discard\" to draw the card on top of the Discard pile, or 
     \"draw Stock\" to draw a random card from the Stock pile. Then enter 
     \"discard\" and the name of the card that you wish to discard from your 
     hand (ex. \"three of hearts\"). By drawing new cards, you will try to form 
     as many melds as you can in your hand.

     If it is your first turn, you can only either draw from the Discard pile 
     or pass your turn (type \"pass\").

     A meld is either:
     1) Three or more cards of the same rank (called a set)
     2) Three or more cards of the same suit in consecutive rank order 
     (called a run)
     (In Gin Rummy, the Ace is low, so A-2-3 is a valid run, but Q-K-A is not.)

    How to Win

     The remaining cards in your hand that do not form melds are called 
     deadwood. Your objective is to minimize the value of your deadwood. When 
     your deadwood has a value of 10 or less, you can choose to end the round 
     by knocking (type \"knock\"). Knocking discards the highest value card 
     in your deadwood and arranges the remaining cards into runs and sets. 

     After knocking, your opponent will be allowed to lay off their deadwood 
     cards by adding them to your melds (but not your deadwood), if possible.
     To lay off any cards, type \"match\". Then, you will be prompted to type
     the list of cards that you wish to lay off, separated by a comma and 
     no spaces.

     Your scores will then be calculated and the winner of the round will be 
     the player with a lower deadwood value. If you knock when you have no 
     deadwood, that is called \"going gin\" (type \"gin\" or \"knock\") and 
     your opponent will not be allowed to lay off. The game will continue until 
     a player reaches a score of 100 or more and wins.

    Other Commands

     score- view your current score
     sort- sort your hand by suit first, then rank
     help- bring up rules and gameplay information
     quit- quit the game


    Type \"resume\" to resume playing";
  print_string "\n";
  print_string "> ";
  match read_line () with 
  | "resume" | "Resume" -> st
  | _ -> print_endline "Invalid command. Please type \"resume\"."; do_nothing st

(** After Player 1 knocks in state [st], [knock] handles [new_st], 
    in which Player 2 is the current player and can choose cards to lay off. 
    The resulting state is an initialized subsequent round, unless
    a player wins and the game ends. *)
let rec knock (new_st : State.result) (st : State.t) : State.t =
  match new_st with 
  | Legal t -> t
  | Illegal str -> print_string (str^"\n"); st
  | Null t -> failwith "knock fail: null"
  | Win t -> failwith "knock fail: win"

let rec knock_match (st : State.t) : State.t =
  match State.knock_match_declare st with
  | Legal st ->
    begin
      print_string "Your Deadwood:\n";
      print_cards (st |> State.get_current_player_hand |> Deck.deadwood |> 
                   Deck.string_of_deck);
      print_string ("\n\n");
      print_string (st |> State.get_opponent_player_name); 
      print_string "'s Melds:\n";
      print_melds (st |> State.get_opponent_player_hand |> Deck.best_meld);
      print_string ("\n");

      print_endline ("\nPlease list any cards you want to lay off. Separate cards with a single comma.");
      print_string  "> ";
      match read_line () with 
      | exception End_of_file -> 
        print_string "I don't know what you did, but... try again.\n"; st
      | read_line-> 
        match Deck.deck_of_string read_line with
        | exception Deck.Malformed ->
          print_endline "You cannot match these. Try again.\n"; 
          st
        | valid_deck -> match State.knock_match valid_deck st with
          | Legal new_st ->
            (* | Legal new_st,end_hand_p1,end_hand_p2,winner_bonus -> *)
            (* TODO: want to print ending deadwoods, score added before next round *)
            let winner_score = State.get_current_player_score new_st in
            let winner_name = State.get_current_player_name new_st in
            let loser_score = State.get_opponent_player_score new_st in
            let loser_name = State.get_opponent_player_name new_st in
            print_string winner_name; print_string "'s Score: "; 
            print_endline (winner_score |> string_of_int);
            print_string loser_name; print_string "'s Score: "; 
            print_endline (loser_score |> string_of_int);
            print_endline (winner_name ^ " has won this round!");
            new_st 
          (* TODO: prompt for continue to next round... new function *)
          | Illegal _ -> print_string "Not all these cards form melds. Try again.\n"; 
            knock_match st
          | Null new_st -> failwith "knock fail (shouldnt happen)"
          | Win new_st -> 
            let winner_score = State.get_current_player_score new_st in
            let winner_name = State.get_current_player_name new_st in
            let loser_score = State.get_opponent_player_score new_st in
            let loser_name = State.get_opponent_player_name new_st in
            print_string winner_name; print_string "'s Final Score: "; 
            print_endline (winner_score |> string_of_int);
            print_string loser_name; print_string "'s Final Score: "; 
            print_endline (loser_score |> string_of_int);
            print_string ("Congrats, " ^ winner_name ^ ", you've won!"); exit 0
    end
  | Illegal str -> print_string (str^"\n"); st
  | _ -> failwith "knock_match: something went wrong."

(** [process_command] takes terminal input and executes a command. The command 
    may or may not change state but process_command always returns a state. *)
let process_command (command : Command.command) (st : State.t) =  
  match command with
  | Draw obj_phrase -> change (State.draw (String.concat " " obj_phrase) st) st
  | Discard obj_phrase -> (
      match (String.concat " " obj_phrase) |> Deck.card_of_string with 
      | exception Deck.Malformed -> 
        print_endline "You cannot discard this.\n"; 
        st
      | valid_card -> change (State.discard valid_card st) st)
  | Knock -> knock (State.knock_declare st) st
  | Match -> knock_match st
  | Pass -> change (State.pass st) st
  | Sort -> change (State.sort st) st
  | Score -> handle_score st; st
  | Help -> print_help st
  | Quit -> exit 0

(*A function that either quits or executes a command based on input*)
let process_readline read_line (st : State.t) = 
  match Command.parse read_line with
  | exception Command.Empty ->
    print_endline "This is an invalid command.\n"; st
  | exception Command.Malformed ->
    print_endline "This is an malformed command.\n"; st
  | command -> (process_command command st)

(* Should initalize game but not initiate state transitions *)
let rec play_game (st : State.t) =
  print_string "\n----------------------------------------------------------\n";

  print_string "It is "; print_string (st |> State.get_current_player_name); 
  print_string "'s Turn:\n\n";

  print_string "Discard Pile:             Stock Pile:\n";
  print_piles (st |> State.get_discard |> Deck.string_of_hd);
  print_string ("\n");

  print_string (st |> State.get_current_player_name); print_string "'s Hand:\n";
  print_cards (st |> State.get_current_player_hand |> Deck.string_of_deck);
  print_string ("\n");

  (* Print melds of current player's hand *)
  print_string "\nMelds:\n";
  print_melds (st |> State.get_current_player_hand |> Deck.best_meld);
  print_string ("\n");

  (* Print deadwood of current player's hand *)
  print_string "Deadwood:\n";
  print_deadwood (st |> State.get_current_player_hand |> Deck.deadwood |> 
                  Deck.string_of_deck);
  print_string ("\n\n");

  (* Prompt for the appropriate command(s). *)
  print_endline (State.prompt_command st);
  print_string "\n> ";

  match read_line () with 
  | exception End_of_file -> ()
  | read_line -> let next_st = process_readline read_line st in 
    (play_game next_st)

(** [init_game n1 n2] starts a game of gin rummy with players [n1] and [n2]. *)
let init_game name1 name2 =
  let init = State.init_state (0, 0) 0 (name1,name2) in
  play_game init

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to Gin Rummy.\n";
  print_endline "Please enter your name, Player 1.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name1 -> 
    print_endline "Please enter your name, Player 2.\n";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> ()
    | name2 -> init_game name1 name2

(* Execute the game engine. *)
let () = main ()
