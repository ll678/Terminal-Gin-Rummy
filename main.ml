(** This code was inspired by the adventure game we created in A2 and A3 *)

(** [print_string_list lst] prints the string list [lst] in the
    command shell. *)
let rec print_string_list = function 
    [] -> ()
  | h::t -> print_string h ; print_string " " ; print_string_list t

(** [if_red suit] is true if [suit] is "Hearts" or "Diamonds". *)
let if_red suit =
  if suit = "Hearts" || suit = "Diamonds" then true else false

(** [print_cards_top lst] prints the top line marking each card in [lst] in the
    command shell. *)
let rec print_cards_top lst =
  match lst with
  | [] -> print_string ""
  | h::t -> print_string " ___   "; print_cards_top t

(** [print_cards_rank1 lst] prints the rank symbol in the top left corner of 
    each card in [lst] in the command shell. *)
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

(** [print_cards_suit lst] prints the suit symbol in the center of each card in 
    [lst] in the command shell. *)
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

(** [print_cards_rank2 lst] prints the rank symbol in the bottom right corner of 
    each card in [lst] in the command shell. *)
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

(** [print_cards_bottom lst] prints the bottom line marking each card in [lst] 
    in the command shell. *)
let rec print_cards_bottom lst = 
  match lst with
  | [] -> print_string ""
  | h::t -> print_string " ‾‾‾   "; print_cards_bottom t

(** [print_cards lst] prints a graphic representation of the cards in [lst]
    in sequential order (left to right) in the command shell. *)
let print_cards lst =
  print_cards_top lst; print_string "\n";
  print_cards_rank1 lst; print_string "\n";
  print_cards_suit lst; print_string "\n";
  print_cards_rank2 lst; print_string "\n";
  print_cards_bottom lst; print_string "\n"

(** [print_piles lst] prints a graphic representation of the face-up card in the
    discard pile [lst], followed by a graphic representation of the face-down 
    card in the stock pile in the command shell. If the discard pile is empty,
    no card is printed for the discard pile. *)
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

(** [print_melds lst] prints each sublist of cards in [lst], separated by
    a new line between each sublist. *)
let rec print_melds lst =
  match lst with
  | [] -> ()
  | h::t -> print_cards (Deck.string_of_deck h);
    print_string "\n"; 
    print_melds t

(** [print_deadwood lst] prints the deadwood cards in [lst]. *)
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
  | RoundEnd _ -> failwith "change fail: roundend"

let handle_score (st : State.t) = 
  print_string "Your score is: " ; 
  print_int (State.get_current_player_score st);
  print_endline "\n"

let handle_hint (new_move : Optimal.move) (st : State.t) = 
  match new_move with
  | Discard t -> print_string "Discard: " ; 
    print_string (Deck.string_of_card t);
    print_endline "\n"
  | Draw t -> print_string "Draw from: " ; 
    print_string t;
    print_endline "\n"
  | Knock -> print_string "You should knock to win this round!" ; 
    print_endline "\n"
  | Cards t -> print_string "Match with: " ; 
    print_string_list (Deck.string_of_deck t);
    print_endline "\n"
  | Match -> print_string "Type Match to begin to match cards!" ; 
    print_endline "\n"

let perform_optimal st =
  let call = Optimal.get_optimal st in
  match call with
  | Discard t -> "Discard " ^ Deck.string_of_card t  ; 
  | Draw t -> "Draw " ^  t ; 
  | Knock -> "Knock" ;
  | Cards t -> String.concat "Match "  (Deck.string_of_deck t)
  | Match -> "Match"

(** [do_nothing st] prompts for the "resume" keyword to resume playing if
    the player enters any string other than "resume". *)
let rec do_nothing st =
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with   
  | "resume" -> st
  | _ -> print_endline "Invalid command. Please type \"resume\""; do_nothing st

(** [print_help st] prints the help menu prompted by the Help command and
    prompts for the "resume" keyword to resume playing. *)
let print_help st = 
  print_endline "How to play Gin Rummy:\n";
  print_string 
    "Basic Gameplay

     It is your turn to make a move. Each turn consists of a draw and a discard.
     Enter \"draw discard\" to draw the card on top of the discard pile, or 
     \"draw stock\" to draw a random card from the stock pile. Then enter 
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
     no spaces. If you knock when you have no deadwood, that is called 
     \"going gin\" (type \"gin\" or \"knock\") and your opponent will not be 
     allowed to lay off any cards. 

     Your scores will then be calculated and the winner of the round will be 
     the player with a lower deadwood value. The game will continue until 
     a player reaches a score of 100 or more.

    Other Commands

     score- view your current score
     sort- sort your hand by suit first, then rank
     help- bring up rules and gameplay information
     hint- suggests your next move
     quit- quit the game


    Type \"resume\" to resume playing";
  print_string "\n";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with 
  | "resume" -> st
  | _ -> print_endline "Invalid command. Please type \"resume\"."; do_nothing st


let conclude_round st winner_deck loser_deck round_score = 
  let winner_name = State.get_current_player_name st in
  let loser_name = State.get_opponent_player_name st in

  print_string (winner_name); 
  print_string "'s Ending Deadwood:\n";
  print_cards (winner_deck |> Deck.deadwood |> Deck.string_of_deck);
  print_string ("\n");
  print_string (loser_name); 
  print_string "'s Ending Deadwood:\n";
  print_cards (loser_deck |> Deck.deadwood |> Deck.string_of_deck);

  print_string ("\n\n");
  print_string (winner_name ^ " has won this round, gaining ");
  print_string ((string_of_int round_score) ^ " points!\n");

  let winner_score = State.get_current_player_score st in
  let loser_score = State.get_opponent_player_score st in
  print_string winner_name; print_string "'s Score: "; 
  print_endline (winner_score |> string_of_int);
  print_string loser_name; print_string "'s Score: "; 
  print_endline (loser_score |> string_of_int);

  print_endline ("\nPress enter to continue.");
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> 
    print_string "I don't know what you did, but... try again.\n"; st
  | _ ->
    if winner_score > 100 then (
      print_string ("\nCongrats " ^ winner_name ^ ", you've won!");
      exit 0
    ) else print_endline ("\nStarting new round..."); st 

(** After Player 1 knocks in state [st], [knock] handles [new_st], 
    in which Player 2 is the current player and can choose cards to lay off. 
    The resulting state is an initialized subsequent round, unless
    a player wins and the game ends. *)
let rec knock (new_st : State.result) (st : State.t) : State.t =
  match new_st with 
  | Legal t -> t
  | RoundEnd (new_st,winner_deck,loser_deck,round_score) ->
    conclude_round new_st winner_deck loser_deck round_score
  | Illegal str -> print_string (str^"\n"); st
  | Null t -> failwith "knock fail: null"

let rec knock_match (st : State.t) : State.t =
  match State.knock_match_declare st with
  | Legal st ->
    begin
      print_string "Your Deadwood:\n";
      print_cards (st |> State.get_current_player_hand |>
                   Deck.deadwood |> Deck.string_of_deck);
      print_string ("\n\n");
      print_string (st |> State.get_opponent_player_name); 
      print_string "'s Melds:\n";
      print_melds (st |> State.get_opponent_player_hand |>
                   Deck.best_meld);
      print_endline ("\n");
      print_endline 
        ("Please list any cards you want to lay off. \
          Separate cards with a single comma and no spaces.\n\
          If you cannot lay off any cards, press enter to end the round.");
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
          | RoundEnd (new_st,winner_deck,loser_deck,round_score) ->
            conclude_round new_st winner_deck loser_deck round_score
          | Illegal str -> print_string (str^"\n"); knock_match st
          | _ -> failwith "knock_match fail (shouldnt happen)"
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
  | Hint -> handle_hint (Optimal.get_optimal st) st; st
  | Score -> handle_score st; st
  | Help -> print_help st
  | Quit -> exit 0

(*A function that either quits or executes a command based on input*)
let process_readline read_line (st : State.t) = 
  match Command.parse read_line with
  | exception Command.Empty ->
    print_endline "This is an invalid command.\n"; st
  | exception Command.Malformed ->
    print_endline "This is a malformed command.\n"; st
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


let rec play_cpu_game (st : State.t) =
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


  if (State.get_current_player_name st = "CPU") then 
    let s = perform_optimal st in 
    let next_st = process_readline s st in 
    (play_cpu_game next_st)
  else
    begin
      print_endline (State.prompt_command st);
      print_string "\n> ";
      match read_line () with 
      | exception End_of_file -> ()
      | read_line -> let next_st = process_readline read_line st in 
        (play_cpu_game next_st)
    end

(** [init_game n1 n2] starts a game of gin rummy with players [n1] and [n2]. *)
let init_game name1 name2 b =
  print_endline 
    "\n
  You can type \"help\" for the game instructions and rules. 
  You can type \"hint\" for a hint.";
  if b then  
    let init = State.init_state (0, 0) 0 (name1,"CPU") in
    play_cpu_game init 
  else
    let init = State.init_state (0, 0) 0 (name1,name2) in
    play_game init

let rec main_help () =
  print_endline "Please enter a valid response:'yes' if you would like to play against the 
  computer and 'no' if you would like to play a two-player game .\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | answer -> 
    match answer with
    | "yes" -> begin print_endline "Please enter your name, Player 1.\n";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> ()
        | name1 -> 
          init_game name1 "" true
      end
    | "no" ->  
      begin
        print_endline "Please enter your name, Player 1.\n";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> ()
        | name1 -> 
          print_endline "Please enter your name, Player 2.\n";
          print_string  "> ";
          match read_line () with
          | exception End_of_file -> ()
          | name2 -> init_game name1 name2 false
      end
    | _-> main_help ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to Gin Rummy.\n";
  print_endline
    "Please enter 'yes' if you would like to play against the \
     computer and 'no' if you would like to play a two-player game.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | answer -> 
    match answer with
    | "yes" -> begin print_endline "Please enter your name, Player 1.\n";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> ()
        | name1 -> 
          init_game name1 "" true
      end

    | "no" ->  
      begin
        print_endline "Please enter your name, Player 1.\n";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> ()
        | name1 -> 
          print_endline "Please enter your name, Player 2.\n";
          print_string  "> ";
          match read_line () with
          | exception End_of_file -> ()
          | name2 -> init_game name1 name2 false
      end
    | _-> main_help ()


(* Execute the game engine. *)
let () = main ()
