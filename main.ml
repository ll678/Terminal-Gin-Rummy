(** This code was inspired by the adventure game we created in p2 and p3 *)

(* Things to do:
   Implement knocking in state
   Implement getting current player's hand
   Functions handling drawing and discarding produce results (Legal/Illegal) and not states
   so pattern matching needs to be added to handle Legal of st and Illegal
   If command is invalid, rerun previous state and prompt for command again
*)

let rec print_list lst =
  match lst with
  | [] -> ()
  | h::t -> print_string h; 
    print_string " "; 
    print_list t

let rec print_melds lst =
  match lst with
  | [] -> ()
  | h::t -> print_list (Deck.string_of_deck h); 
    print_string "\n"; 
    print_melds t

let change (command : State.result) (st : State.t) = 
  match command with 
  | Legal t -> t
  | Illegal -> print_string "This is an illegal move.\n"; st
  | Null t -> print_string "This is an invalid command.\n"; st
  | Win -> print_string ("Congrats, you've won"); exit 0 


let handle_score (st : State.t) = 
  print_string "Your score is: " ; 
  print_int (State.get_current_player_score st);
  print_endline "\n"

(** After Player 1 knocks, [knock] handles [st], in which Player 2 is the
    current player and can choose cards to lay off. The resulting *)
let rec knock (command : State.result) (st : State.t) =
  match command with 
  | Legal t -> 
    (print_endline (st |> State.get_current_player_name); print_string "'s Hand:\n";
     print_list (st |> State.get_current_player_hand |> Deck.string_of_deck);
     (* Print deadwood of current player's hand *)
     print_string "Deadwood:\n";
     print_list (st |> State.get_current_player_hand |> Deck.deadwood 
                 |> Deck.string_of_deck);
     print_endline ("Please list any cards you want to lay off.");
     print_string  "> ";
     (* match parse (read_line ()) with *)
     match read_line () with 
     | exception End_of_file -> ()
     | read_line-> let res = State.knock_match (Deck.deck_of_string read_line) st in
       match res with
       | Legal new_st ->
         print_string (st |> State.get_current_player_name); print_string "'s Score: "; print_endline (st |> State.get_current_player_score |> string_of_int);
         print_string (new_st |> State.get_current_player_name); print_string "'s Score: "; print_endline (new_st |> State.get_current_player_score |> string_of_int);
         print_endline (get_winner_name ^ " has won this round!"); (* Implement get_winner_name *)
         play_game new_st
       | Illegal -> print_string "This is an illegal move.\n"; knock command st
       | Win -> print_string ("Congrats, you've won"); exit 0 
       | Null t -> print_endline "This is an invalid command.\n"; knock command st)
  | Illegal -> print_string "This is an illegal move.\n"; knock command st
  | Null t -> print_endline "This is an invalid command.\n"; knock command st
  | Win -> print_string ("Congrats, you've won"); exit 0 

(**process_command takes terminal input and executes a command. The command may or
   may not change state but process_command always returns a state *)
let process_command (command : Command.command) (st : State.t) =  
  match command with
  | Draw obj_phrase -> change (State.draw (String.concat " " obj_phrase) st) st
  | Discard obj_phrase -> change (State.discard (String.concat " " obj_phrase |> Deck.card_of_string) st) st
  | Knock -> knock (State.knock_declare st) st
  | Pass -> change (State.pass (String.concat " " ) st) st 
  | Sort -> change (State.sort (String.concat " " ) st) st
  | Score -> handle_score st; st
  | Quit -> exit 0

(* | Discard obj_phrase -> change (State.discard (String.concat " " obj_phrase) st) st
   | Knock -> change (State.knock (String.concat " " ) st) st
   | Pass -> change (State.pass (String.concat " " ) st) st *)


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
  (* Print stock pile *)
  (* TODO: will we want to do this? *)
  (* print_list (State.get_stock st); *)
  (* Print first card in discard pile *)
  print_string "Discard Pile:\n";
  print_endline (st |> State.get_discard |> Deck.string_of_deck |> List.hd);

  print_string (st |> State.get_current_player_name); print_string "'s Hand:\n";
  print_list (st |> State.get_current_player_hand |> Deck.string_of_deck);

  (* Print melds of current player's hand *)
  print_string "Melds:\n";
  print_melds (st |> State.get_current_player_hand |> Deck.best_meld);

  (* Print deadwood of current player's hand *)
  print_string "Deadwood:\n";
  print_list (st |> State.get_current_player_hand |> Deck.deadwood 
              |> Deck.string_of_deck);

  (* Prompt for player to draw. *)
  print_endline ("Please draw a card from either the stock or the discard pile.");
  print_string "> ";

  (* match parse (read_line ()) with *)
  match read_line () with 
  | exception End_of_file -> ()
  | read_line -> let state = process_readline read_line st in 
    (play_game state )


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
