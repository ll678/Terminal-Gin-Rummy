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

let change (command : State.result) (st : State.t) = 
  match command with 
  | Legal t -> t
  | Illegal -> print_string "This is an illegal move.\n"; st
  | Null t -> t

(** We need to implement win condition*)
(* | Win t -> print_string (t); exit 0 *)

let handle_score (st : State.t) = 
  print_string "Your score is: " ; 
  print_int (get_player_score st);
  print_endline "\n"


(**process_command takes terminal input and executes a command. The command may or
   may not change state but process_command always returns a state *)
let process_command (command : Command.command) (st : State.t) =  
  match command with
  | Draw obj_phrase -> change (State.draw (String.concat " " obj_phrase) st) st
  | Discard obj_phrase -> change (State.discard (String.concat " " obj_phrase) st) st
  | Knock -> change (State.knock (String.concat " " ) st) st
  | Pass -> st (** Need to discuss this, not currently functional*)
  | Sort -> change (State.sort (String.concat " " ) st) st
  | Score -> handle_score st; st
  | Show obj_phrase -> st (**Need to discuss *)
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
  (* Print stock pile *)
  (* TODO: will we want to do this? *)
  (* print_list (State.get_stock st); *)
  (* Print first card in discard pile *)




  print_string "Discard Pile:\n";
  print_endline (st |> State.get_discard |> Deck.string_of_deck |> List.hd);
  (* Print hand of current player (Function not yet defined in state.ml string function @lawrence?*)
  print_endline (st |> State.get_current_player_string); print_string "'s Hand:\n";
  print_list (st |> State.get_current_player_hand |> Deck.string_of_deck);
  (* Prompt for player to draw. *)
  print_endline ("Please draw a card from either the stock or the discard pile.");
  print_string  "> ";

  (* match parse (read_line ()) with *)
  match read_line () with 
  | exception End_of_file -> ()
  | read_line -> let state = process_readline read_line st in 
    (play_game state )

(* 

  | Draw deck -> let new_st = draw deck st in
    (* Print stock pile *)
    print_list (State.get_stock new_st);

    (* Print first card in discard pile *)
    print_endline (st |> State.get_discard |> Deck.string_of_deck |> List.hd);

    (* TODO: note - after a player knocks, main should immediately prompt
       the opponent for a list of cards to match with the knocker's deadwood *)
    (* 1. player 1 knocks 2. call State.knock_declare, this will return
       Legal / Illegal for the knocker 3. prompt player 2 for a list of cards
       4. call State.knock_match, this will return Legal / Illegal for player 2 *)

    (* Print hand of current player 
       (Function not yet defined in state.ml) *)
    print_list (st |> State.get_current_player_hand |> Deck.string_of_deck);
    if knocking allowed then
      (print_endline ("Discard a card from your hand or knock.");
       match parse (read_line()) with
       | Discard card -> let next_st = discard card new_st in play_game next_st
       | Knock -> (* implement knocking function in state *) failwith "unimplemented"
       | _ -> print_endline ("Invalid command."))
    else 
      (print_endline ("Discard a card from your hand or knock.");
       match parse (read_line ()) with
       | Discard card -> let next_st = discard card new_st in play_game next_st
       | _ -> print_endline ("Invalid command."))

 *)


(* | _ -> print_endline ("Invalid command.")

         failwith "unimplemented" *)

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
