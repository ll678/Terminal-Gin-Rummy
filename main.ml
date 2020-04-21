open Deck
open Command
open State

let rec print_list lst =
  match lst with
  | [] -> ()
  | h::t -> print_string h; 
    print_string " "; 
    print_list t

let rec play_game st =
  (* Print stock pile *)
  print_list (current_stock_pile st);

  (* Print first card in discard pile *)
  print_endline (List.nth (get_discard_pile st) 0);

  (* Print hand of current player 
     (Function not yet defined in state.ml *)
  print_list (get_current_player_hand);

  (* Prompt for player to draw. *)
  print_endline ("Draw a card from either the stock or the discard pile.");


  failwith "unimplemented"

(** [init_game n1 n2] starts a game of gin rummy with players [n1] and [n2]. *)
let init_game name1 name2 =
  let init = init_state (0, 0) name1 in
  play_game init

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to Gin Rummy.\n";
  print_endline "Please enter your name, Player 1.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name1 -> 
    print_endline "Please enter your name, Player 1.\n";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> ()
    | name2 -> init_game name1 name2

(* Execute the game engine. *)
let () = main ()
