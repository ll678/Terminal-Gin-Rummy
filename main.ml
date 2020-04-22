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
  (* TODO: will we want to do this? *)
  (* print_list (State.get_stock st); *)

  (* Print first card in discard pile *)
  print_endline (st |> State.get_discard |> Deck.string_of_deck |> List.hd);

  (* Print hand of current player 
     (Function not yet defined in state.ml *)
  print_list (st |> State.get_current_player_hand |> Deck.string_of_deck);

  (* Prompt for player to draw. *)
  print_endline ("Draw a card from either the stock or the discard pile.");

  (* TODO: note - after a player knocks, main should immediately prompt
     the opponent for a list of cards to match with the knocker's deadwood *)
  (* 1. player 1 knocks 2. call State.knock_declare, this will return
     Legal / Illegal for the knocker 3. prompt player 2 for a list of cards
     4. call State.knock_match, this will return Legal / Illegal for player 2 *)
  failwith "unimplemented"

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
    print_endline "Please enter your name, Player 1.\n";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> ()
    | name2 -> init_game name1 name2

(* Execute the game engine. *)
let () = main ()
