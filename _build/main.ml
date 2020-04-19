open Deck
open Command
open State

(** [play_game f] starts the adventure in file [f]. *)
let play_game name1 name2 =
  failwith "unimplemented" 

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
    | name2 -> play_game name1 name2

(* Execute the game engine. *)
let () = main ()
