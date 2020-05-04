open OUnit2
open Deck
open State
open Command

(** Test Plan:
    Our OUnit test suite automatically tests the main functions in the Deck
    and Command modules, as well as several functions in the State module. The
    remaining State module functions and the Main module were manually debugged
    by playtesting the game. 
    To test the Deck and Command modules, black box testing was utilized, 
    implementing test cases commonly encountered during gameplay, as well as 
    unexpected/invalid inputs resulting in raised exceptions. Sample decks and 
    melds for testing purposes were created in the Deck module to facilitate 
    the testing of various functions.
    To test the State and Main modules, we actively playtested the game as
    functions were implemented. This involved both inputting valid and invalid
    commands when interacting with the command shell, including testing edge
    cases (e.g. drawing from discard results in an empty discard pile).
    Overall, this testing approach, which was largely modeled after A2/A3
    (Adventure), demonstrates the validity of our system because it combines 
    both efficient and automatic OUnit testing for the simpler functions and 
    playtesting with effective code coverage for the more complex functions 
    that handle state. Our testing scenarios consider all possible inputs 
    encountered during gameplay.
*) 

let suit_sort_test
    (name : string)
    (hand : Deck.t)
    (expected_output : Deck.t) : test =
  name >:: (fun _ -> 
      assert_equal (suit_sort hand) expected_output)

let value_of_hand_test
    (name : string)
    (hand : Deck.t)
    (expected_output : int) : test =
  name >:: (fun _ ->
      assert_equal (value_of_hand hand) expected_output)

let melds_test
    (name : string)
    (hand : Deck.t)
    (expected_output : Deck.t list) : test =
  name >:: (fun _ -> 
      assert_equal (best_meld hand) expected_output)

let deadwood_test
    (name : string)
    (hand : Deck.t)
    (expected_output : Deck.t) : test =
  name >:: (fun _ -> 
      assert_equal (deadwood hand) expected_output)

let string_of_deck_test
    (name : string)
    (hand : Deck.t)
    (expected_output : string list) : test =
  name >:: (fun _ -> 
      assert_equal (string_of_deck hand) expected_output)

let deck_of_string_test
    (name : string)
    (hand : string)
    (expected_output : Deck.t) : test =
  name >:: (fun _ -> 
      assert_equal (deck_of_string hand) expected_output)

let push_test
    (name : string)
    (card_string : string)
    (hand : Deck.t)
    (expected_output : Deck.t) : test =
  name >:: (fun _ -> 
      assert_equal (push (card_of_string card_string) hand) expected_output)

let remove_test
    (name : string)
    (card_string : string)
    (hand : Deck.t)
    (expected_output : Deck.t) : test =
  name >:: (fun _ -> 
      assert_equal (remove (card_of_string card_string) hand) expected_output)

let deck_tests = 
  [
    suit_sort_test "sort hand 2 by suit" test_hand2 sorted_test_hand2;
    value_of_hand_test "value of empty hand" test_hand_empty 0;
    value_of_hand_test "value of hand 1" test_hand 38;
    value_of_hand_test "value of hand 2" test_hand2 46;
    melds_test "testing meld of empty hand" test_hand_empty [];
    melds_test "testing meld one run and set" test_hand test_meld;
    melds_test "testing meld multiple runs/sets" test_hand2 test_meld2;
    deadwood_test "testing deadwood of empty hand" 
      test_hand_empty test_hand_empty;
    deadwood_test "testing deadwood of hand 1" test_hand test_deadwood;
    deadwood_test "testing deadwood of hand 2" test_hand2 test_deadwood2;
    string_of_deck_test "string of hand 1" test_hand
      ["Queen of Hearts"; "Six of Clubs"; "Four of Hearts"; "Four of Clubs";
       "Four of Spades"; "Two of Clubs"; "Two of Hearts"; "Ace of Spades";
       "Three of Spades"; "Two of Spades"];
    deck_of_string_test "deck of hand 1" 
      "queen of hearts,six of clubs,four of hearts,four of clubs,four of spades,two of clubs,two of hearts,ace of spades,three of spades,two of spades" 
      test_hand;
    remove_test "remove a card from hand 2 (uppercase)" "Six of Spades" 
      test_hand2 test_hand3;
    remove_test "remove a card from hand 2 (lowercase)" "six of spades" 
      test_hand2 test_hand3;
    push_test "push a card to hand 3 (mixed case)" "king of Hearts" 
      test_hand3 test_hand4;
    push_test "push a card to hand 3 (lowercase)" "king of hearts" 
      test_hand3 test_hand4;
  ]

let parse_test 
    (name : string) 
    (str : string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str))

let command_tests = [
  parse_test "draw a card lowercase" "draw stock" 
    (Draw ["stock"]);
  parse_test "draw a card uppercase" "draw Discard" 
    (Draw ["Discard"]);
  parse_test "discard a card uppercase" "discard jack of diamonds" 
    (Discard ["jack";"of";"diamonds"]);
  parse_test "knock" "knock" 
    (Knock);
  parse_test "gin" "gin" 
    (Knock);
  parse_test "match" "match" 
    (Match);
  parse_test "pass" "pass" 
    (Pass);
  parse_test "sort" "sort" 
    (Sort);
  parse_test "score" "score" 
    (Score);
  parse_test "help" "help" 
    (Help);
  "parse malformed exception" >:: 
  (fun _ -> assert_raises (Malformed) 
      (fun () -> parse "hello   world "));
  "parse malformed exception sort" >:: 
  (fun _ -> assert_raises (Malformed) 
      (fun () -> parse "sort deck"));
  "parse malformed exception help" >:: 
  (fun _ -> assert_raises (Malformed) 
      (fun () -> parse "help me"));
  "parse empty exception" >:: 
  (fun _ -> assert_raises (Empty) 
      (fun () -> parse "      "));
]

let suite = 
  "test suite for A2"  >::: List.flatten [deck_tests; command_tests]

let () = run_test_tt_main suite
