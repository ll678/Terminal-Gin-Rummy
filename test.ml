open OUnit2
open Deck
open State
open Command

(** Test Plan:
    Our OUnit test suite automatically tests the main functions in the Deck
    and Command modules. The State, Main, and Optimal modules were manually 
    debugged by playtesting the game. 
    To test the Deck and Command modules, black box testing was 
    utilized, implementing test cases commonly encountered during gameplay, as 
    well as unexpected/invalid inputs resulting in raised exceptions. Sample 
    decks and melds for testing purposes were created in the Deck module to 
    facilitate the testing of various functions.
    Testing the State, Main, and Optimal modules with our testing suite would
    have involved complicated test functions to initialize and change state.
    Instead, we actively playtested the game while functions were implemented
    to test these modules. This involved both inputting valid and invalid 
    commands when interacting with the command shell, including edge cases 
    (e.g. drawing from discard results in an empty discard pile).
    Overall, this testing approach, which was largely modeled after A2/A3
    (Adventure), demonstrates the validity of our system because it combines 
    both efficient and automatic OUnit testing for the simpler functions and 
    playtesting with effective code coverage for the more complex functions 
    that handle state and interaction with the command shell. Our testing 
    scenarios consider all the inputs encountered during gameplay.
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

let deadwood_value_test
    (name : string)
    (hand : Deck.t)
    (expected_output : int) : test =
  name >:: (fun _ -> 
      assert_equal (deadwood_value hand) expected_output)

let meld_value_test
    (name : string)
    (hand : Deck.t)
    (expected_output : int) : test =
  name >:: (fun _ -> 
      assert_equal (meld_value hand) expected_output)

let valid_match_test
    (name : string)
    (deck : string)
    (hand : Deck.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal (valid_match (deck_of_string deck) (best_meld hand))
        expected_output)

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

let remove_deck_test
    (name : string)
    (deck1 : string)
    (deck2 : Deck.t)
    (expected_output : Deck.t) : test =
  name >:: (fun _ -> 
      assert_equal (remove_deck (deck_of_string deck1) deck2) expected_output)

let string_of_hd_test
    (name : string)
    (deck : Deck.t)
    (expected_output : string list) : test =
  name >:: (fun _ -> 
      assert_equal (string_of_hd deck) expected_output)

let get_worst_test
    (name : string)
    (deck : Deck.t)
    (expected_output : (Deck.card * int)* (Deck.card * int)) : test =
  name >:: (fun _ -> 
      assert_equal (get_worst deck) expected_output)

let deck_tests = 
  [
    suit_sort_test "sort empty hand" test_hand_empty test_hand_empty;
    suit_sort_test "sort hand 2 by suit" test_hand2 sorted_test_hand2;
    value_of_hand_test "value of empty hand" test_hand_empty 0;
    value_of_hand_test "value of hand 1" test_hand 38;
    value_of_hand_test "value of hand 2" test_hand2 46;
    melds_test "meld of empty hand" test_hand_empty [];
    melds_test "meld one run and set" test_hand test_meld;
    melds_test "meld multiple runs/sets" test_hand2 test_meld2;
    deadwood_test "deadwood of empty hand" test_hand_empty test_hand_empty;
    deadwood_test "deadwood of hand 1" test_hand test_deadwood;
    deadwood_test "deadwood of hand 2" test_hand2 test_deadwood2;
    deadwood_value_test "deadwood value of empty hand" test_hand_empty 0;
    deadwood_value_test "deadwood value of hand 1" test_hand 20;
    deadwood_value_test "deadwood value when no deadwood" test_hand6 0;
    meld_value_test "meld value of empty hand" test_hand_empty 0;
    meld_value_test "meld value of hand 2" test_hand2 42;
    meld_value_test "meld value when no deadwood" test_hand6 48;
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
    remove_deck_test "remove three cards from hand 4" 
      "king of hearts,four of diamonds,four of spades" test_hand4 test_hand5;
    remove_deck_test "remove no cards from hand 3" 
      "" test_hand3 test_hand3;
    push_test "push a card to hand 3 (mixed case)" "king of Hearts" 
      test_hand3 test_hand4;
    push_test "push a card to hand 3 (lowercase)" "king of hearts" 
      test_hand3 test_hand4;
    string_of_hd_test "empty deck"
      test_hand_empty [];
    string_of_hd_test "non-empty deck"
      test_hand ["Queen of Hearts"];
    valid_match_test "empty match"
      "" test_hand true;
    valid_match_test "empty melds"
      "king of clubs" test_hand_empty false;
    valid_match_test "partially valid match, lowercase"
      "four of diamonds,five of spades" test_hand false;
    valid_match_test "invalid match, uppercase"
      "Five of Spades" test_hand false;
    valid_match_test "valid match, mixed cases"
      "ace of spades,Six of Hearts,five of Hearts" test_hand2 true;
    valid_match_test "valid match, multiple runs and sets"
      "six of clubs, seven of clubs" test_hand6 true;
    valid_match_test "invalid match, multiple runs and sets"
      "six of clubs" test_hand6 false;
    get_worst_test "worst of deadwood" test_deadwood 
      ((card_of_string "six of clubs", 3), (card_of_string "two of hearts", 4));
    get_worst_test "worst of deadwood with one card" test_deadwood2 
      ((card_of_string "four of diamonds", 2), 
       (card_of_string "four of diamonds", 2));
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
    (Draw ["discard"]);
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
  parse_test "hint" "hint" 
    (Hint);
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

(** [state_test] passes when next_st matches the given parameters. *)
let state_test
    (name : string)
    (next_st : State.t)
    (exp_curr_p : int)

    (exp_stock_len : int)
    (exp_discard_len : int)
    (exp_curr_hand_len : int)
    (exp_opp_hand_len : int)

    (exp_curr_score : int)
    (exp_opp_score : int) 

    (exp_curr_name : string)
    (exp_opp_name : string) 
  : test =
  name >:: (fun _ -> 
      assert_equal (next_st |> get_current_player) exp_curr_p;

      let stock = get_stock next_st in
      let discard = get_discard next_st in
      assert_equal (length stock) exp_stock_len;
      assert_equal (length discard) exp_discard_len;

      let curr_hand = get_current_player_hand next_st in
      let opp_hand = get_opponent_player_hand next_st in
      assert_equal (length curr_hand) exp_curr_hand_len;
      assert_equal (length opp_hand) exp_curr_hand_len;

      let all_cards = 
        stock |> union discard |> union curr_hand |> union opp_hand in
      assert_equal (length all_cards) 52;

      let curr_score = get_current_player_score next_st in
      assert_equal (curr_score) exp_curr_score;
      let opp_score = get_opponent_player_score next_st in
      assert_equal (opp_score) exp_opp_score;

      let curr_name = get_current_player_name next_st in
      assert_equal (curr_name) exp_curr_name;
      let opp_name = get_opponent_player_name next_st in
      assert_equal (opp_name) exp_opp_name;
    )

let init_state = init_state (0, 0) 0 ("jason","nate")

let state_tests = [
  state_test "basic init state" init_state 0 31 1 10 10 0 0 "jason" "nate";
]

let suite = 
  "test suite" >::: List.flatten [deck_tests; command_tests; state_tests]

let () = run_test_tt_main suite
