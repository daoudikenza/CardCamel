open OUnit2
open QCheck

(** Test for Flashcard module **)

open Final.Flashcard

(* Tests for Flashcard.is_valid_name*)

let make_test_is_valid_name_valid name =
  assert_equal ~printer:string_of_bool true (is_valid_name name)

let make_test_is_valid_name_invalid name =
  assert_equal ~printer:string_of_bool false (is_valid_name name)

let test_is_valid_name =
  "Test suite for is_valid_name"
  >::: [
         ( "test is_valid_name for an invalid name" >:: fun _ ->
           make_test_is_valid_name_invalid "" );
         ( "test is_valid_name for an invalid name" >:: fun _ ->
           make_test_is_valid_name_invalid "hello/world" );
         ( "test is_valid_name for a valid name" >:: fun _ ->
           make_test_is_valid_name_valid "hello" );
         ( "test is_valid_name for a valid name" >:: fun _ ->
           make_test_is_valid_name_valid "hello world" );
       ]

(* Tests for Flashcard.create_new*)

let make_test_create_new name =
  assert_equal
    ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
    [] (create_new name)

let test_create_new =
  "Test suite for create_new"
  >::: [
         ("test create_new" >:: fun _ -> make_test_create_new "hello");
         ("test create_new" >:: fun _ -> make_test_create_new "hi");
       ]

(* Tests for Flashcard.load_flashcards*)

let make_test_load_flashcards name =
  let flashcards = load_flashcards name in
  assert_equal ~printer:string_of_bool false (flashcards = [])

let test_load_flashcards =
  "Test suite for load_flashcards"
  >::: [
         ( "test load test" >:: fun _ ->
           make_test_load_flashcards "../data/math.csv" );
       ]

(* Tests for Flashcard.save_flashcards *)

let make_test_save_flashcards name flashcards =
  let () = save_flashcards name flashcards in
  let result = load_flashcards name in
  assert_equal
    ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
    result flashcards

let lst = [ "3+3"; "6"; "hello?"; "hi" ]

let test_save_flashcards =
  "Test suite for save_flashcards"
  >::: [
         ( "test save to test1.csv" >:: fun _ ->
           make_test_save_flashcards "../data/test1.csv" lst );
         ( "test save to cs3110.csv" >:: fun _ ->
           make_test_save_flashcards "../data/cs3110.csv" lst );
       ]

(* Tests for Flashcard.preview_flashcards *)

let blue = "\027[34m"
let yellow = "\027[33m"
let green = "\027[32m"
let reset = "\027[0m"

(* A mock implementation of preview_flashcards for this example. In your real
   test file, you should `open` or `require` the module where preview_flashcards
   is defined, instead of redefining it. *)

(* Helper function to capture printed output *)
let test_preview_flashcards =
  "test_string_of_flashcards"
  >::: [
         (* Test when there are no flashcards *)
         ( "test_no_flashcards" >:: fun _ ->
           let flashcards = [] in
           let expected =
             "\nNo flashcards available. Use 'add' to create new ones."
           in
           assert_equal
             ~printer:(fun x -> x)
             expected
             (string_of_flashcards flashcards) );
         (* Test with a single flashcard (question and answer pair) *)
         ( "test_single_flashcard" >:: fun _ ->
           let flashcards =
             [ "What is OCaml?"; "A functional programming language." ]
           in
           let expected =
             blue ^ "╭───────────── FLASHCARD PREVIEW ─────────────╮" ^ reset
             ^ "\n" ^ yellow ^ "Total Cards: 1\n" ^ reset ^ blue
             ^ "├───────────────────────────────────────────────┤" ^ reset ^ "\n"
             ^ green ^ "\n  Card #1\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "What is OCaml?\n" ^ blue ^ "  │ A: " ^ reset
             ^ "A functional programming language.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ blue
             ^ "\n╰───────────────────────────────────────────────╯" ^ reset
             ^ "\n" ^ "Press Enter to continue..."
           in
           assert_equal
             ~printer:(fun x -> x)
             expected
             (string_of_flashcards flashcards) );
         (* Test with multiple flashcards *)
         ( "test_multiple_flashcards" >:: fun _ ->
           let flashcards =
             [
               "What is OCaml?";
               "A functional programming language.";
               "What is Dune?";
               "A build system for OCaml.";
             ]
           in
           let expected =
             blue ^ "╭───────────── FLASHCARD PREVIEW ─────────────╮" ^ reset
             ^ "\n" ^ yellow ^ "Total Cards: 2\n" ^ reset ^ blue
             ^ "├───────────────────────────────────────────────┤" ^ reset ^ "\n"
             ^ green ^ "\n  Card #1\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "What is OCaml?\n" ^ blue ^ "  │ A: " ^ reset
             ^ "A functional programming language.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ green
             ^ "\n  Card #2\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "What is Dune?\n" ^ blue ^ "  │ A: " ^ reset
             ^ "A build system for OCaml.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ blue
             ^ "\n╰───────────────────────────────────────────────╯" ^ reset
             ^ "\n" ^ "Press Enter to continue..."
           in
           assert_equal
             ~printer:(fun x -> x)
             expected
             (string_of_flashcards flashcards) );
         (* Test with uneven list (odd number of elements) *)
         ( "test_uneven_flashcards" >:: fun _ ->
           let flashcards =
             [
               "What is OCaml?";
               "A functional programming language.";
               "What is missing?";
             ]
           in
           let expected =
             blue ^ "╭───────────── FLASHCARD PREVIEW ─────────────╮" ^ reset
             ^ "\n" ^ yellow ^ "Total Cards: 1\n" ^ reset
             (* Only one complete card *)
             ^ blue
             ^ "├───────────────────────────────────────────────┤" ^ reset ^ "\n"
             ^ green ^ "\n  Card #1\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "What is OCaml?\n" ^ blue ^ "  │ A: " ^ reset
             ^ "A functional programming language.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ blue
             ^ "\n╰───────────────────────────────────────────────╯" ^ reset
             ^ "\n" ^ "Press Enter to continue..."
           in
           assert_equal
             ~printer:(fun x -> x)
             expected
             (string_of_flashcards flashcards) );
         (* Test with flashcards that contain leading and trailing spaces *)
         ( "test_trim_flashcards" >:: fun _ ->
           let flashcards =
             [
               "   What is OCaml?  ";
               "  A functional programming language. ";
               " What is Dune?";
               "A build system for OCaml.  ";
             ]
           in
           let expected =
             blue ^ "╭───────────── FLASHCARD PREVIEW ─────────────╮" ^ reset
             ^ "\n" ^ yellow ^ "Total Cards: 2\n" ^ reset ^ blue
             ^ "├───────────────────────────────────────────────┤" ^ reset ^ "\n"
             ^ green ^ "\n  Card #1\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "What is OCaml?\n" ^ blue ^ "  │ A: " ^ reset
             ^ "A functional programming language.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ green
             ^ "\n  Card #2\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "What is Dune?\n" ^ blue ^ "  │ A: " ^ reset
             ^ "A build system for OCaml.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ blue
             ^ "\n╰───────────────────────────────────────────────╯" ^ reset
             ^ "\n" ^ "Press Enter to continue..."
           in
           assert_equal
             ~printer:(fun x -> x)
             expected
             (string_of_flashcards flashcards) );
         (* Test with special characters in flashcards *)
         ( "test_special_characters_flashcards" >:: fun _ ->
           let flashcards =
             [
               "What does ♥ mean?";
               "It means love.";
               "Use of \\n?";
               "A newline character.";
             ]
           in
           let expected =
             blue ^ "╭───────────── FLASHCARD PREVIEW ─────────────╮" ^ reset
             ^ "\n" ^ yellow ^ "Total Cards: 2\n" ^ reset ^ blue
             ^ "├───────────────────────────────────────────────┤" ^ reset ^ "\n"
             ^ green ^ "\n  Card #1\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "What does ♥ mean?\n" ^ blue ^ "  │ A: "
             ^ reset ^ "It means love.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ green
             ^ "\n  Card #2\n" ^ reset ^ blue
             ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n" ^ blue
             ^ "  │ Q: " ^ reset ^ "Use of \\n?\n" ^ blue ^ "  │ A: " ^ reset
             ^ "A newline character.\n" ^ blue
             ^ "  └──────────────────────────────────┘" ^ reset ^ "\n" ^ blue
             ^ "\n╰───────────────────────────────────────────────╯" ^ reset
             ^ "\n" ^ "Press Enter to continue..."
           in
           assert_equal
             ~printer:(fun x -> x)
             expected
             (string_of_flashcards flashcards) );
       ]
(* Tests for Flashcard.normalize*)

let make_test_normalize str1 str2 =
  let new_str = normalize str1 in
  assert_equal ~printer:(fun x -> x) str2 new_str

let test_normalize =
  "Test suite for normalize"
  >::: [
         ( "test normalize a trivial string" >:: fun _ ->
           make_test_normalize "Hello" "hello" );
         ( "test normalize an all capital string" >:: fun _ ->
           make_test_normalize "WORLD" "world" );
         ( "test normalize a string with space" >:: fun _ ->
           make_test_normalize "Hello World" "hello world" );
         ( "test normalize a string with number" >:: fun _ ->
           make_test_normalize "Cs3110" "cs3110" );
         ( "test normalize a string with symbol" >:: fun _ ->
           make_test_normalize "Hello &World!" "hello &world!" );
       ]

(* Test for Flashcards.shuffle*)

let make_test_shuffle flashcards =
  let shuffled = shuffle flashcards in
  assert_equal ~printer:string_of_bool false (flashcards = shuffled)

let make_test_shuffle_empty flashcards =
  let shuffled = shuffle flashcards in
  assert_equal
    ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
    shuffled []

let to_shuffle = [ "hello"; "yo"; "what's up?" ]

let test_shuffle =
  "Test suite for shuffle"
  >::: [
         ( "test shuffle for a non-empty list" >:: fun _ ->
           make_test_shuffle to_shuffle );
         ( "test shuffle for an empty list" >:: fun _ ->
           make_test_shuffle_empty [] );
       ]

(* Test for Flashcard.percentage_of*)

let make_test_percentage_of completed total expected =
  let percentage = percentage_of completed total in
  assert_equal
    ~printer:(function
      | Some i -> string_of_int i
      | None -> "None")
    percentage expected

let test_percentage_of =
  "Test suite for percentage_of"
  >::: [
         ("test when total is 0" >:: fun _ -> make_test_percentage_of 10 0 None);
         ( "test when total is not 0 and completed is above 75%" >:: fun _ ->
           make_test_percentage_of 80 100 (Some 75) );
         ( "test when total is not 0 and completed is between 50% and 75%"
         >:: fun _ -> make_test_percentage_of 55 100 (Some 50) );
         ( "test when total is not 0 and completed is between 25% and 75%"
         >:: fun _ -> make_test_percentage_of 32 100 (Some 25) );
         ( "test when total is not 0 and completed is between 0 and 25%"
         >:: fun _ -> make_test_percentage_of 15 100 None );
       ]

(* Test Flashcard.group_into_pairs*)

(* let make_test_group_into_pairs lst new_lst = let lst1 = group_into_pairs lst
   in assert_equal new_lst lst1

   let test_group_into_pairs = "Test suite for group_into_pairs" >::: [ ( "test
   group_into_pair an empty list" >:: fun _ -> make_test_group_into_pairs [] []
   ); ( "test group_into_pair a list of length 1" >:: fun _ ->
   make_test_group_into_pairs [ "hello" ] [] ); ( "test group_into_pair a list of
   length 2" >:: fun _ -> make_test_group_into_pairs [ "hello"; "world" ] [
   ("hello", "world") ] ); ( "test group_into_pair a list of length 3" >:: fun _
   -> make_test_group_into_pairs [ "hello"; "world"; "cs3110" ] [ ("hello",
   "world") ] ); ( "test group_into_pair a list of length 4" >:: fun _ ->
   make_test_group_into_pairs [ "hello"; "world"; "cs3110"; "OCaml" ] [ ("hello",
   "world"); ("cs3110", "OCaml") ] ); ] *)

let tests_contains_substring =
  "Test suite for contains_substring"
  >::: [
         (* Basic cases *)
         ( "simple match" >:: fun _ ->
           assert_equal true (contains_substring "hello world" "world") );
         ( "no match" >:: fun _ ->
           assert_equal false (contains_substring "hello world" "planet") );
         (* Edge cases *)
         ( "empty substring" >:: fun _ ->
           assert_equal true (contains_substring "hello" "") );
         ( "empty string" >:: fun _ ->
           assert_equal false (contains_substring "" "test") );
         ( "exact match" >:: fun _ ->
           assert_equal true (contains_substring "test" "test") );
         (* Substring longer than string *)
         ( "substring longer than string" >:: fun _ ->
           assert_equal false (contains_substring "hi" "hello") );
         (* Case sensitivity *)
         ( "case sensitive match" >:: fun _ ->
           assert_equal false (contains_substring "Hello World" "hello") );
         (* Overlapping substrings *)
         ( "overlapping match" >:: fun _ ->
           assert_equal true (contains_substring "aaaaaa" "aaa") );
         (* Substring at the edges *)
         ( "substring at start" >:: fun _ ->
           assert_equal true (contains_substring "hello world" "hello") );
         ( "substring at end" >:: fun _ ->
           assert_equal true (contains_substring "hello world" "world") );
       ]

(* Helper to clean up files *)
let cleanup filename = if Sys.file_exists filename then Sys.remove filename

(* Test 1: Save score successfully *)
let test_save_simple_high_score () =
  let set_name = "test_set" in
  let score = 42 in
  let filename = "high_score_" ^ set_name ^ ".txt" in
  cleanup filename;

  (* Ensure test starts with no pre-existing file *)
  save_high_score set_name score;

  (* Assert file exists *)
  "test save high score simple"
  >::: [
         ( "test file should exist" >:: fun _ ->
           assert_bool "File should exist after saving high score"
             (Sys.file_exists filename) );
         (* Assert file contains correct score *)
         ( "test the score" >:: fun _ ->
           let ic = open_in filename in
           let content = input_line ic in
           close_in ic;

           assert_equal ~cmp:String.equal (string_of_int score) content;

           (* Cleanup file after test *)
           cleanup filename );
       ]

(* Test 2: Handle overwriting a file *)
let test_overwrite_high_score () =
  let set_name = "overwrite_set" in
  let initial_score = 10 in
  let new_score = 100 in
  let filename = "high_score_" ^ set_name ^ ".txt" in
  cleanup filename;
  (* Save initial score *)
  save_high_score set_name initial_score;
  (* Save new score *)
  save_high_score set_name new_score;
  "test_save_high_score_overwrite"
  >::: [
         (* Assert file exists *)
         ( "test_file_exist" >:: fun _ ->
           assert_bool "File should exist after saving high\n   score"
             (Sys.file_exists filename);

           let ic = open_in filename in
           let content = input_line ic in
           close_in ic;

           (* Assert the most recent score is stored *)
           assert_equal ~cmp:String.equal (string_of_int new_score) content;

           (* Cleanup after test *) cleanup filename );
       ]

(* Test 3: Edge case with score 0 *)
let test_save_edge_case () =
  let set_name = "edge_case_set" in
  let score = 0 in
  let filename = "high_score_" ^ set_name ^ ".txt" in

  cleanup filename;

  save_high_score set_name score;
  "test save high score edge case"
  >::: [
         ( "test file exists" >:: fun _ ->
           (* Assert file exists *)
           assert_bool "File should exist after saving high\n   score"
             (Sys.file_exists filename) );
         ( "the the score" >:: fun _ ->
           let ic = open_in filename in
           let content = input_line ic in
           close_in ic;

           assert_equal ~cmp:String.equal "0" content;

           cleanup filename );
       ]

(* Test Suite *)
let test_save_high_score =
  "High Score Tests"
  >::: [
         test_save_simple_high_score ();
         test_overwrite_high_score ();
         test_save_edge_case ();
       ]

let _ = run_test_tt_main test_save_high_score

(* Helper to create a test file *)
let create_test_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

(* Test 1: Load high score successfully *)
let test_load_high_score_success () =
  let set_name = "test_set" in
  let score = 42 in
  let filename = "high_score_" ^ set_name ^ ".txt" in

  cleanup filename;
  (* Ensure no pre-existing file *)
  create_test_file filename (string_of_int score);

  (* Test loading the high score *)
  let loaded_score = load_high_score set_name in
  "test load score"
  >::: [
         ( "test_successful_load" >:: fun _ ->
           assert_equal ~printer:string_of_int score loaded_score;

           cleanup filename );
       ]

(* Test 2: Handle missing file gracefully *)
let test_load_high_score_missing_file () =
  let set_name = "missing_set" in
  let filename = "high_score_" ^ set_name ^ ".txt" in

  cleanup filename;

  (* Ensure file doesn't exist *)

  (* Test loading the high score when file is missing *)
  let loaded_score = load_high_score set_name in
  "test load high score missing"
  >::: [
         ( "test missing" >:: fun _ ->
           assert_equal ~printer:string_of_int 0 loaded_score );
       ]

(* Test 3: Handle corrupted file gracefully *)
let test_load_high_score_corrupted_file () =
  let set_name = "corrupted_set" in
  let filename = "high_score_" ^ set_name ^ ".txt" in

  cleanup filename;
  (* Ensure no pre-existing file *)
  create_test_file filename "not_a_number";

  (* Test loading the high score when file is corrupted *)
  let loaded_score = load_high_score set_name in
  "test load high score corrupt"
  >::: [
         ( "test_corrupt" >:: fun _ ->
           assert_equal ~printer:string_of_int 0 loaded_score;

           cleanup filename );
       ]

(* Test Suite *)
let test_load_high_score =
  "Load High Score Tests"
  >::: [
         test_load_high_score_success ();
         test_load_high_score_missing_file ();
         test_load_high_score_corrupted_file ();
       ]

(* Test cases *)
let test_update_high_score_higher () =
  let set_name = "test_set" in
  let filename = "high_score_" ^ set_name ^ ".txt" in

  (* Setup *)
  cleanup filename;
  create_test_file filename "50";

  (* Current high score = 50 *)

  (* Perform test *)
  check_and_update_high_score set_name 100;

  (* Verify *)
  let updated_score = load_high_score set_name in
  "test check update simple"
  >::: [
         ( "test update score" >:: fun _ ->
           assert_equal ~printer:string_of_int 100 updated_score;

           (* Cleanup *)
           cleanup filename );
       ]

let test_no_update_high_score_lower () =
  let set_name = "test_set" in
  let filename = "high_score_" ^ set_name ^ ".txt" in

  (* Setup *)
  cleanup filename;
  create_test_file filename "50";

  (* Current high score = 50 *)

  (* Perform test *)
  check_and_update_high_score set_name 30;

  (* Verify *)
  let updated_score = load_high_score set_name in
  "test no update"
  >::: [
         ( "test update score" >:: fun _ ->
           assert_equal ~printer:string_of_int 50 updated_score;

           (* Cleanup *)
           cleanup filename );
       ]

let test_update_high_score_no_existing () =
  let set_name = "new_test_set" in
  let filename = "high_score_" ^ set_name ^ ".txt" in

  (* Setup *)
  cleanup filename;

  (* Ensure no pre-existing file *)

  (* Perform test *)
  check_and_update_high_score set_name 70;

  (* Verify *)
  let updated_score = load_high_score set_name in
  "test no existing score"
  >::: [
         ( "test update score" >:: fun _ ->
           assert_equal ~printer:string_of_int 70 updated_score;

           (* Cleanup *)
           cleanup filename );
       ]

(* Collect tests into a suite *)
let test_check_update_high_score =
  "check_and_update_high_score_suite"
  >::: [
         test_update_high_score_higher ();
         test_no_update_high_score_lower ();
         test_update_high_score_no_existing ();
       ]

let test_contains_substring_present =
  Test.make ~name:"contains_substring returns true when substring is present"
    (pair
       (make ~print:Print.string Gen.string)
       (make ~print:Print.string Gen.string))
    (fun (s, substr) ->
      let s = s ^ substr in
      (* Ensure substr is in s *)
      contains_substring s substr)

let test_contains_substring_equal =
  Test.make ~name:"contains_substring returns true when substring equals string"
    (make ~print:Print.string Gen.string) (fun s -> contains_substring s s)

let test_contains_substring_empty_substr =
  Test.make ~name:"contains_substring returns true when substring is empty"
    (make ~print:Print.string Gen.string) (fun s -> contains_substring s "")

(* Run tests *)

let () =
  let qcheck_tests =
    List.map QCheck_ounit.to_ounit2_test
      [
        test_contains_substring_present;
        test_contains_substring_equal;
        test_contains_substring_empty_substr;
      ]
  in
  List.iter run_test_tt_main
    ([
       test_is_valid_name;
       test_create_new;
       test_load_flashcards;
       test_save_flashcards;
       test_preview_flashcards;
       test_normalize;
       (* test_group_into_pairs; *)
       test_percentage_of;
       test_shuffle;
       tests_contains_substring;
       (* test_save_high_score; *)
       test_load_high_score;
       test_check_update_high_score;
     ]
    @ qcheck_tests)

(** Tests for Final.Asteroid *)

open Final.Asteroid

let string_of_int_pair (x, y) = Printf.sprintf "(%d, %d)" x y

(* Test for Asteroid.create *)
let make_test_asteroid_create content col =
  let asteroid = create content col in
  assert_equal ~printer:(fun x -> x) content (get_content asteroid);
  assert_equal ~printer:string_of_int_pair (0, col) (get_position asteroid)

let test_asteroid_create =
  "Test suite for asteroid create"
  >::: [
         ("test_asteroid_create1" >:: fun _ -> make_test_asteroid_create "ast1" 5);
         ("test_asteroid_create2" >:: fun _ -> make_test_asteroid_create "yoyo" 0);
         ( "test_asteroid_create_empty" >:: fun _ ->
           make_test_asteroid_create "" 3 );
         ( "test_asteroid_create_long_content" >:: fun _ ->
           make_test_asteroid_create "long_content_string" 10 );
         ( "test_asteroid_create_special_chars" >:: fun _ ->
           make_test_asteroid_create "@#$%^&*" 7 );
         ( "test_asteroid_create_negative_col" >:: fun _ ->
           make_test_asteroid_create "test" (-1) );
         ( "test_asteroid_create_max_int_col" >:: fun _ ->
           make_test_asteroid_create "max_col" max_int );
       ]

(* Test for Asteroid.move *)
let make_test_asteroid_move name =
  let asteroid = create name 5 in
  let moved_asteroid = move asteroid (get_speed asteroid) in
  let expected_row = 1 in
  let row, _ = get_position moved_asteroid in
  assert_equal ~printer:string_of_int expected_row row

let test_asteroid_move =
  "Test suite for asteroid move"
  >::: [
         ("test_asteroid_move1" >:: fun _ -> make_test_asteroid_move "ast1");
         ("test_asteroid_move2" >:: fun _ -> make_test_asteroid_move "yoyo");
         ("test_asteroid_move_empty_name" >:: fun _ -> make_test_asteroid_move "");
         ( "test_asteroid_move_special_chars" >:: fun _ ->
           make_test_asteroid_move "@#$%^&*" );
         ( "test_asteroid_move_long_name" >:: fun _ ->
           make_test_asteroid_move "very_long_asteroid_name" );
       ]

(* Test for Asteroid.destroyed *)
let make_test_asteroid_destroyed name time_taken =
  let asteroid = create name 5 in
  let score = destroyed asteroid time_taken in
  assert_bool "Score should be positive" (score > 0)

let test_asteroid_destroyed =
  "Test suite for asteroid destroyed"
  >::: [
         ( "test_asteroid_destroyed_fast" >:: fun _ ->
           make_test_asteroid_destroyed "fast_destroy" 0.5 );
         ( "test_asteroid_destroyed_normal" >:: fun _ ->
           make_test_asteroid_destroyed "normal_destroy" 2.0 );
         ( "test_asteroid_destroyed_slow" >:: fun _ ->
           make_test_asteroid_destroyed "slow_destroy" 5.0 );
         (* ( "test_asteroid_destroyed_negative_time" >:: fun _ ->
            make_test_asteroid_destroyed "negative_time" (-1.0) ); *)
       ]

(* Test for Asteroid.to_string*)
let make_test_to_string name col symbol =
  let asteroid = create name col in
  let expected = symbol in
  let as_string = to_string asteroid in
  assert_equal as_string expected

let test_asteroid_to_string =
  "Test suite for to_string"
  >::: [
         ("test_to_string_*" >:: fun _ -> make_test_to_string "hello" 5 "*");
         ("test_to_string_@" >:: fun _ -> make_test_to_string "abc" 7 "@");
         ("test_to_string_#" >:: fun _ -> make_test_to_string "xyzhls" 3 "#");
       ]

let asteroid1 = create "asteroid1" 5
let asteroid2 = create "asteroid2" 10
let asteroid3 = create "asteroid3" 15
let asteroid4 = create "asteroid4" 20
let asteroid5 = create "asteroid5" 25
let asteroid6 = create "asteroid6" 30

(* Update test_asteroid_get_content *)
let test_asteroid_get_content =
  "Test suite for get_content"
  >::: [
         ( "test for asteroid1" >:: fun _ ->
           assert_equal ~msg:"Content mismatch for asteroid1" "asteroid1"
             (get_content asteroid1) );
         ( "test for asteroid2" >:: fun _ ->
           assert_equal ~msg:"Content mismatch for asteroid2" "asteroid2"
             (get_content asteroid2) );
         ( "test for asteroid3" >:: fun _ ->
           assert_equal ~msg:"Content mismatch for asteroid3" "asteroid3"
             (get_content asteroid3) );
         ( "test for asteroid4" >:: fun _ ->
           assert_equal ~msg:"Content mismatch for asteroid4" "asteroid4"
             (get_content asteroid4) );
         ( "test for asteroid5" >:: fun _ ->
           assert_equal ~msg:"Content mismatch for asteroid5" "asteroid5"
             (get_content asteroid5) );
         ( "test for asteroid6" >:: fun _ ->
           assert_equal ~msg:"Content mismatch for asteroid6" "asteroid6"
             (get_content asteroid6) );
       ]

(* Update test_asteroid_get_position *)
let test_asteroid_get_position =
  "Test suite for get_position"
  >::: [
         ( "test for asteroid1" >:: fun _ ->
           assert_equal ~msg:"Position mismatch for asteroid1" (0, 5)
             (get_position asteroid1) );
         ( "test for asteroid2" >:: fun _ ->
           assert_equal ~msg:"Position mismatch for asteroid2" (0, 10)
             (get_position asteroid2) );
         ( "test for asteroid3" >:: fun _ ->
           assert_equal ~msg:"Position mismatch for asteroid3" (0, 15)
             (get_position asteroid3) );
         ( "test for asteroid4" >:: fun _ ->
           assert_equal ~msg:"Position mismatch for asteroid4" (0, 20)
             (get_position asteroid4) );
         ( "test for asteroid5" >:: fun _ ->
           assert_equal ~msg:"Position mismatch for asteroid5" (0, 25)
             (get_position asteroid5) );
         ( "test for asteroid6" >:: fun _ ->
           assert_equal ~msg:"Position mismatch for asteroid6" (0, 30)
             (get_position asteroid6) );
       ]

(* Update test_get_creation_time *)
let test_get_creation_time _ =
  (* Test get_creation_time returns a valid time *)
  let time1 = get_creation_time asteroid1 in
  let time2 = get_creation_time asteroid2 in
  let time3 = get_creation_time asteroid3 in
  let time4 = get_creation_time asteroid4 in
  let time5 = get_creation_time asteroid5 in
  let time6 = get_creation_time asteroid6 in
  let now = Unix.gettimeofday () in

  assert_bool "Creation time for asteroid1 is invalid"
    (time1 <= now && time1 > 0.);
  assert_bool "Creation time for asteroid2 is invalid"
    (time2 <= now && time2 > 0.);
  assert_bool "Creation time for asteroid3 is invalid"
    (time3 <= now && time3 > 0.);
  assert_bool "Creation time for asteroid4 is invalid"
    (time4 <= now && time4 > 0.);
  assert_bool "Creation time for asteroid5 is invalid"
    (time5 <= now && time5 > 0.);
  assert_bool "Creation time for asteroid6 is invalid"
    (time6 <= now && time6 > 0.)

let test_asteroid_get_creation_time =
  "Test suite for get_creation_time"
  >::: [ "test_get_creation_time" >:: test_get_creation_time ]

(* Update test_get_speed *)
let test_get_speed _ =
  (* Test get_speed returns a valid speed *)
  let speed1 = get_speed asteroid1 in
  let speed2 = get_speed asteroid2 in
  let speed3 = get_speed asteroid3 in
  let speed4 = get_speed asteroid4 in
  let speed5 = get_speed asteroid5 in
  let speed6 = get_speed asteroid6 in

  assert_bool "Speed for asteroid1 is out of range" (speed1 >= 1 && speed1 <= 3);
  assert_bool "Speed for asteroid2 is out of range" (speed2 >= 1 && speed2 <= 3);
  assert_bool "Speed for asteroid3 is out of range" (speed3 >= 1 && speed3 <= 3);
  assert_bool "Speed for asteroid4 is out of range" (speed4 >= 1 && speed4 <= 3);
  assert_bool "Speed for asteroid5 is out of range" (speed5 >= 1 && speed5 <= 3);
  assert_bool "Speed for asteroid6 is out of range" (speed6 >= 1 && speed6 <= 3)

let test_asteroid_get_speed =
  "Test suite for get_speed" >::: [ "test_get_speed" >:: test_get_speed ]

(* Test assign_symbol *)
let test_assign_symbol =
  Test.make ~name:"assign_symbol returns a valid symbol"
    (make ~print:Print.string Gen.string) (fun content ->
      let symbol = assign_symbol content in
      List.mem symbol [ "@"; "#"; "*" ])

(* Test create *)
let test_create =
  Test.make ~name:"create initializes asteroid with valid properties"
    (pair (make ~print:Print.string Gen.string) (make ~print:Print.int Gen.int))
    (fun (content, col) ->
      let asteroid = create content col in
      get_speed asteroid >= 1
      && get_speed asteroid <= 3
      && fst (get_position asteroid) = 0
      && snd (get_position asteroid) = col)

let () =
  let qcheck_tests =
    List.map QCheck_ounit.to_ounit2_test [ test_assign_symbol; test_create ]
  in
  List.iter run_test_tt_main
    ([
       test_asteroid_create;
       test_asteroid_move;
       test_asteroid_destroyed;
       test_asteroid_to_string;
       test_asteroid_get_content;
       test_asteroid_get_position;
       test_asteroid_get_creation_time;
       test_asteroid_get_speed;
     ]
    @ qcheck_tests)
    
(** Tests for Final.Board *)
open Final.Board

let string_of_asteroid asteroid =
  Printf.sprintf "{content = %s; position = (%d, %d)}" (get_content asteroid)
    (fst (get_position asteroid))
    (snd (get_position asteroid))

let string_of_ast_list ast_list =
  "[" ^ String.concat "; " (List.map string_of_asteroid ast_list) ^ "]"
(* Test for Board.initialize *)
let make_test_initialize rows cols ast_list =
  let board = initialize rows cols ast_list in
  assert_equal ~printer:string_of_int (get_rows board) rows;
  assert_equal ~printer:string_of_int (get_cols board) cols;
  assert_equal ~printer:string_of_ast_list (get_asteroids board) ast_list

let asteroids_list = [ create "Hello?" 1; create "What?" 2; create "Who?" 3 ]

let test_initialize =
  "Test suite for initialize"
  >::: [
         ( "test_initialize_zero_empty_board" >:: fun _ ->
           make_test_initialize 0 0 [] );
         ("test_initialize" >:: fun _ -> make_test_initialize 5 3 asteroids_list);
         ( "test_initialize_zero_board" >:: fun _ ->
           make_test_initialize 0 0 asteroids_list );
         ("test_initialize_empty_board" >:: fun _ -> make_test_initialize 5 3 []);
       ]

(* Test for Board.add_asteroid*)
let make_test_add_asteroid rows cols ast_list asteroid =
  let board = initialize rows cols ast_list in
  let new_board = add_asteroid board asteroid in
  assert_equal ~printer:string_of_ast_list (get_asteroids new_board)
    (asteroid :: ast_list);
  assert_equal (get_rows new_board) (get_rows board);
  assert_equal (get_cols new_board) (get_cols board)

let test_add_asteroid =
  "Test suite for add_asteroid"
  >::: [
         ( "test_initialize" >:: fun _ ->
           make_test_add_asteroid 5 5 asteroids_list (create "Yo" 4) );
         ( "test_initialize" >:: fun _ ->
           make_test_add_asteroid 0 0 [] (create "Yo" 4) );
       ]

(* Test for Board.get_rows*)
let make_test_get_rows rows cols =
  let board = initialize rows cols asteroids_list in
  let actual = get_rows board in
  assert_equal ~printer:string_of_int actual rows

(* Test for Board.get_cols *)
let make_test_get_cols rows cols =
  let board = initialize rows cols asteroids_list in
  let actual = get_cols board in
  assert_equal ~printer:string_of_int actual cols

let test_dimensions =
  "Test suite for get_rows and get_cols"
  >::: [
         ("test_get_rows" >:: fun _ -> make_test_get_rows 5 3);
         ("test_get_cols" >:: fun _ -> make_test_get_cols 0 6);
         ("test_get_rows" >:: fun _ -> make_test_get_rows 0 6);
         ("test_get_cols" >:: fun _ -> make_test_get_cols 5 3);
       ]

(* Test for Board.move_asteroids *)
let make_test_move_asteroids board frame_number =
  let new_board = move_asteroids board frame_number in
  let old_asteroids = get_asteroids board in
  let new_asteroids = get_asteroids new_board in
  assert_equal (get_cols board) (get_cols new_board);
  assert_equal (get_rows board) (get_rows new_board);
  assert_equal ~printer:string_of_bool (board = new_board) false;
  assert_equal ~printer:string_of_bool (old_asteroids = new_asteroids) false

let test_move_asteroids =
  "Test suite for move_asteroids"
  >::: [
         ( "test_move_asteroids" >:: fun _ ->
           make_test_move_asteroids (initialize 5 4 asteroids_list) 1 );
         ( "test_move_asteroids" >:: fun _ ->
           make_test_move_asteroids (initialize 5 4 asteroids_list) 0 );
       ]

(* Test for Board.check_collision*)
let make_test_check_collision board =
  let updated_board = check_collision board in
  assert_equal ~printer:string_of_bool (snd updated_board = board) false

let test_check_collisions =
  "Test suite for check_collisions"
  >::: [
         ( "test_check_collisions" >:: fun _ ->
           make_test_check_collision (initialize 0 0 asteroids_list) );
         ( "test_check_collisions" >:: fun _ ->
           make_test_check_collision (initialize 5 4 asteroids_list) );
       ]

(* Test for Board.resize *)
let make_test_resize board new_rows new_cols =
  let new_board = resize board new_rows new_cols in
  assert_equal ~printer:string_of_int (get_cols new_board) new_cols;
  assert_equal ~printer:string_of_int (get_rows new_board) new_cols;
  assert_equal (get_asteroids new_board) (get_asteroids board)

let test_resize =
  "Test suite for resize"
  >::: [
         ( "test_resize" >:: fun _ ->
           make_test_resize (initialize 5 4 asteroids_list) 5 5 );
         ( "test_resize" >:: fun _ ->
           make_test_resize (initialize 0 0 asteroids_list) 5 5 );
       ]

(* Test for Board.get_dimensions *)
let make_test_get_dimensions board =
  let dimensions = get_dimensions board in
  assert_equal ~printer:string_of_int_pair dimensions
    (get_rows board, get_cols board)

let test_get_dimensions =
  "Test suite for get_dimensions "
  >::: [
         ( "test_get_dimensions" >:: fun _ ->
           make_test_get_dimensions (initialize 5 4 asteroids_list) );
         ( "test_get_dimensions" >:: fun _ ->
           make_test_get_dimensions (initialize 0 0 asteroids_list) );
         ( "test_get_dimensions" >:: fun _ ->
           make_test_get_dimensions (initialize 5 4 []) );
         ( "test_get_dimensions" >:: fun _ ->
           make_test_get_dimensions (initialize 0 0 []) );
       ]

(* Test for Board.get_asteroids *)
let make_test_get_asteroids rows cols asteroids_list =
  let board = initialize rows cols asteroids_list in
  let asteroids = get_asteroids board in
  assert_equal ~printer:string_of_ast_list asteroids asteroids_list

let test_get_asteroids =
  "Test suite for get_asteroids"
  >::: [
         ( "test_get_asteroids" >:: fun _ ->
           make_test_get_asteroids 2 4 asteroids_list );
         ("test_get_asteroids" >:: fun _ -> make_test_get_asteroids 5 4 []);
       ]

let capture_render_output f =
  let buffer = Buffer.create 256 in
  let original_stdout = Unix.dup Unix.stdout in
  Unix.dup2 (Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o640) Unix.stdout;
  f ();
  (* Call the render function *)
  Unix.dup2 original_stdout Unix.stdout;
  Unix.close original_stdout;
  Buffer.contents buffer

(* Simplified test for render function *)
let test_render _ =
  let rows = 2 in
  let cols = 3 in
  let board = initialize rows cols [] in
  let output = capture_render_output (fun () -> render board) in

  assert_equal "" output
    ~msg:"Render output should match expected for an empty board"

(* Test for Board.update_asteroids*)
let make_test_update_asteroids board asteroids_list =
  let new_board = update_asteroids board asteroids_list in
  assert_equal (get_cols board) (get_cols new_board);
  assert_equal (get_rows board) (get_rows new_board);
  assert_equal (get_asteroids new_board) asteroids_list

let test_update_asteroids =
  "Test suite for update_asteroids"
  >::: [
         ( "test_update_asteroids" >:: fun _ ->
           make_test_update_asteroids (initialize 5 4 []) asteroids_list );
         ( "test_update_asteroids" >:: fun _ ->
           make_test_update_asteroids (initialize 0 0 []) asteroids_list );
         ( "test_update_asteroids" >:: fun _ ->
           make_test_update_asteroids (initialize 5 4 asteroids_list) [] );
         "test_render" >:: test_render;
         ( "test_update_asteroids" >:: fun _ ->
           make_test_update_asteroids (initialize 0 0 asteroids_list) [] );
       ]

let () =
  List.iter run_test_tt_main
    [
      test_initialize;
      test_add_asteroid;
      test_dimensions;
      test_move_asteroids;
      test_check_collisions;
      test_resize;
      test_get_dimensions;
      test_get_asteroids;
      test_update_asteroids;
    ]

(* Tests for Final.Game*)

open Final.Game

(* Test for Game.update *)

(* let make_test_update_game flashcards rows cols fps = let game = initialize
   flashcards rows cols fps in let updated = update game in assert_equal game
   updated

   let test_update_game = "Test suite for updating game" >::: [
   ("test_for_update_game" >:: fun _ -> make_test_update_game [] 5 4 1.); (
   "test_for_update_game" >:: fun _ -> make_test_update_game [ "hello"; "How are
   you?"; "How's life?" ] 6 3 2. ); ] *)

(* Test for Game.game_over*)

let make_test_game_over_false flashcards rows cols fps =
  let game = initialize flashcards rows cols fps in
  let result = game_over game in
  assert_equal ~printer:string_of_bool result false

let test_game_over =
  "Test suite game_over"
  >::: [
         ( "test_for_game_over" >:: fun _ ->
           make_test_game_over_false [ "hello"; "yp"; "whatsup" ] 5 4 1. );
       ]

let () = List.iter run_test_tt_main [ test_game_over ]
let string_of_string_option = function
  | Some s -> Printf.sprintf "Some \"%s\"" s
  | None -> "None"
(* Test for Game.set_motivational_message*)
let make_test_set_motivational_message flashcards rows cols fps msg =
  let game = initialize flashcards rows cols fps in
  let new_game = set_motivational_message game msg in
  let expected = get_motivational_message new_game in
  assert_equal ~printer:string_of_string_option expected msg

let test_set_motivational_message =
  "Test suite game_over"
  >::: [
         ( "test_for_set_motivational_msg_None" >:: fun _ ->
           make_test_set_motivational_message
             [ "hello"; "yp"; "whatsup" ]
             5 4 1. None );
         ( "test_for_set_motivational_msg_None" >:: fun _ ->
           make_test_set_motivational_message
             [ "hello"; "yp"; "whatsup" ]
             5 4 1. (Some "Hello, how are you?") );
       ]

(* Test for Game.set_score*)
let make_test_set_score flashcards rows cols fps score =
  let game = initialize flashcards rows cols fps in
  let new_game = set_score game score in
  let expected = get_score new_game in
  assert_equal ~printer:string_of_int expected score

let test_set_score =
  "Test suite set_score"
  >::: [
         ( "test_for_set_motivational_msg_None" >:: fun _ ->
           make_test_set_score [ "hello"; "yp"; "whatsup" ] 5 4 1. 6 );
         ( "test_for_set_motivational_msg_None" >:: fun _ ->
           make_test_set_score [ "hello"; "yp"; "whatsup" ] 5 4 1. 67 );
       ]

(* Test for Game.set_lives*)
let make_test_set_lives flashcards rows cols fps score =
  let game = initialize flashcards rows cols fps in
  let new_game = set_lives game score in
  let expected = get_lives new_game in
  assert_equal ~printer:string_of_int expected score

let test_set_lives =
  "Test suite set_lives"
  >::: [
         ( "test_for_set_motivational_msg_None" >:: fun _ ->
           make_test_set_lives [ "hello"; "yp"; "whatsup" ] 5 4 1. 6 );
         ( "test_for_set_motivational_msg_None" >:: fun _ ->
           make_test_set_lives [ "hello"; "yp"; "whatsup" ] 5 4 1. 67 );
       ]

(* Test for Game.initialize *)
let make_test_initialize rows cols fps =
  let game = initialize [] rows cols fps in
  let result_flashcards = get_flashcards game in
  let result_fps = get_fps game in
  assert_equal result_flashcards [];
  assert_equal result_fps fps

let test_initialize =
  "Test suite for initializing a game"
  >::: [ ("test_for_initialize_game" >:: fun _ -> make_test_initialize 5 4 1.) ]

(* Test for Game.game_over*)

let make_test_game_over flashcards rows cols fps =
  let game = initialize flashcards rows cols fps in
  let result = game_over game in
  assert_equal ~printer:string_of_bool result false

let make_test_game_over_true flashcards rows cols fps =
  let game = initialize flashcards rows cols fps in
  let result = game_over game in
  assert_equal ~printer:string_of_bool result true

let test_game_over =
  "Test suite game_over"
  >::: [
         ( "test_for_game_over" >:: fun _ ->
           make_test_game_over [ "hello"; "yp"; "whatsup" ] 5 4 1. );
         ( "test_for_empty_flashcards" >:: fun _ ->
           make_test_game_over_true [] 5 4 1. );
         ( "test_for_single_flashcard" >:: fun _ ->
           make_test_game_over_true [ "single" ] 5 4 1. );
         ( "test_for_large_flashcard_list" >:: fun _ ->
           make_test_game_over
             [ "Q1"; "Q2"; "Q3"; "Q4"; "Q5"; "Q6"; "Q7"; "Q8"; "Q9"; "Q10" ]
             10 10 1. );
         ( "test_for_high_fps" >:: fun _ ->
           make_test_game_over_true [ "fast" ] 5 5 60.0 );
         ( "test_for_no_rows" >:: fun _ ->
           make_test_game_over_true [ "no rows" ] 0 5 1. );
         ( "test_for_no_cols" >:: fun _ ->
           make_test_game_over_true [ "no cols" ] 5 0 1. );
         ( "test_for_zero_fps" >:: fun _ ->
           make_test_game_over_true [ "zero fps" ] 5 5 0. );
         ( "test_for_special_characters" >:: fun _ ->
           make_test_game_over_true [ "@#$%^&*()!" ] 5 5 1. );
         ( "test_for_empty_strings" >:: fun _ ->
           make_test_game_over [ ""; "" ] 5 5 1. );
         ( "test_for_long_flashcard_strings" >:: fun _ ->
           make_test_game_over
             [ String.make 100 'A'; String.make 200 'B' ]
             5 5 1. );
         ( "test_for_fractional_fps" >:: fun _ ->
           make_test_game_over_true [ "fractional fps" ] 5 5 0.5 );
         ( "test_for_very_high_fps" >:: fun _ ->
           make_test_game_over_true [ "very high fps" ] 5 5 1000. );
       ]

let cleanup_test_file filename =
  let full_path = "../../../data/" ^ filename in
  if Sys.file_exists full_path then Sys.remove full_path

(* Create the data directory if it doesn't exist *)
let ensure_data_dir () =
  if not (Sys.file_exists "../../../data/") then
    Unix.mkdir "../../../data/" 0o755

(* Test importing a non-existent file *)
let test_import_nonexistent_file _ =
  let result = import_set_from_path "nonexistent_file.csv" in
  assert_equal
    ~msg:
      ("Import of nonexistent file should return None, but got: "
      ^
      match result with
      | Some s -> s
      | None -> "None")
    None result

(* Test importing a file that already exists *)
let test_import_existing_file _ =
  ensure_data_dir ();

  (* First, create a file in the data directory *)
  let test_file = "../../../data/duplicate.csv" in
  let lines = BatFile.lines_of "../../../../test.csv" in
  BatFile.write_lines test_file lines;

  (* Try to import the same file again *)
  let result = import_set_from_path "../../test.csv" in
  assert_equal
    ~msg:
      ("Importing an existing file should\n   return None, but got: "
      ^
      match result with
      | Some s -> s
      | None -> "None")
    None result;

  (* Clean up *) cleanup_test_file "duplicate.csv"

(* Test deleting a non-existent set *)
let test_delete_nonexistent_set _ =
  let result = delete_set "nonexistent_set" in
  assert_equal
    ~msg:
      ("Deleting nonexistent set should return false, but got: "
     ^ string_of_bool result)
    false result

(* Test is_valid_name function *)
let test_is_valid_name2 _ =
  (* Valid names *)
  assert_equal ~msg:"Simple name should be valid, but was reported invalid" true
    (is_valid_name "MySet");
  assert_equal ~msg:"Name with numbers should be valid, but was reported invalid"
    true (is_valid_name "Set123");

  (* Invalid names *)
  assert_equal ~msg:"Empty name should be invalid, but was reported valid" false
    (is_valid_name "");
  assert_equal ~msg:"Name with slash should be invalid, but was reported valid"
    false (is_valid_name "My/Set")

(* Comprehensive test suite *)
let path_suite =
  "FlashcardTests"
  >::: [
         "test_import_nonexistent_file" >:: test_import_nonexistent_file;
         "test_import_existing_file" >:: test_import_existing_file;
         "test_delete_nonexistent_set" >:: test_delete_nonexistent_set;
         "test_is_valid_name" >:: test_is_valid_name2;
       ]

(* Test for Game.get_motivational_message *)
let make_test_get_motivational_message flashcards rows cols fps =
  let game = initialize flashcards rows cols fps in
  let motivational_message = get_motivational_message game in
  assert_equal ~printer:string_of_string_option motivational_message None

let test_motivational_message =
  "Test motivational message"
  >::: [
         ( "Test for getting motivational message" >:: fun _ ->
           make_test_get_motivational_message [] 4 2 1. );
       ]

(* Test set asteroids_to_spawn*)

let make_test_set_asteroids_to_spawn flashcards rows cols fps n =
  let game = initialize flashcards rows cols fps in
  let new_game = set_asteroids_to_spawn game n in
  let expected = get_asteroids_to_spawn new_game in
  assert_equal ~printer:string_of_int expected n

let test_set_asteroids_to_spawn =
  "Test asteroids to spawn"
  >::: [
         ( "Test for asteroids to spawn" >:: fun _ ->
           make_test_set_asteroids_to_spawn [] 4 2 1. 5 );
       ]

(* Test set_last_spawn_time*)
let make_test_set_last_spawn_time flashcards rows cols fps fl =
  let game = initialize flashcards rows cols fps in
  let new_game = set_last_spawn_time game fl in
  let expected = get_last_spawn_time new_game in
  assert_equal ~printer:string_of_float expected fl

let test_set_last_spawn_time =
  "Test last spawn time"
  >::: [
         ( "Test for last spawn time" >:: fun _ ->
           make_test_set_last_spawn_time [] 4 2 1. 5. );
       ]
let string_of_flashcard_list lst =
  "[" ^ (String.concat "; " (List.map (Printf.sprintf "\"%s\"") lst)) ^ "]"

(* Test set_used_cards*)
let make_test_set_used_cards flashcards rows cols fps lst =
  let game = initialize flashcards rows cols fps in
  let new_game = set_used_cards game lst in
  let expected = get_used_cards new_game in
  assert_equal ~printer:string_of_flashcard_list expected lst

let test_set_used_cards =
  "Test set used cards"
  >::: [
         ( "Test for used cards" >:: fun _ ->
           make_test_set_used_cards [] 4 2 1. [ "hello"; "what" ] );
       ]

(* Test set_elapsed_time*)
let make_test_set_elapsed_time flashcards rows cols fps flt =
  let game = initialize flashcards rows cols fps in
  let new_game = set_elapsed_time game flt in
  let expected = get_elapsed_time new_game in
  assert_equal ~printer:string_of_float expected flt

let test_set_elapsed_time =
  "Test set elapsed time"
  >::: [
         ( "Test for elapsed time" >:: fun _ ->
           make_test_set_elapsed_time [] 4 2 1. 5. );
       ]

let () =
  List.iter run_test_tt_main
    [
      test_initialize;
      test_game_over;
      path_suite;
      test_motivational_message;
      test_set_motivational_message;
      test_set_score;
      test_set_lives;
      test_set_asteroids_to_spawn;
      test_set_last_spawn_time;
      test_set_used_cards;
      test_set_elapsed_time;
    ]
