open Printf
open Final.Flashcard
open Final.Game
open Final.Board
open Final.Asteroid

let welcome_banner =
  {|

 ██████╗ █████╗ ██████╗ ██████╗  ██████╗ █████╗ ███╗   ███╗███████╗██╗     
██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗████╗ ████║██╔════╝██║     
██║     ███████║██████╔╝██║  ██║██║     ███████║██╔████╔██║█████╗  ██║     
██║     ██╔══██║██╔══██╗██║  ██║██║     ██╔══██║██║╚██╔╝██║██╔══╝  ██║     
╚██████╗██║  ██║██║  ██║██████╔╝╚██████╗██║  ██║██║ ╚═╝ ██║███████╗███████╗
 ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝  ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝
                                                                           

|}

let menu_separator = String.make 40 '-'
let success msg = Printf.printf "\027[32m%s\027[0m\n" msg (* Green text *)
let error msg = Printf.printf "\027[31m%s\027[0m\n" msg (* Red text *)
let info msg = Printf.printf "\027[36m%s\027[0m\n" msg (* Cyan text *)

let search_flashcards flashcards =
  print_string "\nEnter a keyword to search: ";
  let keyword = read_line () |> String.lowercase_ascii in
  let matches =
    (* Iterate over the string list in pairs (index manipulation) *)
    let rec find_matches i acc =
      if i >= List.length flashcards then acc
      else
        let question = String.lowercase_ascii (List.nth flashcards i) in
        let answer = String.lowercase_ascii (List.nth flashcards (i + 1)) in
        if
          contains_substring question keyword
          || contains_substring answer keyword
        then
          find_matches (i + 2)
            ((List.nth flashcards i, List.nth flashcards (i + 1)) :: acc)
        else find_matches (i + 2) acc
    in
    find_matches 0 []
  in
  if matches = [] then print_endline "No matching flashcards found."
  else (
    print_endline "\nSearch results:";
    List.iter (fun (q, a) -> Printf.printf "Q: %s\nA: %s\n" q a) matches)

let group_into_pairs lst =
  let rec aux acc = function
    | q :: a :: rest -> aux ((q, a, 0) :: acc) rest
    | _ -> List.rev acc
  in
  aux [] lst

let green s = Printf.sprintf "\027[32m%s\027[0m" s
let red s = Printf.sprintf "\027[31m%s\027[0m" s
let blue s = Printf.sprintf "\027[34m%s\027[0m" s
let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let print_separator () = print_endline (String.make 50 '-')

let countdown () =
  List.iter
    (fun n ->
      Printf.printf "\rNext question in %d..." n;
      flush stdout;
      Unix.sleep 1)
    [ 3; 2; 1 ];
  print_endline "\r                      "

type progress = {
  quizzes_taken : int;
  total_score : float;
  best_score : float;
  last_score : float;
  average_score : float;
}

let load_progress () =
  if Sys.file_exists "progress.txt" then
    let ic = open_in "progress.txt" in
    let rec parse_lines acc =
      try
        let line = input_line ic in
        let parts = String.split_on_char '=' line in
        match parts with
        | [ key; value ] -> parse_lines ((key, value) :: acc)
        | _ -> parse_lines acc
      with End_of_file ->
        close_in ic;
        acc
    in
    let progress = parse_lines [] in
    {
      quizzes_taken = int_of_string (List.assoc "quizzes_taken" progress);
      total_score = float_of_string (List.assoc "total_score" progress);
      best_score = float_of_string (List.assoc "best_score" progress);
      last_score = float_of_string (List.assoc "last_score" progress);
      average_score = float_of_string (List.assoc "average_score" progress);
    }
  else
    (* Initialize progress if no file exists *)
    {
      quizzes_taken = 0;
      total_score = 0.0;
      best_score = 0.0;
      last_score = 0.0;
      average_score = 0.0;
    }

let save_progress progress =
  let oc = open_out "progress.txt" in
  Printf.fprintf oc "quizzes_taken=%d\n" progress.quizzes_taken;
  Printf.fprintf oc "total_score=%.2f\n" progress.total_score;
  Printf.fprintf oc "best_score=%.2f\n" progress.best_score;
  Printf.fprintf oc "last_score=%.2f\n" progress.last_score;
  Printf.fprintf oc "average_score=%.2f\n" progress.average_score;
  close_out oc

let update_progress current_score =
  let progress = load_progress () in
  let quizzes_taken = progress.quizzes_taken + 1 in
  let total_score = progress.total_score +. current_score in
  let best_score = max progress.best_score current_score in
  let last_score = current_score in
  let average_score = total_score /. float_of_int quizzes_taken in
  let updated_progress =
    { quizzes_taken; total_score; best_score; last_score; average_score }
  in
  save_progress updated_progress

let display_progress () =
  let progress = load_progress () in
  Printf.printf "\nProgress Report:\n";
  Printf.printf "Quizzes Taken: %d\n" progress.quizzes_taken;
  Printf.printf "Total Score: %.2f\n" progress.total_score;
  Printf.printf "Best Score: %.2f\n" progress.best_score;
  Printf.printf "Last Score: %.2f\n" progress.last_score;
  Printf.printf "Average Score: %.2f\n" progress.average_score

let rec quiz flashcards file_path =
  if BatList.is_empty flashcards then
    print_endline (red "\nNo flashcards to quiz on.")
  else begin
    print_separator ();
    print_endline (bold "🎓 Flash Card Quiz");
    print_separator ();
    let set_name = Filename.chop_extension (Filename.basename file_path) in
    print_endline
      ("Previous High Score: " ^ string_of_int (load_high_score set_name));
    let quiz_pool = group_into_pairs flashcards in
    run_quiz (shuffle quiz_pool) (List.length quiz_pool) 0 set_name
  end

and run_quiz pool total current_score set_name =
  let remaining = List.length pool in
  if remaining = 0 then begin
    print_separator ();
    print_endline (green "\n🎉 Congratulations! You've completed the quiz!");
    Printf.printf "Final Score: %s\n" (bold (string_of_int current_score));
    check_and_update_high_score set_name current_score;

    update_progress (float_of_int current_score /. float_of_int total *. 100.0);
    print_separator ()
  end
  else
    let completed = total - remaining in
    begin
      match percentage_of completed total with
      | None ->
          Printf.printf "%s\n"
            (blue (Printf.sprintf "Remaining: %d cards" remaining))
      | Some p ->
          Printf.printf "\n%s | %s | %s\n"
            (blue (Printf.sprintf "Progress: %d%%" p))
            (green (Printf.sprintf "Score: %d" current_score))
            (Printf.sprintf "Completed: %d/%d" completed total);
          countdown ()
    end;
    let question, answer, streak = List.hd pool in
    print_endline "\n----------------------------";
    printf "Question: %s\n" question;
    print_string "Your Answer (#back to exit): ";
    let user_input = read_line () in
    if normalize user_input = "#back" then print_endline "Exiting quiz mode."
    else if normalize user_input = normalize answer then
      let new_streak = streak + 1 in
      let new_score = current_score + 1 in
      if new_streak >= 2 then (
        print_endline "Correct! You've mastered this flashcard!";
        run_quiz (List.tl pool) total new_score set_name)
      else (
        print_endline "Correct! Keep it up!";
        run_quiz (List.tl pool) total new_score set_name)
    else (
      printf "Incorrect. The correct answer is: %s\n" answer;
      let new_pool = List.tl pool in
      run_quiz new_pool total current_score set_name)

(* Main function to print the flashcards preview *)
let preview_flashcards flashcards =
  let output = string_of_flashcards flashcards in
  print_endline output

(* Function to edit the flashcard set *)
let rec edit_flashcards flashcards file_path =
  preview_flashcards flashcards;
  print_endline "\nEdit Options:";
  print_endline "1. Add a New Flashcard";
  print_endline "2. Edit an Existing Flashcard";
  print_endline "3. Delete an Existing Flashcard";
  print_endline "4. Return to Main Menu";
  print_string "Enter your choice: ";
  match read_line () with
  | "1" ->
      print_endline "\n";
      add_flashcard flashcards file_path
  | "2" ->
      print_endline "\n";
      edit_existing_flashcard flashcards file_path
  | "3" ->
      print_endline "\n";
      delete_flashcard flashcards file_path
  | "4" -> ()
  | _ ->
      print_endline "\nInvalid choice. Please try again.";
      edit_flashcards flashcards file_path

(* Add a new flashcard *)
and add_flashcard flashcards file_path =
  print_endline "\nAdd a New Flashcard:";
  print_string "Enter the question (or press Enter to cancel): ";
  let question = read_line () in
  if question = "" then print_endline "Cancelled adding a new flashcard."
  else
    let rec get_answer () =
      print_string "Enter the answer: ";
      let answer = read_line () in
      if answer = "" then (
        print_endline "Answer cannot be empty. Please try again.";
        get_answer ())
      else answer
    in
    let answer = get_answer () in
    let updated_flashcards = flashcards @ [ question; answer ] in
    save_flashcards file_path updated_flashcards;
    print_endline "Flashcard added successfully.";
    edit_flashcards updated_flashcards file_path

(* Edit an existing flashcard *)
and edit_existing_flashcard flashcards file_path =
  if BatList.is_empty flashcards then
    print_endline "The flashcard set is empty. Nothing to edit."
  else (
    print_string "Enter the number of the flashcard to edit: ";
    match read_int_opt () with
    | Some n when n > 0 && n <= BatList.length flashcards / 2 ->
        let idx = (n - 1) * 2 in
        let question = List.nth flashcards idx in
        let answer = List.nth flashcards (idx + 1) in
        printf "Current Question: %s\n" question;
        print_string "Enter new question (or press Enter to keep the same): ";
        let new_question = read_line () in
        printf "Current Answer: %s\n" answer;
        print_string "Enter new answer (or press Enter to keep the same): ";
        let new_answer = read_line () in
        let updated_flashcards =
          BatList.modify_at idx
            (fun _ -> if new_question = "" then question else new_question)
            flashcards
          |> BatList.modify_at (idx + 1) (fun _ ->
                 if new_answer = "" then answer else new_answer)
        in
        save_flashcards file_path updated_flashcards;
        print_endline "Flashcard updated successfully.";
        edit_flashcards updated_flashcards file_path
    | _ ->
        print_endline "Invalid number. Please try again.";
        edit_flashcards flashcards file_path)

(* Delete an existing flashcard *)
and delete_flashcard flashcards file_path =
  if BatList.is_empty flashcards then
    print_endline "The flashcard set is empty. Nothing to delete."
  else (
    print_string "Enter the number of the flashcard to delete: ";
    match read_int_opt () with
    | Some n when n > 0 && n <= BatList.length flashcards / 2 ->
        let idx = (n - 1) * 2 in
        let updated_flashcards =
          BatList.remove_at idx flashcards |> BatList.remove_at idx
        in
        save_flashcards file_path updated_flashcards;
        print_endline "Flashcard deleted successfully.";
        edit_flashcards updated_flashcards file_path
    | _ ->
        print_endline "Invalid number. Please try again.";
        edit_flashcards flashcards file_path)

(* Start the Match game *)
(* let rec match_game flashcards = let total_flashcards = min 6 (List.length
   flashcards / 2) in if total_flashcards = 0 then print_endline "\nNot enough
   flashcards to play Match." else ( printf "\nStarting a Match game with %d
   flashcards.\n" total_flashcards;

   (* Select the flashcards and shuffle questions/answers *) let pairs =
   List.fold_left (fun acc (q, a) -> (q, a) :: acc) [] (group_into_pairs
   flashcards) in let selected_pairs = shuffle pairs |> BatList.take
   total_flashcards in let questions, answers = List.split selected_pairs in let
   shuffled_questions = shuffle questions in let shuffled_answers = shuffle
   answers in

   (* Start the game timer *) let start_time = Unix.gettimeofday () in

   (* Run the Match game loop *) match_loop selected_pairs shuffled_questions
   shuffled_answers start_time) *)

(* Helper function to group flashcards into question-answer pairs *)
and group_into_pairs lst =
  let rec aux acc = function
    | q :: a :: rest -> aux ((q, a) :: acc) rest
    | _ -> List.rev acc
  in
  aux [] lst

(* Print game status footer *)

(** [display game] renders the game state *)
let display game =
  (* Clear the screen is handled in Board.render *)
  render (get_board game);
  (* Display score and lives *)
  Printf.printf "Score: %d | Lives: %d\n" (get_score game) (get_lives game);
  (* Display asteroid map *)
  print_endline "\nMap:";
  List.iter
    (fun asteroid ->
      Printf.printf "%s - %s\n" (to_string asteroid) (get_content asteroid))
    (get_asteroids (get_board game));
  (* Display motivational message *)
  (match get_motivational_message game with
  | Some msg -> Printf.printf "\n-- %s --\n" msg
  | None -> ());
  (* Display user input prompt *)
  print_string "Destroy: ";
  flush stdout

(** [handle_input game input] processes user input *)
let handle_input game input =
  let normalized_input = String.trim (String.lowercase_ascii input) in
  if normalized_input = "#help" then (
    (* Display help and pause the game *)
    print_endline "\n--- Help Menu ---";
    print_endline "Instructions:";
    print_endline "- Type the correct response to destroy asteroids.";
    print_endline "- Gain points based on asteroid speed and response time.";
    print_endline "- Lose a life if an asteroid reaches the bottom of the board.";
    print_endline "- Type #back to return to the main menu.";
    print_endline "\nPress Enter to resume the game.";
    ignore (read_line ());
    game)
  else if normalized_input = "#back" then
    (* Handle returning to main menu *)
    set_lives game 0 (* Trigger game over *)
  else
    let current_time = Unix.gettimeofday () in
    let asteroids = get_asteroids (get_board game) in
    let matched, unmatched =
      List.partition
        (fun asteroid ->
          let question = get_content asteroid in
          let answer = List.assoc question (get_flashcards game) in
          String.lowercase_ascii answer = normalized_input)
        asteroids
    in
    if matched = [] then
      (* No asteroid destroyed *)
      set_motivational_message game None
    else
      let board = update_asteroids (get_board game) unmatched in
      let total_score =
        List.fold_left
          (fun acc asteroid ->
            let time_taken = current_time -. get_creation_time asteroid in
            acc + destroyed asteroid time_taken)
          0 matched
      in
      let game = set_board game board in
      let game = set_score game (get_score game + total_score) in
      set_motivational_message game
        (Some (Printf.sprintf "Great job! +%d points!" total_score))

(** [spawn_asteroids game] adds new asteroids to the board *)
let spawn_asteroids game =
  let available_flashcards =
    List.filter
      (fun (_, a) -> not (List.mem a (get_used_cards game)))
      (get_flashcards game)
  in
  if available_flashcards = [] then game
  else
    let idx = Random.int (List.length available_flashcards) in
    let q, a = List.nth available_flashcards idx in
    let col = Random.int (snd (get_dimensions (get_board game))) in
    let new_asteroid = create q col in
    (* Store question as content *)
    let updated_board = add_asteroid (get_board game) new_asteroid in
    let new_game = set_board game updated_board in
    set_used_cards new_game (a :: get_used_cards game)

(** [should_increase_difficulty game] checks if it's time to increase difficulty *)
let should_increase_difficulty game =
  let time_since_start = Unix.gettimeofday () -. get_start_time game in
  int_of_float time_since_start / 5 > int_of_float (get_elapsed_time game /. 5.0)

(** [update game] advances the game state by one frame *)
let update game =
  let current_time = Unix.gettimeofday () in
  let elapsed = current_time -. get_start_time game in
  let frame_number = int_of_float (elapsed *. get_fps game) in
  let board = move_asteroids (get_board game) frame_number in
  let collided_asteroids, board = check_collision board in
  let lives = get_lives game - List.length collided_asteroids in
  let game = set_board game board in
  let game = set_lives game lives in
  let game =
    if should_increase_difficulty game then
      let new_game =
        set_asteroids_to_spawn game (get_asteroids_to_spawn game + 3)
      in
      set_elapsed_time new_game elapsed
    else game
  in
  let time_since_last_spawn = current_time -. get_last_spawn_time game in
  let game =
    if time_since_last_spawn >= 1.0 then
      let game = spawn_asteroids game in
      set_last_spawn_time game current_time
    else game
  in
  game

(** [run game] starts the main game loop *)
let rec run game =
  if game_over game then (
    print_endline "\nGame over!";
    Printf.printf "Your final score is: %d\n" (get_score game);
    ())
  else
    let start_time = Unix.gettimeofday () in
    display game;

    (* Wait for input with timeout *)
    let input_ready, _, _ =
      Unix.select [ Unix.stdin ] [] [] (1. /. get_fps game)
    in

    let game =
      if input_ready <> [] then
        (* Process user input *)
        try
          let input = read_line () in
          handle_input game input
        with End_of_file -> game
      else game
    in

    (* Calculate how much time passed during input *)
    let elapsed = Unix.gettimeofday () -. start_time in

    (* Update game state based on elapsed time *)
    let rec update_loop g remaining =
      if remaining <= 0. then g
      else update_loop (update g) (remaining -. (1. /. get_fps game))
    in

    let updated_game = update_loop game elapsed in
    run updated_game

let is_set_empty file_path =
  let flashcards = load_flashcards file_path in
  BatList.is_empty flashcards

(* List all flashcard sets with empty status *)
let list_flashcard_sets_with_empty_status () =
  let data_dir = "data" in
  try
    Sys.readdir data_dir |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file ".csv")
    |> List.map (fun file ->
           let name = Filename.chop_extension file in
           let file_path = Filename.concat data_dir file in
           (name, is_set_empty file_path))
  with Sys_error _ ->
    print_endline "Error: Could not access the data directory.";
    []

(* Display flashcard sets and mark empties *)
let display_flashcard_sets_with_empty_status sets =
  let empty_sets =
    List.filter (fun (_, is_empty) -> is_empty) sets |> List.map fst
  in
  if empty_sets <> [] then (
    print_endline
      "Notice - The following flashcard sets are empty. This will lead to \
       limited possibilities:";
    empty_sets
    |> List.iteri (fun i name -> Printf.printf "%s (%d), " name (i + 1));
    print_endline "");
  print_endline "\nAvailable flashcard sets:";
  List.iteri (fun i (name, _) -> Printf.printf "%d. %s\n" (i + 1) name) sets

(* Main menu logic *)
let rec main_menu flashcards file_path =
  print_endline "\n";
  print_endline menu_separator;
  info "Main Menu";
  print_endline menu_separator;

  if BatList.is_empty flashcards then
    error "⚠️  Note: The flashcard set is empty; some options are unavailable."
  else
    info
      (Printf.sprintf "📚 Current set: %s (%d cards)"
         (Filename.chop_extension (Filename.basename file_path))
         (List.length flashcards / 2));

  print_endline "\nAvailable Actions:";
  let menu_items =
    [
      (1, "📝 Quiz", not (BatList.is_empty flashcards));
      (2, "👀 Preview Flashcards", true);
      (3, "✏️  Edit Set", true);
      (4, "🎮 Match Game", not (BatList.is_empty flashcards));
      (5, "🌠 Gravity Game", not (BatList.is_empty flashcards));
      (6, "📂 Use a Different Set", true);
      (7, "🔍 Search Flashcards", not (BatList.is_empty flashcards));
      (8, "🔄 Return to Main Menu", true);
      (9, "🚪 Exit", true);
    ]
  in

  List.iter
    (fun (num, text, available) ->
      if available then Printf.printf "%d. %s\n" num text
      else Printf.printf "%d. %s (unavailable)\n" num text)
    menu_items;

  print_string "Enter your choice: ";
  match read_line () with
  | "1" when not (BatList.is_empty flashcards) ->
      quiz flashcards file_path;
      main_menu flashcards file_path
  | "1" ->
      print_endline "\nQuiz is unavailable because the flashcard set is empty.";
      main_menu flashcards file_path
  | "2" ->
      preview_flashcards flashcards;
      main_menu flashcards file_path
  | "3" ->
      edit_flashcards flashcards file_path;
      main_menu flashcards file_path
  | "4" when not (BatList.is_empty flashcards) ->
      match_game flashcards;
      main_menu flashcards file_path
  | "4" ->
      print_endline
        "\n\
         This set has no flashcards to play Match. Please add flashcards or \
         choose a different set.";
      main_menu flashcards file_path
  | "5" when not (BatList.is_empty flashcards) ->
      gravity_game flashcards file_path
  | "5" ->
      print_endline
        "\n\
         This set has no flashcards to play Gravity Game. Please add flashcards \
         or choose a different set.";
      main_menu flashcards file_path
  | "6" -> start ()
  | "7" ->
      search_flashcards flashcards;
      main_menu flashcards file_path
  | "8" ->
      print_endline "\nReturning to the main menu...";
      start ()
  | "9" -> print_endline "\nGoodbye!"
  | _ ->
      print_endline "\nInvalid choice. Please try again.";
      main_menu flashcards file_path

(* Prompt for a valid flashcard set name *)
and get_valid_name () =
  print_string "\nEnter a name for the flashcard set: ";
  let name = read_line () in
  if is_valid_name name then name
  else (
    print_endline "Invalid name. Please try again.";
    get_valid_name ())

(* Main entry point for the program *)
and start () =
  Random.self_init ();
  (* print_endline welcome_banner; print_endline menu_separator; info "Welcome to
     Flashcard Manager!"; print_endline menu_separator; *)
  print_endline "\nPlease select an option:";
  print_endline "1. 📚 Import an existing flashcard set";
  print_endline "2. ✨ Create a new flashcard set";
  print_endline "3. 📤 Import set from computer";
  print_endline "4. 🗑️  Delete a flashcard set";
  print_endline "5. 📊 View Progress";
  print_endline "6. 🚪 Exit";
  print_string "\nEnter your choice: ";
  match read_line () with
  | "1" ->
      let flashcard_sets = list_flashcard_sets_with_empty_status () in
      if flashcard_sets = [] then (
        error "\n⚠️  No flashcard sets found in the data directory.";
        start ())
      else (
        display_flashcard_sets_with_empty_status flashcard_sets;
        print_string "\nEnter the number of the flashcard set to load: ";
        match read_line () |> int_of_string_opt with
        | Some n when n > 0 && n <= List.length flashcard_sets ->
            let selected_set, _ = List.nth flashcard_sets (n - 1) in
            let file_path = "data/" ^ selected_set ^ ".csv" in
            let flashcards = load_flashcards file_path in
            success "\n✅ Flashcard set loaded successfully!";
            main_menu flashcards file_path
        | _ ->
            error "\n❌ Invalid choice. Please try again.";
            start ())
  | "2" ->
      let name = get_valid_name () in
      let file_path = "data/" ^ name ^ ".csv" in
      if Sys.file_exists file_path then (
        error "❌ Flashcard set already exists.";
        start ())
      else (
        BatFile.write_lines file_path (BatList.enum []);
        success "✨ New flashcard set created successfully!";
        main_menu [] file_path)
  | "3" -> (
      print_string "Enter the full path to the CSV file: ";
      let path = read_line () in
      match import_set_from_path path with
      | Some name ->
          success (Printf.sprintf "✅ Imported flashcard set: %s" name);
          start ()
      | None ->
          error
            "❌ Failed to import the flashcard set. Please check the file path.";
          start ())
  | "4" ->
      let flashcard_sets = list_flashcard_sets_with_empty_status () in
      if flashcard_sets = [] then (
        error "\n⚠️  No flashcard sets found in the data directory.";
        start ())
      else (
        display_flashcard_sets_with_empty_status flashcard_sets;
        print_string "\nEnter the number of the flashcard set to delete: ";
        match read_line () |> int_of_string_opt with
        | Some n when n > 0 && n <= List.length flashcard_sets -> (
            let selected_set, _ = List.nth flashcard_sets (n - 1) in
            match delete_set selected_set with
            | true ->
                success
                  (Printf.sprintf "🗑️  Deleted flashcard set: %s" selected_set);
                start ()
            | false ->
                error "❌ Failed to delete the flashcard set.";
                start ())
        | _ ->
            error "\n❌ Invalid choice. Please try again.";
            start ())
  | "5" ->
      display_progress ();
      start ()
  | "6" ->
      info "\n👋 Thank you for using Flashcard Manager. Goodbye!";
      exit 0
  | _ ->
      error "\n❌ Invalid choice. Please try again.";
      start ()

(* Start the program *)

(* Start the Match game *)
and match_game flashcards =
  let total_flashcards = min 6 (List.length flashcards / 2) in
  if total_flashcards = 0 then
    print_endline "\nNot enough flashcards to play Match."
  else (
    Printf.printf "\nStarting a Match game with %d flashcards.\n"
      total_flashcards;

    (* Select the flashcards and shuffle questions/answers *)
    let pairs =
      List.fold_left
        (fun acc (q, a) -> (q, a) :: acc)
        []
        (group_into_pairs flashcards)
    in
    let selected_pairs = shuffle pairs |> BatList.take total_flashcards in
    let questions, answers = List.split selected_pairs in
    let shuffled_questions = shuffle questions in
    let shuffled_answers = shuffle answers in

    (* Start the game timer *)
    let start_time = Unix.gettimeofday () in

    (* Run the Match game loop *)
    match_loop selected_pairs shuffled_questions shuffled_answers start_time)

(* Display the current Match grid *)
and display_grid questions answers =
  print_endline "\nQuestions:";
  List.iteri (fun i q -> Printf.printf "%d. %s\n" (i + 1) q) questions;
  print_endline "\nAnswers:";
  List.iteri (fun i a -> Printf.printf "%c. %s\n" (Char.chr (i + 65)) a) answers

(* Main Match game loop *)
and match_loop pairs questions answers start_time =
  display_grid questions answers;

  (* Prompt the user for a match *)
  print_string "\nEnter your match (e.g., 1B or type #back to exit): ";
  let input = read_line () in
  if normalize input = "#back" then (
    print_endline "Exiting Match game.";
    ())
  else if String.length input >= 2 then (
    let question_idx = int_of_string_opt (String.sub input 0 1) in
    let answer_idx =
      if String.length input = 2 then Some (Char.code (String.get input 1) - 65)
      else None
    in

    (* Validate and process the input *)
    match (question_idx, answer_idx) with
    | Some q_idx, Some a_idx
      when q_idx > 0
           && q_idx <= List.length questions
           && a_idx >= 0
           && a_idx < List.length answers ->
        let question = List.nth questions (q_idx - 1) in
        let answer = List.nth answers a_idx in

        (* Check if the pair is correct *)
        if List.exists (fun (q, a) -> q = question && a = answer) pairs then (
          Printf.printf "Correct! \"%s\" matches with \"%s.\"\n" question answer;

          (* Remove the matched pair *)
          let updated_pairs =
            List.filter (fun (q, a) -> q <> question || a <> answer) pairs
          in
          let updated_questions =
            List.filter (fun q -> q <> question) questions
          in
          let updated_answers = List.filter (fun a -> a <> answer) answers in

          (* Check if all pairs are matched *)
          if updated_pairs = [] then
            let end_time = Unix.gettimeofday () in
            let elapsed = end_time -. start_time in
            let minutes = int_of_float (elapsed /. 60.) in
            let seconds = int_of_float (mod_float elapsed 60.) in
            Printf.printf
              "Congratulations! You matched all pairs in %d minutes and %d \
               seconds.\n"
              minutes seconds
          else
            match_loop updated_pairs updated_questions updated_answers start_time)
        else (
          Printf.printf "Incorrect. \"%s\" does not match \"%s.\"\n" question
            answer;
          match_loop pairs questions answers start_time)
    | _ ->
        print_endline "Invalid input. Please try again.";
        match_loop pairs questions answers start_time)
  else (
    print_endline "Invalid input format. Please try again.";
    match_loop pairs questions answers start_time)

(* Start the Gravity game *)
and gravity_game flashcards file_path =
  let default_rows = 15 in
  let default_cols = 40 in
  let default_fps = 0.5 in

  (* Prepare the flashcards *)
  let flashcards_list = flashcards in

  (* Initialize the game *)
  let game =
    Final.Game.initialize flashcards_list default_rows default_cols default_fps
  in

  (* Run the Gravity game *)
  run game;
  main_menu flashcards file_path

let () =
  print_endline welcome_banner;
  print_endline menu_separator;
  info "Welcome to Flashcard Manager!";
  print_endline menu_separator;
  start ()
