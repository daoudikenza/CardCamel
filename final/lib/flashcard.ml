open Printf

let create_new name =
  let empty = BatList.enum [] in
  let _ = BatFile.write_lines name empty in
  []

(* Helper function to validate a flashcard set name *)
let is_valid_name name =
  (* Ensure the name is not empty and doesn't contain forbidden characters *)
  name <> "" && not (String.contains name '/')

(* Helper function to load a flashcard set from a CSV file *)
let load_flashcards file_path =
  try BatFile.lines_of file_path |> BatList.of_enum with _ -> []

(* Helper function to save a flashcard set to a CSV file *)
let save_flashcards file_path flashcards =
  let lines = BatList.enum flashcards in
  BatFile.write_lines file_path lines

let blue = "\027[34m"
let green = "\027[32m"
let yellow = "\027[33m"
let reset = "\027[0m"
(* let bold = "\027[1m" *)

let string_of_flashcards flashcards =
  if List.length flashcards = 0 then
    "\nNo flashcards available. Use 'add' to create new ones."
  else
    let buffer = Buffer.create 128 in

    (* Decorative header *)
    Buffer.add_string buffer
      (blue ^ "╭───────────── FLASHCARD PREVIEW ─────────────╮" ^ reset ^ "\n");

    (* Stats *)
    let total = List.length flashcards / 2 in
    Buffer.add_string buffer (yellow ^ sprintf "Total Cards: %d\n" total ^ reset);
    Buffer.add_string buffer
      (blue ^ "├───────────────────────────────────────────────┤" ^ reset ^ "\n");

    (* Cards with formatting *)
    let rec add_cards idx = function
      | q :: a :: rest ->
          (* Card number and question *)
          Buffer.add_string buffer
            (green ^ sprintf "\n  Card #%d\n" ((idx / 2) + 1) ^ reset);
          Buffer.add_string buffer
            (blue ^ "  ┌──────────────────────────────────┐" ^ reset ^ "\n");
          Buffer.add_string buffer
            (blue ^ "  │ Q: " ^ reset ^ String.trim q ^ "\n");
          Buffer.add_string buffer
            (blue ^ "  │ A: " ^ reset ^ String.trim a ^ "\n");
          Buffer.add_string buffer
            (blue ^ "  └──────────────────────────────────┘" ^ reset ^ "\n");
          add_cards (idx + 2) rest
      | _ -> ()
    in
    add_cards 0 flashcards;

    (* Footer *)
    Buffer.add_string buffer
      (blue ^ "\n╰───────────────────────────────────────────────╯" ^ reset
     ^ "\n");
    Buffer.add_string buffer "Press Enter to continue...";

    (* Return the accumulated string *)
    Buffer.contents buffer

(* Helper function to normalize a string for comparison *)
let normalize str = String.trim (String.lowercase_ascii str)

(* Shuffle a list *)
let shuffle lst =
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

(* Helper function to group a flat list into pairs *)

(* Helper function to calculate progress percentage *)
let percentage_of completed total =
  if total = 0 then None
  else
    let progress = completed * 100 / total in
    if progress >= 75 && progress < 100 then Some 75
    else if progress >= 50 && progress < 75 then Some 50
    else if progress >= 25 && progress < 50 then Some 25
    else None

let contains_substring s substr =
  let s_len = String.length s in
  let substr_len = String.length substr in
  if substr_len > s_len then false
  else
    let rec loop i =
      if i + substr_len > s_len then false
      else if String.sub s i substr_len = substr then true
      else loop (i + 1)
    in
    loop 0

let import_set_from_path path =
  if Sys.file_exists path then
    try
      let file_name = Filename.basename path in
      let new_path = "data/" ^ file_name in
      if Sys.file_exists new_path then None (* File already exists *)
      else
        let lines = BatFile.lines_of path in
        let _ = BatFile.write_lines new_path lines in
        Some (Filename.chop_extension file_name)
    with _ -> None
  else None

(* Delete a flashcard set by name *)
let delete_set name =
  let file_path = "data/" ^ name ^ ".csv" in
  if Sys.file_exists file_path then
    try
      Sys.remove file_path;
      true
    with _ -> false
  else false

let save_high_score set_name score =
  let filename = "high_score_" ^ set_name ^ ".txt" in
  let oc = open_out filename in
  output_string oc (string_of_int score);
  close_out oc;
  print_endline "High score saved."

let load_high_score set_name =
  let filename = "high_score_" ^ set_name ^ ".txt" in
  try
    let ic = open_in filename in
    let score = input_line ic in
    close_in ic;
    int_of_string score
  with _ -> 0 (* No saved high score exists yet *)

let check_and_update_high_score set_name current_score =
  let current_high_score = load_high_score set_name in
  if current_score > current_high_score then (
    save_high_score set_name current_score;
    print_endline "🎉 New High Score! 🎉")
  else print_endline "Keep trying to beat your best score!"
