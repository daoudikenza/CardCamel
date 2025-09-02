(** Flashcard module provides helper functions for managing flashcards *)

val is_valid_name : string -> bool
(** [is_valid_name name] checks whether a flashcard set name is valid. A valid
    name is non-empty and does not contain the '/' character. *)

val create_new : string -> string list
(** [create_new name] creates a new flashcard set with the given [name]. It
    creates an empty file for the flashcard set and returns an empty list of
    flashcards. *)

val load_flashcards : string -> string list
(** [load_flashcards file_path] loads a list of flashcards from a CSV file.
    Returns a list of strings read from the file. *)

val save_flashcards : string -> string list -> unit
(** [save_flashcards file_path flashcards] saves a list of flashcards to a CSV
    file. Each string in the [flashcards] list is written as a line in the file. *)

val string_of_flashcards : string list -> string
(** [string_of_flashcards flashcards] converts a list of flashcards to a
    formatted string representation. Each question and answer pair is displayed
    with numbering. *)

val normalize : string -> string
(** [normalize str] trims and converts the given string to lowercase for
    comparison. *)

val shuffle : 'a list -> 'a list
(** [shuffle lst] randomly shuffles the given list. Returns a new list with the
    elements in random order. *)

val percentage_of : int -> int -> int option
(** [percentage_of completed total] calculates progress as a percentage based on
    the number of completed flashcards and the total number of flashcards.
    Returns [None] if [total] is 0, or [Some p] where [p] is the progress rounded
    to the nearest milestone (25%, 50%, 75%). *)

(* val quiz : string list -> unit (** [quiz flashcards] starts a quiz session
   with the given flashcards. It will shuffle the flashcards and run the quiz
   logic. *)

   val run_quiz : (string * string * int) list -> int -> unit (** [run_quiz pool
   total] runs the quiz session with the given pool of flashcards and total
   number of flashcards. It handles the logic for progress updates and answer
   checking. *)

   val edit_flashcards : string list -> string -> unit (** [edit_flashcards
   flashcards file_path] provides an interactive menu for editing the flashcard
   set. It allows the user to add, edit, or delete flashcards. *)

   val add_flashcard : string list -> string -> unit (** [add_flashcard
   flashcards file_path] adds a new flashcard to the set by prompting the user to
   enter a question and answer, then saves the updated flashcards to the file. *)

   val edit_existing_flashcard : string list -> string -> unit (**
   [edit_existing_flashcard flashcards file_path] allows the user to edit an
   existing flashcard. It prompts the user to select a flashcard by its number,
   edit its question and answer, and then saves the changes. *)

   val delete_flashcard : string list -> string -> unit (** [delete_flashcard
   flashcards file_path] allows the user to delete a flashcard from the set. It
   prompts the user to select a flashcard by its number, removes it from the
   list, and saves the updated set. *)

   val match_game : string list -> unit (** [match_game flashcards] starts the
   Match game using the given [flashcards]. It ensures that a minimum of 6
   flashcards are used, or all available flashcards if fewer than 6 are present.
   The function shuffles the questions and answers independently and launches the
   gameplay loop. *)

   val group_into_pairs : string list -> (string * string) list (**
   [group_into_pairs flashcards] groups a flat list of [flashcards] into pairs of
   questions and answers. Each pair consists of a question and its corresponding
   answer, represented as a tuple. *)

   val display_grid : string list -> string list -> unit (** [display_grid
   questions answers] displays the current Match game grid, showing [questions]
   with numerical identifiers and [answers] with alphabetical identifiers. This
   grid is updated dynamically during the game as pairs are matched and removed.
   *)

   val match_loop : (string * string) list -> string list -> string list -> float
   -> unit (** [match_loop pairs questions answers start_time] runs the main
   Match game loop. It displays the grid, prompts the user for a match, validates
   the input, and checks the correctness of the selected pair. If the match is
   correct, the pair is removed from the grid, and the game progresses. If
   incorrect, the user is informed and allowed to try again. When all pairs are
   matched, the function calculates and displays the total time taken based on
   [start_time]. *) *)

val contains_substring : string -> string -> bool
(* val search_flashcards : string list -> unit *)

val import_set_from_path : string -> string option

(* Delete a flashcard set *)
val delete_set : string -> bool
val save_high_score : string -> int -> unit
val load_high_score : string -> int
val check_and_update_high_score : string -> int -> unit

(* type progress

   val update_progress : float -> unit val display_progress : unit -> unit *)
(* val create_progress : int -> int -> int -> int -> float -> progress *)
