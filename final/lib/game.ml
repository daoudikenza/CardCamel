type t = {
  board : Board.t;
  flashcards : (string * string) list; (* All flashcards *)
  used_cards : string list; (* Track used questions *)
  lives : int;
  score : int;
  fps : float;
  elapsed_time : float;
  start_time : float;
  last_spawn_time : float;
  asteroids_to_spawn : int;
  motivational_message : string option;
}
(** The [t] type is a record type that represents the state of the game. It
    includes the following fields:
    - [board]: The current state of the game board.
    - [flashcards]: A list of all flashcards, where each flashcard is a tuple
      containing a question and an answer.
    - [used_cards]: A list of questions that have already been used.
    - [lives]: The number of lives the player has remaining.
    - [score]: The player's current score.
    - [fps]: The frames per second rate of the game.
    - [elapsed_time]: The total time elapsed since the game started.
    - [start_time]: The time at which the game started.
    - [last_spawn_time]: The time at which the last asteroid was spawned.
    - [asteroids_to_spawn]: The number of asteroids that need to be spawned.
    - [motivational_message]: An optional motivational message to display to the
      player. *)

(** [pair_flashcards flashcards] pairs questions and answers with unique symbols *)
let pair_flashcards flashcards =
  let rec aux lst acc =
    match lst with
    | q :: a :: rest -> aux rest ((q, a) :: acc)
    | _ -> List.rev acc
  in
  aux flashcards []

(** [initialize flashcards rows cols fps] sets up the initial game state *)
let initialize flashcards rows cols fps =
  let board = Board.initialize rows cols [] in
  let paired_flashcards = pair_flashcards flashcards in
  {
    board;
    flashcards = paired_flashcards;
    used_cards = [];
    (* Initialize empty used cards list *)
    lives = 3;
    score = 0;
    fps;
    elapsed_time = 0.0;
    start_time = Unix.gettimeofday ();
    last_spawn_time = Unix.gettimeofday ();
    asteroids_to_spawn = 1;
    motivational_message = None;
  }

(** [game_over game] checks if the game has ended *)
let game_over game =
  let all_cards_used =
    List.length game.used_cards = List.length game.flashcards
    && List.length (Board.get_asteroids game.board) = 0
  in
  if game.lives <= 0 then (
    print_endline "\nGame Over! You ran out of lives.";
    Printf.printf "Your final score is: %d\n" game.score;
    true)
  else if all_cards_used then (
    print_endline "\nCongratulations! You completed all flashcards!";
    Printf.printf "Your final score is: %d\n" game.score;
    true)
  else false

let get_used_cards game = game.used_cards
let get_motivational_message game = game.motivational_message
let get_asteroids_to_spawn game = game.asteroids_to_spawn
let set_asteroids_to_spawn game n = { game with asteroids_to_spawn = n }
let get_last_spawn_time game = game.last_spawn_time
let set_last_spawn_time game fl = { game with last_spawn_time = fl }

(** [get_board] is the board in [game]*)
let get_board game = game.board

(** [get_lives] is the number of lives in [game*)
let get_lives game = game.lives

(** [get_score] is the score of [game]*)
let get_score game = game.score

(** [get_flashcards] is the flashcards in [game]*)
let get_flashcards game = game.flashcards

(** [get_fps] is the fps in [game]*)
let get_fps game = game.fps

let set_lives game n = { game with lives = n }
let set_score game n = { game with score = n }
let set_board game b = { game with board = b }
let set_motivational_message game msg = { game with motivational_message = msg }
let set_used_cards game lst = { game with used_cards = lst }
let get_start_time game = game.start_time
let get_elapsed_time game = game.elapsed_time
let set_elapsed_time game flt = { game with elapsed_time = flt }
