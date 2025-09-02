type t
(** The type representing the game state. *)

val initialize : string list -> int -> int -> float -> t
(** [initialize flashcards rows cols fps] sets up the game with the given
    [flashcards], board dimensions ([rows] x [cols]), and frame rate [fps]. *)

val get_lives : t -> int
(** [get_lives] is the number of lives *)

val get_used_cards : t -> string list
(** [get_used_cards] is the list of used cards in [game]*)

val game_over : t -> bool
(** [game_over game] returns [true] if the game has ended (no lives left), else
    [false]. *)

val get_flashcards : t -> (string * string) list
(** [get_flashcards] is the flashcards in [game]*)

val get_fps : t -> float
(** [get_fps] is the fps in [game]*)

val get_score : t -> int
(** [get_score] is the score in [game]*)

val get_motivational_message : t -> string option
(** [get_motivational_message] is the motivational message in [game]*)

val get_board : t -> Board.t
(** [get_board] is the board in [game]*)

(** [set_motivational_message t msg] sets the motivational message of the game state [t] to [msg]. *)
val set_motivational_message : t -> string option -> t

(** [set_score t score] sets the score of the game state [t] to [score]. *)
val set_score : t -> int -> t

(** [set_board t board] sets the board of the game state [t] to [board]. *)
val set_board : t -> Board.t -> t

(** [set_lives t lives] sets the number of lives in the game state [t] to [lives]. *)
val set_lives : t -> int -> t

(** [set_used_cards t cards] sets the list of used cards in the game state [t] to [cards]. *)
val set_used_cards : t -> string list -> t

(** [set_elapsed_time t time] sets the elapsed time of the game state [t] to [time]. *)
val set_elapsed_time : t -> float -> t

(** [get_start_time t] returns the start time of the game state [t]. *)
val get_start_time : t -> float

(** [get_elapsed_time t] returns the elapsed time of the game state [t]. *)
val get_elapsed_time : t -> float

(** [get_asteroids_to_spawn t] returns the number of asteroids to spawn in the game state [t]. *)
val get_asteroids_to_spawn : t -> int

(** [set_asteroids_to_spawn t num] sets the number of asteroids to spawn in the game state [t] to [num]. *)
val set_asteroids_to_spawn : t -> int -> t

(** [get_last_spawn_time t] returns the last spawn time of the game state [t]. *)
val get_last_spawn_time : t -> float

(** [set_last_spawn_time t time] sets the last spawn time of the game state [t] to [time]. *)
val set_last_spawn_time : t -> float -> t
