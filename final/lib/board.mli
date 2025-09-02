type t
(** The type representing the game board. *)

val initialize : int -> int -> Asteroid.t list -> t
(** [initialize rows cols asteroids] creates a new board with dimensions [rows] x
    [cols] and places the initial [asteroids] on it. *)

val add_asteroid : t -> Asteroid.t -> t
(** [add_asteroid board asteroid] adds a new [asteroid] to the [board]. *)

val get_rows : t -> int
(** [get_rows] is the number of columns in [board] *)

val get_cols : t -> int
(** [get_cols] is the number of columns in [board]*)

val get_grid : t -> string array array
(** [get_grid] is the grid in [board]*)

val move_asteroids : t -> int -> t
(** [move_asteroids board frame_number] updates the positions of all asteroids on
    the [board] based on their speed and the current [frame_number]. *)

val check_collision : t -> Asteroid.t list * t
(** [check_collision board] checks if any asteroid has reached the bottom row.
    Returns a tuple of (asteroids_at_bottom, updated_board). *)

val resize : t -> int -> int -> t
(** [resize board new_rows new_cols] changes the dimensions of the [board] to
    [new_rows] x [new_cols]. *)

val render : t -> unit
(** [render board] displays the current state of the [board] on the terminal. *)

val get_dimensions : t -> int * int
(** [get_dimensions board] returns the (rows, columns) of the [board]. *)

val get_asteroids : t -> Asteroid.t list
(** [get_asteroids board] returns the list of active asteroids on the [board]. *)

val update_asteroids : t -> Asteroid.t list -> t
(** [update_asteroids board asteroids] updates the board's asteroids with
    [asteroids]. *)
