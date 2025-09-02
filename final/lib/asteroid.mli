type t
(** The type representing an asteroid. *)

val create : string -> int -> t
(** [create content col] initializes a new asteroid with the given [content] and
    starting at column [col] at the top of the board. The asteroid is assigned a
    random speed between 1 and 3 and the current timestamp as [creation_time]. *)

val move : t -> int -> t
(** [move asteroid frame_number] updates the asteroid's position based on its
    speed and the current [frame_number]. It moves the asteroid down one row if
    the frame number is a multiple of its speed. *)

val destroyed : t -> float -> int
(** [destroyed asteroid time_taken] calculates the score for destroying the
    asteroid based on its speed and the [time_taken] to respond. *)

val to_string : t -> string
(** [to_string asteroid] returns the visual representation (symbol) of the
    asteroid, e.g., "@", "#", "*". *)

val get_content : t -> string
(** [get_content asteroid] retrieves the content (question or answer) of the
    asteroid. *)

val get_position : t -> int * int
(** [get_position asteroid] returns the current (row, column) position of the
    asteroid on the board. *)

val get_creation_time : t -> float
(** [get_creation_time asteroid] returns the timestamp when the asteroid was
    created. *)

val get_speed : t -> int
(** [get_speed asteroid] returns the speed of the asteroid. *)

val assign_symbol : string -> string
(** [assign_symbol content] assigns a unique symbol based on the content. *)
