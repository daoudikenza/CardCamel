type t = {
  content : string;
  speed : int;
  position : int * int; (* (row, col) *)
  creation_time : float;
  symbol : string;
}

(* A list of symbols to represent different asteroids. *)
let symbols = [ "@"; "#"; "*" ]

(* [assign_symbol content] assigns a unique symbol based on the content. *)
let assign_symbol content =
  let index = Hashtbl.hash content mod List.length symbols in
  List.nth symbols index

(** [create content col] initializes a new asteroid with random speed and symbol. *)
let create content col =
  let speed = Random.int 3 + 1 in
  (* Random speed between 1 and 3 *)
  let position = (0, col) in
  (* Start at the top row *)
  let creation_time = Unix.gettimeofday () in
  let symbol = assign_symbol content in
  { content; speed; position; creation_time; symbol }

(** [move asteroid frame_number] updates the asteroid's position based on speed. *)
let move asteroid frame_number =
  let row, col = asteroid.position in
  if frame_number mod asteroid.speed = 0 then
    { asteroid with position = (row + 1, col) }
  else asteroid

(** [destroyed asteroid time_taken] calculates the score for destroying the
    asteroid. *)
let destroyed asteroid time_taken =
  let speed = asteroid.speed in
  let score =
    int_of_float ((50. *. float_of_int speed) +. (100. /. time_taken))
  in
  score

(** [to_string asteroid] returns the visual representation of the asteroid. *)
let to_string asteroid = asteroid.symbol

(** [get_content asteroid] retrieves the asteroid's content. *)
let get_content asteroid = asteroid.content

(** [get_position asteroid] retrieves the asteroid's current position. *)
let get_position asteroid = asteroid.position

(** [get_creation_time asteroid] retrieves the asteroid's creation time. *)
let get_creation_time asteroid = asteroid.creation_time

(** [get_speed asteroid] retrieves the asteroid's speed. *)
let get_speed asteroid = asteroid.speed
