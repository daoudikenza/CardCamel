type t = {
  rows : int;
  cols : int;
  grid : string array array;
  asteroids : Asteroid.t list;
}
(** A record type is used for representing the game board

    @param rows The number of rows in the board.
    @param cols The number of columns in the board.
    @param grid A 2D array of strings representing the grid cells.
    @param asteroids A list of asteroids present on the board. *)

(** [create_empty_grid rows cols] creates an empty grid filled with "." *)
let create_empty_grid rows cols = Array.init rows (fun _ -> Array.make cols ".")

(** [initialize rows cols asteroids] creates a new board with given dimensions
    and asteroids *)
let initialize rows cols asteroids =
  let grid = create_empty_grid rows cols in
  { rows; cols; grid; asteroids }

(** [get_rows] is the number of columns in [board] *)
let get_rows board = board.rows

(** [get_cols] is the number of columns in [board] *)
let get_cols board = board.cols

(** [place_asteroid grid asteroid] places the asteroid's symbol at its position
    on the grid *)
let place_asteroid grid asteroid =
  let row, col = Asteroid.get_position asteroid in
  if row >= 0 && row < Array.length grid then
    let cols = Array.length grid.(0) in
    if col >= 0 && col < cols then
      grid.(row).(col) <- Asteroid.to_string asteroid

(** [update_grid board] updates the board's grid based on current asteroids *)
let update_grid board =
  let grid = create_empty_grid board.rows board.cols in
  List.iter (place_asteroid grid) board.asteroids;
  { board with grid }

(** [add_asteroid board asteroid] adds a new asteroid to the board *)
let add_asteroid board asteroid =
  let updated_board = { board with asteroids = asteroid :: board.asteroids } in
  update_grid updated_board

(** [move_asteroids board frame_number] moves all asteroids based on their speed *)
let move_asteroids board frame_number =
  let moved_asteroids =
    List.map
      (fun asteroid -> Asteroid.move asteroid frame_number)
      board.asteroids
  in
  let updated_board = { board with asteroids = moved_asteroids } in
  update_grid updated_board

(** [check_collision board] checks for asteroids that reached the bottom *)
let check_collision board =
  let at_bottom, remaining =
    List.partition
      (fun asteroid ->
        let row, _ = Asteroid.get_position asteroid in
        row >= board.rows)
      board.asteroids
  in
  let updated_board = { board with asteroids = remaining } in
  (at_bottom, update_grid updated_board)

(** [resize board new_rows new_cols] resizes the board dimensions *)
let resize board new_rows new_cols =
  let updated_board = { board with rows = new_rows; cols = new_cols } in
  update_grid updated_board

(* let red = "\027[31m" let green = "\027[32m" *)
let yellow = "\027[33m"
let blue = "\027[34m"
let reset = "\027[0m"
let bold = "\027[1m"

(* let create_score_line width = let dots = String.init width (fun _ -> '.') in
   print_endline (blue ^ "┃ " ^ dots ^ " ┃" ^ reset) *)
let render board =
  print_string "\027[H\027[2J";

  (* Clear the screen *)

  (* Print game header *)
  let header = "╭━━━━━━━━━━━ FLASHCARD INVADERS ━━━━━━━━━━━╮" in
  print_endline (bold ^ yellow ^ header ^ reset);
  (* Print score area - placeholder for now *)
  print_endline (blue ^ "║" ^ String.make board.cols ' ' ^ "║" ^ reset);

  (* Print the game area with borders *)
  for i = 0 to board.rows - 1 do
    print_string (blue ^ "║ " ^ reset);
    for j = 0 to board.cols - 1 do
      let cell = board.grid.(i).(j) in
      match cell with
      | "." -> print_string "·" (* Use middle dot for empty space *)
      | "*" -> print_string "*" (* Colorful star for asteroids *)
      | _ -> print_string cell
    done;
    print_endline (blue ^ " ║" ^ reset)
  done;

  (* Print the bottom border with a fancy design *)
  print_endline (blue ^ "╚" ^ String.make (board.cols + 2) '=' ^ "╝" ^ reset)

(* Print game status footer *)

(** [get_dimensions board] returns the dimensions of the board *)
let get_dimensions board = (board.rows, board.cols)

(** [get_asteroids board] returns the list of active asteroids *)
let get_asteroids board = board.asteroids

(** [update_asteroids board asteroids] updates the board's asteroids *)
let update_asteroids board asteroids =
  let updated_board = { board with asteroids } in
  update_grid updated_board

(** [get_grid] is the grid of [board]*)
let get_grid board = board.grid
