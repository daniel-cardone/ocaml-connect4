open Types
open Constants

let check_win_at_position board row col =
  let directions = [(0, 1); (1, 0); (1, 1); (1, -1)] in
  let piece = board.(row).(col) in
  match piece with
  | Empty -> None
  | Filled color ->
    let check_direction (delta_row, delta_col) =
      let count_in_direction dr dc start_r start_c =
        let rec count current_row current_col (acc, positions) =
          if current_row >= 0 && current_row < board_height && 
             current_col >= 0 && current_col < board_width &&
             board.(current_row).(current_col) = piece then
            count (current_row + dr) (current_col + dc) ((acc + 1), (current_col, current_row) :: positions)
          else (acc, positions)
        in
        count start_r start_c (0, [])
      in
      let forward_count, forward_positions = count_in_direction delta_row delta_col (row + delta_row) (col + delta_col) in
      let backward_count, backward_positions = count_in_direction (-delta_row) (-delta_col) (row - delta_row) (col - delta_col) in
      (1 + forward_count + backward_count >= connect_length, (col, row) :: forward_positions @ backward_positions)
    in
    let rec aux = function
    | [] -> None
    | dir :: rest ->
      match check_direction dir with
      | (true, positions) -> Some (color, positions)
      | _ -> aux rest
    in aux directions

let check_game_over_with_last_move board last_row last_col =
  match check_win_at_position board last_row last_col with
  | Some (color, positions) -> Win (color, positions)
  | None ->
    match Array.for_all (fun cell -> cell <> Empty) board.(0) with
    | true -> Full
    | false -> Ongoing

let check_game_over board =
  let rec check_all_positions row col =
    if row >= board_height then Ongoing
    else if col >= board_width then check_all_positions (row + 1) 0
    else
      match check_win_at_position board row col with
      | Some (color, positions) -> Win (color, positions)
      | None -> check_all_positions row (col + 1)
  in
  match check_all_positions 0 0 with
  | Win (color, positions) -> Win (color, positions)
  | _ ->
    match Array.for_all (fun cell -> cell <> Empty) board.(0) with
    | true -> Full
    | false -> Ongoing

let is_column_full board col =
  match board.(0).(col) with
  | Empty -> false
  | _ -> true

let update_board board color col =
  let rec find_bottom current_row =
    match board.(current_row).(col) with
    | Empty -> current_row
    | _ -> find_bottom (current_row - 1)
  in
  let final_row = find_bottom (board_height - 1) in
  board.(final_row).(col) <- Filled color;
  (board, final_row)