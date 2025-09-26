open Constants
open Game_logic
open Types

let copy_board = Array.map Array.copy

let opposite_color = function
| Red -> Yellow
| Yellow -> Red

let get_depth = function
| Easy -> 2
| Normal -> 4
| Hard -> 6
| Impossible -> 8

let get_available_moves board =
  let center = board_width / 2 in
  List.init board_width Fun.id
  |> List.filter (fun column_index -> not (is_column_full board column_index))
  |> List.sort (fun col_1 col_2 -> 
    let dist_1 = abs (col_1 - center) in
    let dist_2 = abs (col_2 - center) in
    compare dist_1 dist_2
  )

let make_move board player_color column_index =
  let new_board = copy_board board in
  let (updated_board, _) = update_board new_board player_color column_index in
  updated_board

let count_threats board player_color =
  let directions = [(0, 1); (1, 0); (1, 1); (1, -1)] in
  let check_line row col (delta_row, delta_col) =
    let positions = List.init connect_length (fun idx -> 
      (row + idx * delta_row, col + idx * delta_col)) in
    if List.for_all (fun (r, c) -> r >= 0 && r < board_height && c >= 0 && c < board_width) positions then
      let line_cells = List.map (fun (r, c) -> board.(r).(c)) positions in
      let player_count = List.length (List.filter (function Filled color when color = player_color -> true | _ -> false) line_cells) in
      let empty_count = List.length (List.filter (function Empty -> true | _ -> false) line_cells) in
      let opponent_count = connect_length - player_count - empty_count in
      if opponent_count = 0 then Some (player_count, empty_count) else None
    else None
  in
  let all_potential = List.init board_height (fun row ->
    List.init board_width (fun col ->
      List.filter_map (check_line row col) directions
    ) |> List.flatten
  ) |> List.flatten in
  let count_patterns patterns =
    List.fold_left (fun (threes, twos) (filled, empty) ->
      match (filled, empty) with
      | (3, 1) -> (threes + 1, twos)
      | (2, 2) -> (threes, twos + 1)
      | _ -> (threes, twos)
    ) (0, 0) patterns
  in
  count_patterns all_potential

let evaluate_position board player_color =
  match check_game_over board with
  | Win (color, _) when color = player_color -> win_score
  | Win _ -> min_score
  | Full -> 0
  | Ongoing ->
    let opponent_color = opposite_color player_color in
    let center_bonus = 
      Array.fold_left (fun acc row ->
        match row.(board_width / 2) with
        | Filled color when color = player_color -> 3
        | _ -> 0
        |> (+) acc
      ) 0 board
    in
    let (player_threes, player_twos) = count_threats board player_color in
    let (opponent_threes, opponent_twos) = count_threats board opponent_color in
    center_bonus + (player_threes * 50) + (player_twos * 10) - (opponent_threes * 50) - (opponent_twos * 10)

let rec minimax board player_color maximizing_player depth alpha beta =
  match check_game_over board with
  | Win (color, _) when color = player_color -> win_score + depth
  | Win _ -> min_score - depth
  | Full -> 0
  | Ongoing when depth = 0 -> evaluate_position board player_color
  | Ongoing ->
    let current_color = if maximizing_player then player_color else opposite_color player_color in
    let available_moves = get_available_moves board in
    if maximizing_player then
      let rec max_eval moves best_score current_alpha =
        match moves with
        | [] -> best_score
        | move :: remaining_moves ->
          let new_board = make_move board current_color move in
          let eval_score = minimax new_board player_color false (depth - 1) current_alpha beta in
          let new_best = max best_score eval_score in
          let new_alpha = max current_alpha eval_score in
          if beta <= new_alpha then new_best
          else max_eval remaining_moves new_best new_alpha
      in
      max_eval available_moves min_score alpha
    else
      let rec min_eval moves best_score current_beta =
        match moves with
        | [] -> best_score
        | move :: remaining_moves ->
          let new_board = make_move board current_color move in
          let eval_score = minimax new_board player_color true (depth - 1) alpha current_beta in
          let new_best = min best_score eval_score in
          let new_beta = min current_beta eval_score in
          if new_beta <= alpha then new_best
          else min_eval remaining_moves new_best new_beta
      in
      min_eval available_moves max_score beta

let get_move_scores board player_color depth =
  let available_moves = get_available_moves board in
  List.map (fun move ->
    let new_board = make_move board player_color move in
    let score = minimax new_board player_color false (depth - 1) min_score max_score in
    (move, score)
  ) available_moves

let find_immediate_win board player_color =
  let available_moves = get_available_moves board in
  List.find_opt (fun move ->
    let test_board = make_move board player_color move in
    match check_game_over test_board with
    | Win (color, _) when color = player_color -> true
    | _ -> false
  ) available_moves

let find_blocking_move board player_color =
  let opponent_color = opposite_color player_color in
  let available_moves = get_available_moves board in
  List.find_opt (fun move ->
    let test_board = make_move board opponent_color move in
    match check_game_over test_board with
    | Win (color, _) when color = opponent_color -> true
    | _ -> false
  ) available_moves

let select_move_with_randomness difficulty move_scores =
  let sorted_moves = List.sort (fun (_, score_1) (_, score_2) -> compare score_2 score_1) move_scores in
  match sorted_moves with
  | [] -> failwith "No available moves"
  | [(move, _)] -> move
  | (best_move, best_score) :: (second_move, second_score) :: _ ->
    let use_second_best = match difficulty with
      | Easy -> Random.int 4 = 0
      | Normal -> Random.int 8 = 0
      | Hard -> Random.int 12 = 0
      | Impossible -> false
    in
    if use_second_best && (difficulty = Easy || abs (best_score - second_score) < 50) then second_move
    else best_move

let get_bot_move board player_color difficulty =
  Random.self_init ();
  match find_immediate_win board player_color with
  | Some move -> move
  | None ->
    match find_blocking_move board player_color with
    | Some move when difficulty <> Easy -> move
    | _ ->
      let depth = get_depth difficulty in
      let move_scores = get_move_scores board player_color depth in
      select_move_with_randomness difficulty move_scores
