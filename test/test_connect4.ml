open Alcotest
open Connect4.Bot
open Connect4.Game_logic
open Connect4.Types

let empty_board () = Array.make_matrix 6 7 Empty

let place_pieces board pieces =
  List.iter (fun (row, col, color) -> 
    board.(row).(col) <- Filled color
  ) pieces;
  board

let make_board pieces =
  let board = empty_board () in
  place_pieces board pieces

let game_state = testable (fun ppf -> function
  | Win (Red, _) -> Format.fprintf ppf "Red wins"
  | Win (Yellow, _) -> Format.fprintf ppf "Yellow wins"  
  | Full -> Format.fprintf ppf "Board full"
  | Ongoing -> Format.fprintf ppf "Game ongoing"
) (fun s1 s2 -> match s1, s2 with
  | Win (c1, _), Win (c2, _) -> c1 = c2
  | Full, Full | Ongoing, Ongoing -> true
  | _ -> false)

let cell = testable (fun ppf -> function
  | Empty -> Format.fprintf ppf "Empty"
  | Filled Red -> Format.fprintf ppf "Red"
  | Filled Yellow -> Format.fprintf ppf "Yellow"
) (=)

let color = testable (fun ppf -> function
  | Red -> Format.fprintf ppf "Red"
  | Yellow -> Format.fprintf ppf "Yellow"
) (=)

let should_win expected_color board =
  match check_game_over board with
  | Win (actual_color, _) when actual_color = expected_color -> ()
  | state -> 
    let state_str = match state with
      | Win (c, _) -> Format.asprintf "%a wins" (fun ppf -> function 
        | Red -> Format.fprintf ppf "Red" | Yellow -> Format.fprintf ppf "Yellow") c
      | Full -> "Board full"
      | Ongoing -> "Game ongoing"
    in
    let expected_str = match expected_color with
      | Red -> "Red" | Yellow -> "Yellow"
    in
    Alcotest.fail (Printf.sprintf "Expected %s to win, but got: %s" expected_str state_str)

let test_empty_board_is_ongoing () =
  let board = empty_board () in
  check game_state "Empty board should be ongoing" Ongoing (check_game_over board)

let test_horizontal_wins () =
  let red_horizontal = make_board [(5, 0, Red); (5, 1, Red); (5, 2, Red); (5, 3, Red)] in
  should_win Red red_horizontal;

  let yellow_horizontal = make_board [(3, 2, Yellow); (3, 3, Yellow); (3, 4, Yellow); (3, 5, Yellow)] in
  should_win Yellow yellow_horizontal

let test_vertical_wins () =
  let red_vertical = make_board [(5, 3, Red); (4, 3, Red); (3, 3, Red); (2, 3, Red)] in
  should_win Red red_vertical;

  let yellow_vertical = make_board [(5, 0, Yellow); (4, 0, Yellow); (3, 0, Yellow); (2, 0, Yellow)] in
  should_win Yellow yellow_vertical

let test_diagonal_wins () =
  let red_ascending = make_board [(5, 0, Red); (4, 1, Red); (3, 2, Red); (2, 3, Red)] in
  should_win Red red_ascending;

  let yellow_descending = make_board [(2, 3, Yellow); (3, 2, Yellow); (4, 1, Yellow); (5, 0, Yellow)] in
  should_win Yellow yellow_descending;

  let red_descending = make_board [(0, 6, Red); (1, 5, Red); (2, 4, Red); (3, 3, Red)] in
  should_win Red red_descending

let test_no_false_wins () =
  let three_in_row = make_board [(5, 1, Red); (5, 2, Red); (5, 3, Red)] in
  check game_state "Three in a row should not win" Ongoing (check_game_over three_in_row);

  let mixed_colors = make_board [(5, 0, Red); (5, 1, Yellow); (5, 2, Red); (5, 3, Red)] in
  check game_state "Mixed colors should not win" Ongoing (check_game_over mixed_colors);

  let broken_diagonal = make_board [(5, 0, Red); (4, 1, Red); (3, 2, Yellow); (2, 3, Red)] in
  check game_state "Broken diagonal should not win" Ongoing (check_game_over broken_diagonal)

let test_full_board_detection () =
  let board = empty_board () in
  List.iteri (fun col _ -> 
    board.(0).(col) <- if col mod 2 = 0 then Filled Red else Filled Yellow
  ) [0; 1; 2; 3; 4; 5; 6];
  check game_state "Top row filled should indicate full board" Full (check_game_over board)

let test_column_operations () =
  let empty = empty_board () in
  check bool "Empty column should not be full" false (is_column_full empty 3);

  let board_with_top_piece = make_board [(0, 3, Red)] in
  check bool "Column with top piece should be full" true (is_column_full board_with_top_piece 3);

  let board_missing_top = make_board [(5, 2, Red); (4, 2, Yellow); (3, 2, Red); (2, 2, Yellow); (1, 2, Red)] in
  check bool "Column missing only top should not be full" false (is_column_full board_missing_top 2)

let test_piece_placement () =
  let board = empty_board () in
  let (updated_board, final_row) = update_board board Red 3 in
  check cell "Piece should be placed" (Filled Red) updated_board.(final_row).(3);
  check int "Should place at bottom row" 5 final_row;
  check bool "Should return same board reference" true (updated_board == board);

  let board_with_piece = make_board [(5, 3, Yellow)] in
  let (stacked_board, stack_row) = update_board board_with_piece Red 3 in
  check cell "Should stack on top" (Filled Red) stacked_board.(stack_row).(3);
  check int "Should stack at row 4" 4 stack_row;
  check cell "Bottom piece should remain" (Filled Yellow) stacked_board.(5).(3)

let test_realistic_game_scenarios () =
  let almost_win = make_board [
    (5, 1, Red); (5, 2, Red); (5, 3, Red);
    (4, 1, Yellow); (4, 2, Yellow); 
    (3, 1, Red);
  ]
  in
  check game_state "Almost winning should be ongoing" Ongoing (check_game_over almost_win);

  let winning_move = make_board [
    (5, 0, Red); (5, 1, Red); (5, 2, Red); (5, 3, Red);
    (4, 0, Yellow); (4, 1, Yellow); (4, 2, Yellow);
    (3, 0, Red); (3, 1, Red);
  ]
  in
  should_win Red winning_move

let test_bot_move_prioritization () =
  let empty = empty_board () in
  let moves = get_available_moves empty in
  check (list int) "Should prioritize center columns" [3; 2; 4; 1; 5; 0; 6] moves;

  let blocked_center = make_board (List.init 6 (fun i -> (i, 3, Red))) in
  let moves_blocked = get_available_moves blocked_center in
  check bool "Should exclude full center column" true (not (List.mem 3 moves_blocked));
  check int "Should have 6 available moves" 6 (List.length moves_blocked)

let test_bot_move_execution () =
  let board = empty_board () in
  let new_board = make_move board Yellow 2 in
  check cell "Bot should place piece" (Filled Yellow) new_board.(5).(2);
  check cell "Original board should be unchanged" Empty board.(5).(2);

  let partial_board = make_board [(5, 2, Red); (4, 2, Yellow)] in
  let bot_board = make_move partial_board Red 2 in
  check cell "Bot should stack correctly" (Filled Red) bot_board.(3).(2)

let test_bot_utilities () =
  check color "Red opposite should be Yellow" Yellow (opposite_color Red);
  check color "Yellow opposite should be Red" Red (opposite_color Yellow);

  let original = make_board [(5, 3, Red); (4, 3, Yellow)] in
  let copied = copy_board original in
  copied.(5).(3) <- Filled Yellow;
  check cell "Original should be unchanged" (Filled Red) original.(5).(3);
  check cell "Copy should be modified" (Filled Yellow) copied.(5).(3)

let test_edge_cases () =
  let corner_win = make_board [(0, 0, Red); (0, 1, Red); (0, 2, Red); (0, 3, Red)] in
  should_win Red corner_win;

  let edge_vertical = make_board [(5, 6, Yellow); (4, 6, Yellow); (3, 6, Yellow); (2, 6, Yellow)] in
  should_win Yellow edge_vertical;

  let single_piece = make_board [(3, 3, Red)] in
  check game_state "Single piece should not win" Ongoing (check_game_over single_piece)

let game_logic_tests = [
  "empty board is ongoing", `Quick, test_empty_board_is_ongoing;
  "horizontal wins", `Quick, test_horizontal_wins;
  "vertical wins", `Quick, test_vertical_wins;
  "diagonal wins", `Quick, test_diagonal_wins;
  "no false wins", `Quick, test_no_false_wins;
  "full board detection", `Quick, test_full_board_detection;
  "column operations", `Quick, test_column_operations;
  "piece placement", `Quick, test_piece_placement;
  "realistic game scenarios", `Quick, test_realistic_game_scenarios;
  "edge cases", `Quick, test_edge_cases;
]

let bot_tests = [
  "move prioritization", `Quick, test_bot_move_prioritization;
  "move execution", `Quick, test_bot_move_execution;
  "bot utilities", `Quick, test_bot_utilities;
]

let () = run "Connect4 Game Tests" [
  "Game Logic", game_logic_tests;
  "Bot Logic", bot_tests;
]
