open ANSITerminal
open Bot
open Constants
open Game_logic
open Types
open User_choice

let print_board ?(win_positions=[]) board =
  set_cursor 1 1;
  erase Below;
  set_cursor 1 2;
  Array.iteri (fun i row ->
    Array.iteri (fun j cell ->
      let styles, text =
        match cell with
        | Filled Red -> ([red], "O")
        | Filled Yellow -> ([yellow], "O")
        | Empty -> ([black], " ")
      in
      let bracket_style = if List.mem (j, i) win_positions then [green] else [white] in
      print_string bracket_style "[";
      print_string styles text;
      print_string bracket_style "]";
    ) row;
    print_string [Reset] "\n"
  ) board

let get_player_move board color =
  let available_columns = List.filter (fun column_index -> not (is_column_full board column_index)) (List.init board_width Fun.id) in
  let options = List.map (fun column_index -> ((), column_index)) available_columns in
  let option_length = 1 in
  let option_lengths = Array.make (List.length available_columns) option_length in
  let gen_spacing n =
    let selected_col = List.nth available_columns n in
    String.make (1 + selected_col * 3) ' '
  in
  set_cursor 1 8;
  save_cursor ();
  let cursor_style =
    match color with
    | Red -> [red]
    | _ -> [yellow]
  in
  get_choice ~cursor_style gen_spacing options option_lengths

let get_move board player =
  match player with
  | Player color -> get_player_move board color
  | Bot (color, diff) -> 
    print_string [white] "Thinking...";
    flush stdout;
    Key_reader.with_raw_mode (fun _ -> get_bot_move board color diff)
  |> fun move ->
      Unix.sleepf debounce_seconds;
      Unix.tcflush Unix.stdin Unix.TCIFLUSH;
      move

let display_winner board winner =
  let style_from_winner = function
  | Red -> [red]
  | _ -> [yellow]
  in
  match winner with
  | Some (Player color, positions) ->
    print_board ~win_positions:positions board;
    print_string (style_from_winner color) ("\n" ^ (if color = Red then "Red" else "Yellow" ) ^ " player wins!\n")
  | Some (Bot (color, _), positions) ->
    print_board ~win_positions:positions board;
    print_string (style_from_winner color) "\nThe bot wins!\n"
  | None ->
    print_string [white] "\nDraw\n"

let init player_1 player_2 =
  let board = Array.make_matrix board_height board_width Empty in
  let play_turn player color =
    let column_choice = get_move board player in
    let (updated_board, placed_row) = update_board board color column_choice in
    print_board updated_board;
    check_game_over_with_last_move updated_board placed_row column_choice
  in
  let rec play () =
    match play_turn player_1 Red with
    | Win (_, positions) -> Some (player_1, positions)
    | Full -> None
    | Ongoing ->
      match play_turn player_2 Yellow with
      | Win (_, positions) -> Some (player_2, positions)
      | Full -> None
      | Ongoing -> play ()
  in
  print_board board;
  play ()
  |> display_winner board
