open Connect4
open Types

let hide_cursor () =
  print_endline "\027[?25l"

let show_cursor () =
  print_endline "\027[?25h"

let () =
  hide_cursor ();
  at_exit show_cursor;
  Sys.catch_break true;
  try
    let rec run_games () =
      let game_type = Setup.configure_game () in
      begin match game_type with
      | Player _ -> Game_loop.init (Player Red) (Player Yellow)
      | Bot (Red, _) -> Game_loop.init game_type (Player Yellow)
      | _ -> Game_loop.init (Player Red) game_type
      end;
      if Setup.play_again () then run_games ()
    in
    run_games ()
  with
  | _ -> show_cursor ()
