open ANSITerminal
open Types
open User_choice

let get_bot_color () =
  let options = [([green], "Player"); ([cyan], "Bot")] in
  prompt options "Who goes first?"
  |> function
  | "Player" -> Yellow
  | _ -> Red

let get_bot_difficulty () =
  let options = [([blue], "Easy"); ([green], "Normal"); ([magenta], "Hard"); ([red], "Impossible")] in
  prompt options "How difficult should the bot be?"
  |> function
  | "Easy" -> Easy
  | "Normal" -> Normal
  | "Hard" -> Hard
  | _ -> Impossible

let configure_game () =
  erase Screen;
  let options = [([green], "Player"); ([cyan], "Bot")] in
  prompt options "Would you like to play against another player or a bot?"
  |> function
  | "Player" -> Player Red
  | _ ->
    let color = get_bot_color () in
    let diff = get_bot_difficulty () in
    Bot (color, diff)

let play_again () =
  let options = [([green], "Yes"); ([red], "No")] in
  prompt ~question_pos:(1, 11) options "Play again?"
  |> function
  | "Yes" -> true
  | _ -> false
