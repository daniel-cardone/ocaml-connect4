type player_color = Red | Yellow

type cell = Empty | Filled of player_color

type board = cell array array

type bot_difficulty = Easy | Normal | Hard | Impossible

type game_type = Player of player_color | Bot of player_color * bot_difficulty

type position = int * int

type game_state = Win of player_color * position list | Full | Ongoing
