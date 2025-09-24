# Connect 4

A terminal-based Connect 4 game written in OCaml with AI opponent support.

## Features

- Interactive terminal gameplay with ANSI color support
- AI bot opponents with 4 difficulty levels (Easy, Normal, Hard, Impossible)
- Player vs Player and Player vs Bot modes
- Minimax algorithm with alpha-beta pruning for AI decision making
- Comprehensive test suite using Alcotest
- Functional programming style throughout

## Requirements

- OCaml (tested on 5.3.0)
- Dune (tested on 3.20.1)
- OPAM packages:
  - `ANSITerminal` - for terminal colors and cursor control
  - `alcotest` - for testing (dev dependency)

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/daniel-cardone/ocaml-connect4
   cd connect4
   ```

2. Install dependencies:
   ```bash
   opam install --deps-only .
   ```

3. Build the project:
   ```bash
   dune build
   ```
Note: This is only known to work on Unix-based systems

## Usage

### Run the game:
```bash
dune exec connect4
```

### Run tests:
```bash
dune test
```

## How to Play

1. Game Setup: Choose between Player vs Player or Player vs Bot
2. Bot Configuration: If playing against bot, select difficulty and who goes first
3. Gameplay:
   - Use arrow keys to select column
   - Press Enter or Space to drop piece
   - Connect 4 pieces horizontally, vertically, or diagonally to win

## AI Implementation

The bot uses a minimax algorithm with alpha-beta pruning:

- Evaluation function considers piece positioning and threat detection
- Variable depth search based on difficulty level:
  - Easy: 2 moves ahead
  - Normal: 4 moves ahead  
  - Hard: 6 moves ahead
  - Impossible: 8 moves ahead
- Center-column prioritization for better opening play
- Threat detection for offensive and defensive play
