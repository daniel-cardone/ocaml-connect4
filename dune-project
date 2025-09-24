(lang dune 3.20)

(name connect4)

(generate_opam_files true)

(source
 (github daniel-cardone/ocaml-connect4))

(authors "Daniel Cardone <dcard278@gmail.com>")

(maintainers "Daniel Cardone <dcard278@gmail.com>")

(license MIT)

(homepage "https://github.com/daniel-cardone/ocaml-connect4")

(documentation "https://github.com/daniel-cardone/ocaml-connect4")

(bug_reports "https://github.com/daniel-cardone/ocaml-connect4/issues")

(package
 (name connect4)
 (synopsis "A Connect4 game with either a Human or AI opponent")
 (description "A Connect4 game playable in the terminal with either a Human or AI opponent. The AI uses the minimax algorithm with alpha-beta pruning to determine its moves.")
 (depends 
  ocaml
  dune
  ANSITerminal
  (alcotest :with-test)
  (odoc :with-doc))
 (tags
  ("game" "connect4" "ai" "cli" "tui")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
