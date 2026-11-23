# Team 21 Team Happy

## Play

Open a terminal at least **120 columns wide and 47 rows tall** and run
`dune exec ./bin/main.exe`. If the terminal gets too small, play pauses until
you enlarge it again.

Alex and Blake start on Go with $1500. Pass names to play as someone else:
`dune exec ./bin/main.exe -- Sam Jordan`.

- **Tab** cycles between the board, player roster, and current player's items.
- **Arrows** move around the board or select a player, card, or deed in the sidebar.
- **Enter** inspects the selected tile, player, card, or deed. **Esc** returns to your selection.
- **R** rolls. On an unowned property, **B** buys and **P** passes. **Y** pays rent.
- **Q** quits.

Colored symbols on each tile match the players' names in the roster.

`dune exec ./bin/main.exe -- --tour` plays the scripted scenes, and **[** / **]**
browse them. `dune exec ./bin/main.exe -- --dump` prints those scenes without
an interactive terminal.

## Members
  - Daniel McCance drm329
  - Luca Occhipinti lo225
  - Charlie Foley clf89
  - Jaiden Grimminck jg2575
