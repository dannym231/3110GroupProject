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

## Play over LAN

With both computers on the same network, start the host from the project
directory and give the other player your LAN IP address.

```sh
dune exec ./bin/main.exe -- --host Alex
dune exec ./bin/main.exe -- --join HOST_IP Blake
```

The host starts the game as soon as one guest joins and takes the first turn.
Each player can act during their own turn. If either player quits or disconnects,
the session ends. For a different port, add `--port PORT` to both commands.
The default is `3110`. The host also accepts `--seed N` for repeatable dice rolls.

## Members
  - Daniel McCance drm329
  - Luca Occhipinti lo225
  - Charlie Foley clf89
  - Jaiden Grimminck jg2575
