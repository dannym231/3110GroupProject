(** The live game as the terminal sees it. [snapshot] fills a [View_state.t]
    from the engine, and [apply] runs the command that snapshot offered. *)

val snapshot : Game.t -> View_state.t
(** [snapshot g] is the board, players, prompt, and menu for [g]. *)

val try_apply :
  roll:(unit -> int * int) ->
  View_state.command ->
  Game.t ->
  (Game.t, string) result
(** Apply a gameplay command. Rejections leave the game and its log unchanged.
*)

val apply : roll:(unit -> int * int) -> View_state.command -> Game.t -> Game.t
(** [apply ~roll cmd g] runs [cmd]. [roll] is called only for a legal roll. A
    rejected action is written to the log and leaves the phase unchanged.
    [Quit], [Next], and [Prev] leave [g] unchanged. *)
