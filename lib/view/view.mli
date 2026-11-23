type direction =
  | Up
  | Down
  | Left
  | Right

type key =
  | Arrow of direction
  | Enter
  | Escape
  | Tab
  | Letter of char

type event =
  | Key of key
  | Resize of (int * int)
  | End_of_input

type ui

val minimum_size : int * int

val initial : ?local_player:View_state.player_id -> View_state.t -> ui
(** Give [local_player] in a server session to keep the inventory on that
    player. Otherwise the inventory follows the active turn. *)

val update :
  size:int * int -> View_state.t -> ui -> key -> ui * View_state.command option

val render :
  ?size:int * int ->
  ?ui:ui ->
  ?preview:bool ->
  ?demo:bool ->
  View_state.t ->
  View_style.doc
(** [demo] adds the scripted-tour hint to the status line. *)

val plain : View_style.doc -> string
val show : View_style.doc -> unit
val command_of_key : View_state.t -> char -> View_state.command option
val isatty : unit -> bool
val terminal_size : unit -> int * int
val with_terminal : (unit -> 'a) -> 'a
val read_event : size:int * int -> event
