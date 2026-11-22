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

type focus =
  | Board
  | Roster
  | Inventory

type detail =
  | Tile of int
  | Player of View_state.player_id
  | Held_card of View_state.held_card

type t = {
  focus : focus;
  tile : int option;
  player : View_state.player_id option;
  local_player : View_state.player_id option;
  item_index : int;
  detail : detail option;
  scroll : int;
}

val initial : ?local_player:View_state.player_id -> View_state.t -> t
val inventory_player : View_state.t -> t -> View_state.player option
val is_quit : key -> bool
val command_of_key : View_state.t -> char -> View_state.command option

val update :
  View_state.t ->
  t ->
  scroll_limit:(unit -> int) ->
  key ->
  t * View_state.command option

val decode : read_more:(unit -> char option) -> char -> key option
