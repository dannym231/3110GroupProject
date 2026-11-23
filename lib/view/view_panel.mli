val width : int
val render : ?selected:View_state.player_id -> View_state.t -> View_style.doc

type item =
  | Card of View_state.held_card
  | Deed of View_state.square

val items : View_state.t -> View_state.player -> item list
val item_index : item list -> int -> int

val inventory :
  heading:string ->
  ?selected:int ->
  View_state.t ->
  View_state.player option ->
  View_style.doc

val tile_details : View_state.t -> int -> View_style.doc
val player_details : View_state.t -> View_state.player_id -> View_style.doc
