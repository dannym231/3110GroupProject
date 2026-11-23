val center_width : int
val height : int
val neighbor : int -> dx:int -> dy:int -> int option
val token : View_state.t -> View_state.player -> View_style.piece
val occupants : View_state.t -> int -> View_state.player list

val render :
  ?selected:int -> ?center:View_style.doc -> View_state.t -> View_style.doc
