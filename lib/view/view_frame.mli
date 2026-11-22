val minimum_size : int * int
val fits : int * int -> bool
val scroll_limit : size:int * int -> View_state.t -> View_input.t -> int

val render :
  size:int * int ->
  preview:bool ->
  View_input.t ->
  View_state.t ->
  View_style.doc
