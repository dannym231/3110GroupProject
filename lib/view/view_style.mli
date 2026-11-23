(** Plain pieces and the ANSI colors for a property group. *)

type t = {
  fg : ANSITerminal.color option;
  bg : ANSITerminal.color option;
  bold : bool;
  inverse : bool;
}

type piece = {
  style : t;
  text : string;
  columns : int;
}

type line = piece list
type doc = line list

val none : t
val bold : t
val selected : t
val of_group : View_state.color_group -> t
val piece : ?columns:int -> t -> string -> piece
val plain : string -> piece
val title : string -> line
val line : string -> line
val clip : int -> string -> string
val fit : int -> string -> string
val line_width : line -> int
val crop : int -> line -> line
val pad : int -> line -> line
val wrap : int -> string -> string list
val text : doc -> string
val show : doc -> unit
