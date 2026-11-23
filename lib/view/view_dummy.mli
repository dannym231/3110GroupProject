(** Scripted snapshots. [apply] follows a link or stays put. It does not change
    cash or ownership itself. *)

type t

val ids : unit -> string list
val jump : string -> t
val id : t -> string
val snapshot : t -> View_state.t
val is_preview : t -> bool
val apply : t -> View_state.command -> t
