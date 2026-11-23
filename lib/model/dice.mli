(** Random dice. Seeding makes a game replayable. *)

val roller : ?seed:int -> unit -> unit -> int * int
(** [roller ?seed ()] returns a function that rolls two six-sided dice each
    time it is called. With the same [seed], the same sequence of rolls comes
    out every time; without one, rolls differ every run. *)