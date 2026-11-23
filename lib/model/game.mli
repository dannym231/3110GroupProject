(** The turn engine. A [Game.t] is an immutable snapshot of a game; every action
    returns a new snapshot or an error explaining why the action is not allowed
    right now. Dice values are passed in, so tests and replays can fix them. *)

type player = {
  id : string;
  name : string;
  cash : int;
  position : int;
}

type phase =
  | Awaiting_roll  (** The current player must roll. *)
  | Awaiting_buy of int
      (** The current player landed on this unowned square and must buy or
          pass. *)
  | Awaiting_rent of {
      square : int;
      owner : string;
      amount : int;
    }  (** The current player owes rent and must pay. *)

type t

val starting_cash : int
(** 1500 *)

val go_salary : int
(** 200 *)

val create : Board.t -> (string * string) list -> t
(** [create board players] starts a game. [players] lists [(id, name)] pairs in
    turn order; everyone starts on Go with [starting_cash]. Raises
    [Invalid_argument] if there are fewer than two players or ids repeat. *)

val board : t -> Board.t
val players : t -> player list
val current : t -> player
val turn : t -> int
(** Turn number, starting at 1. *)

val phase : t -> phase
val owner : t -> int -> string option
val last_dice : t -> (int * int) option

val log : t -> string list
(** Events so far, oldest first. *)

val roll : int * int -> t -> (t, string) result
(** [roll (d1, d2) g] moves the current player [d1 + d2] squares, paying
    [go_salary] if they pass or land on Go, then resolves the landing square:
    an unowned buyable square leads to [Awaiting_buy], someone else's to
    [Awaiting_rent], and anything else ends the turn. Error unless the phase is
    [Awaiting_roll] and both dice are in 1..6. *)

val buy : t -> (t, string) result
(** Buy the square in [Awaiting_buy] and end the turn. Error in any other phase
    or if the player cannot afford it. *)

val pass : t -> (t, string) result
(** Decline the square in [Awaiting_buy] and end the turn. *)

val pay : t -> (t, string) result
(** Pay the rent in [Awaiting_rent] to its owner and end the turn. Cash may go
    negative until bankruptcy is implemented. *)

val note : string -> t -> t
(** Add a message to the log without changing anything else. *)