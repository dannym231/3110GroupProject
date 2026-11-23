(** The game board: squares loaded from a JSON file, plus the rent rules that
    depend only on the board and who owns what. *)

type kind =
  | Go
  | Property of {
      group : string;
      price : int;
      rent : int;
    }  (** A colored street. [rent] is the base rent with no houses. *)
  | Railroad of { price : int }
  | Utility of { price : int }
  | Tax of { amount : int }
  | Chance
  | Community_chest
  | Jail
  | Free_parking
  | Go_to_jail

type square = {
  name : string;
  kind : kind;
}

type t
(** A board of squares indexed from 0 (Go). *)

val of_string : string -> (t, string) result
(** [of_string json] parses a board. Returns [Error msg] if the text is not
    JSON, a square is missing a field or has an unknown kind, a price, rent, or
    tax is negative, the board is empty, or square 0 is not Go. *)

val load : string -> (t, string) result
(** [load path] reads and parses the board file at [path]. Returns [Error msg]
    if the file cannot be read or [of_string] rejects it. *)

val size : t -> int
(** Number of squares. *)

val square : t -> int -> square
(** [square b i] is square [i]. Requires: [0 <= i < size b]. *)

val price : square -> int option
(** Purchase price, or [None] if the square cannot be bought. *)

val group_members : t -> string -> int list
(** Indices of every street in a color group, in board order. *)

val rent : t -> owner_of:(int -> string option) -> dice_total:int -> int -> int
(** [rent b ~owner_of ~dice_total i] is the rent owed for landing on owned
    square [i]. Streets charge base rent, doubled when one owner holds the
    whole color group. Railroads charge 25, 50, 100, or 200 by how many the
    owner holds. Utilities charge 4x the dice, or 10x if the owner holds both.
    Returns 0 if [i] is unowned or not buyable. *)
