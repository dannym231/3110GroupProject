(** A two-player LAN game. The host terminal and guest use the same operations.
*)

type t

type error =
  | Rejected of string
  | Disconnected of string

val accept :
  Unix.file_descr ->
  board:Board.t ->
  roll:(unit -> int * int) ->
  name:string ->
  t
(** Wait for a valid guest handshake. The caller owns the listening socket and
    may close it once the guest has joined. The host takes the first turn. *)

val join : address:string -> port:int -> name:string -> t
(** Join a waiting host. Connection and handshake errors raise an exception. *)

val player : t -> View_state.player_id
val poll : t -> (Session.snapshot, error) result

val submit :
  t -> revision:int -> View_state.command -> (Session.snapshot, error) result

val close : t -> unit
