(** The controller shared by the host terminal and its LAN guests. *)

type t

type snapshot = {
  revision : int;
  view : View_state.t;
}

val create : roll:(unit -> int * int) -> Game.t -> t
val snapshot : t -> snapshot

val submit :
  t ->
  player:View_state.player_id ->
  revision:int ->
  View_state.command ->
  (snapshot, string) result
(** Check the actor, revision and game rules together. Every accepted action
    increases the revision, including actions that leave the turn unchanged. *)
