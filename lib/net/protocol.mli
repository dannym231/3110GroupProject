(** The request/response protocol carried by [Net]. Each value is one JSON line.
*)

type request =
  | Join of string
  | Poll
  | Act of {
      revision : int;
      command : View_state.command;
    }

type response =
  | Joined of View_state.player_id * Session.snapshot
  | State of Session.snapshot
  | Rejected of string

val encode_request : request -> string
val decode_request : string -> (request, string) result
val encode_response : response -> string
val decode_response : string -> (response, string) result
