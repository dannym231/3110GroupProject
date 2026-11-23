(** Newline-delimited messages over IPv4 TCP. *)

type conn

val default_port : int
val listen : addr:string -> int -> Unix.file_descr
val bound_port : Unix.file_descr -> int
val accept : Unix.file_descr -> conn
val connect : string -> int -> conn
val send : conn -> string -> unit

val recv : ?timeout:float -> conn -> string
(** A missing timeout allows an idle connection. Raises [End_of_file] when the
    peer closes and [Failure] on a timeout or an oversized frame. *)

val shutdown : conn -> unit
(** Interrupt a worker blocked on this connection before reaping it. *)

val close : conn -> unit
