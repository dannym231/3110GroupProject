type conn = {
  fd : Unix.file_descr;
  buffer : bytes;
  mutable first : int;
  mutable last : int;
}

let default_port = 3110
let timeout = 3.
let max_frame = 262_144
let of_fd fd = { fd; buffer = Bytes.create 4096; first = 0; last = 0 }

let listen ~addr port =
  let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  try
    Unix.setsockopt fd Unix.SO_REUSEADDR true;
    Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_of_string addr, port));
    Unix.listen fd 1;
    fd
  with exn ->
    Unix.close fd;
    raise exn

let bound_port fd =
  match Unix.getsockname fd with
  | Unix.ADDR_INET (_, port) -> port
  | Unix.ADDR_UNIX _ -> invalid_arg "Net.bound_port: expected TCP listener"

let accept listener =
  let fd, _ = Unix.accept ~cloexec:true listener in
  try
    Unix.set_nonblock fd;
    of_fd fd
  with exn ->
    Unix.close fd;
    raise exn

let rec wait fd ~read deadline =
  let remaining =
    match deadline with
    | None -> -1.
    | Some until -> max 0. (until -. Unix.gettimeofday ())
  in
  match
    Unix.select
      (if read then [ fd ] else [])
      (if read then [] else [ fd ])
      [] remaining
  with
  | [], [], _ -> failwith "Timed out waiting for the other player."
  | _ -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait fd ~read deadline

let connect host port =
  match
    Unix.getaddrinfo host (string_of_int port)
      [ Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
  with
  | [] -> failwith ("Could not resolve host " ^ host ^ ".")
  | address :: _ -> (
      let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
      try
        Unix.set_nonblock fd;
        (try Unix.connect fd address.ai_addr
         with Unix.Unix_error ((Unix.EINPROGRESS | Unix.EINTR), _, _) -> (
           wait fd ~read:false (Some (Unix.gettimeofday () +. timeout));
           match Unix.getsockopt_error fd with
           | None -> ()
           | Some error -> raise (Unix.Unix_error (error, "connect", host))));
        of_fd fd
      with exn ->
        Unix.close fd;
        raise exn)

let send conn message =
  if String.length message > max_frame then failwith "LAN message is too large.";
  let frame = message ^ "\n" in
  let deadline = Some (Unix.gettimeofday () +. timeout) in
  let rec write offset =
    if offset < String.length frame then (
      wait conn.fd ~read:false deadline;
      match
        Unix.single_write_substring conn.fd frame offset
          (String.length frame - offset)
      with
      | 0 -> raise End_of_file
      | written -> write (offset + written)
      | exception
          Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
        -> write offset)
  in
  write 0

let recv ?timeout conn =
  let line = Buffer.create 256 in
  let deadline =
    Option.map (fun seconds -> Unix.gettimeofday () +. seconds) timeout
  in
  let rec read () =
    if conn.first = conn.last then (
      wait conn.fd ~read:true deadline;
      match Unix.read conn.fd conn.buffer 0 (Bytes.length conn.buffer) with
      | 0 -> raise End_of_file
      | count ->
          conn.first <- 0;
          conn.last <- count;
          read ()
      | exception
          Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
        -> read ())
    else
      let start = conn.first in
      let next = Bytes.index_from_opt conn.buffer start '\n' in
      let newline =
        match next with
        | Some i when i < conn.last -> Some i
        | _ -> None
      in
      let stop = Option.value newline ~default:conn.last in
      if Buffer.length line + stop - start > max_frame then
        failwith "LAN message is too large."
      else (
        Buffer.add_subbytes line conn.buffer start (stop - start);
        match newline with
        | Some index ->
            conn.first <- index + 1;
            Buffer.contents line
        | None ->
            conn.first <- conn.last;
            read ())
  in
  read ()

let shutdown conn =
  try Unix.shutdown conn.fd Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ()

let close conn = Unix.close conn.fd
