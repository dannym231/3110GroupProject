type error =
  | Rejected of string
  | Disconnected of string

type t = {
  player : View_state.player_id;
  poll : unit -> (Session.snapshot, error) result;
  submit : int -> View_state.command -> (Session.snapshot, error) result;
  disconnect : unit -> unit;
  closed : bool Atomic.t;
}

let host_id = "host"
let guest_id = "guest"
let timeout = 3.

let name_of name =
  let name = String.trim name in
  if
    name = ""
    || String.length name > 32
    || String.exists (fun c -> c < ' ' || c = '\127') name
  then
    failwith "Enter a player name of 1 to 32 bytes without control characters.";
  name

let connection_error = function
  | End_of_file -> "The other player disconnected."
  | Failure reason -> reason
  | Unix.Unix_error (error, operation, _) ->
      Printf.sprintf "%s: %s" operation (Unix.error_message error)
  | exn -> raise exn

let send conn response = Net.send conn (Protocol.encode_response response)

let serve conn session disconnected () =
  let rec loop () =
    let response =
      match Protocol.decode_request (Net.recv conn) with
      | Ok Poll -> Protocol.State (Session.snapshot session)
      | Ok (Act { revision; command }) -> (
          match Session.submit session ~player:guest_id ~revision command with
          | Ok state -> State state
          | Error reason -> Rejected reason)
      | Ok (Join _) -> Rejected "A guest has already joined on this connection."
      | Error reason -> Rejected reason
    in
    send conn response;
    loop ()
  in
  try loop ()
  with exn ->
    Atomic.set disconnected (Some (connection_error exn));
    Net.shutdown conn

let guest_handshake conn =
  let guest =
    match Protocol.decode_request (Net.recv ~timeout conn) with
    | Ok (Join name) -> (
        try Ok (name_of name) with Failure reason -> Error reason)
    | Ok _ -> Error "Join the game before sending requests."
    | Error reason -> Error reason
  in
  match guest with
  | Ok name -> Some name
  | Error reason ->
      send conn (Rejected reason);
      None

let accept listener ~board ~roll ~name =
  let name = name_of name in
  let rec await_guest () =
    let conn = Net.accept listener in
    let guest =
      try guest_handshake conn with
      | End_of_file | Failure _ | Unix.Unix_error _ -> None
      | exn ->
          Net.close conn;
          raise exn
    in
    match guest with
    | None ->
        Net.close conn;
        await_guest ()
    | Some guest -> (conn, guest)
  in
  let conn, guest = await_guest () in
  try
    let game = Game.create board [ (host_id, name); (guest_id, guest) ] in
    let session = Session.create ~roll game in
    send conn (Joined (guest_id, Session.snapshot session));
    let disconnected = Atomic.make None in
    let worker = Thread.create (serve conn session disconnected) () in
    let if_connected f =
      match Atomic.get disconnected with
      | None -> f ()
      | Some reason -> Error (Disconnected reason)
    in
    {
      player = host_id;
      poll = (fun () -> if_connected (fun () -> Ok (Session.snapshot session)));
      submit =
        (fun revision command ->
          if_connected (fun () ->
              Session.submit session ~player:host_id ~revision command
              |> Result.map_error (fun reason -> Rejected reason)));
      disconnect =
        (fun () ->
          Net.shutdown conn;
          Thread.join worker;
          Net.close conn);
      closed = Atomic.make false;
    }
  with exn ->
    Net.close conn;
    raise exn

let exchange conn request =
  Net.send conn (Protocol.encode_request request);
  match Protocol.decode_response (Net.recv ~timeout conn) with
  | Ok response -> response
  | Error reason -> failwith reason

let join ~address ~port ~name =
  let name = name_of name in
  let conn = Net.connect address port in
  try
    let player =
      match exchange conn (Join name) with
      | Joined (player, _) -> player
      | Rejected reason -> failwith reason
      | State _ -> failwith "Host did not acknowledge the join request."
    in
    let disconnected = ref None in
    let request message =
      match !disconnected with
      | Some reason -> Error (Disconnected reason)
      | None -> (
          try
            match exchange conn message with
            | State state -> Ok state
            | Rejected reason -> Error (Rejected reason)
            | Joined _ -> failwith "Unexpected join response from the host."
          with exn ->
            let reason = connection_error exn in
            disconnected := Some reason;
            Net.shutdown conn;
            Error (Disconnected reason))
    in
    {
      player;
      poll = (fun () -> request Poll);
      submit = (fun revision command -> request (Act { revision; command }));
      disconnect = (fun () -> Net.close conn);
      closed = Atomic.make false;
    }
  with exn ->
    Net.close conn;
    failwith (connection_error exn)

let player t = t.player

let poll t =
  if Atomic.get t.closed then Error (Disconnected "This connection is closed.")
  else t.poll ()

let submit t ~revision = function
  | (View_state.Roll | Buy | Pass | Pay) as command ->
      if Atomic.get t.closed then
        Error (Disconnected "This connection is closed.")
      else t.submit revision command
  | _ -> Error (Rejected "This command is unavailable in LAN play.")

let close t = if not (Atomic.exchange t.closed true) then t.disconnect ()
