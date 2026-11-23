type direction = View_input.direction =
  | Up
  | Down
  | Left
  | Right

type key = View_input.key =
  | Arrow of direction
  | Enter
  | Escape
  | Tab
  | Letter of char

type event =
  | Key of key
  | Resize of (int * int)
  | End_of_input

type ui = View_input.t

let initial = View_input.initial
let minimum_size = View_frame.minimum_size
let plain = View_style.text
let show = View_style.show
let command_of_key = View_input.command_of_key

let render ?(size = minimum_size) ?ui ?(preview = false) ?(demo = false) state =
  let ui =
    match ui with
    | Some ui -> ui
    | None -> initial state
  in
  View_frame.render ~size ~preview ~demo ui state

let update ~size state ui key =
  if View_frame.fits size || View_input.is_quit key then
    View_input.update state ui
      ~scroll_limit:(fun () -> View_frame.scroll_limit ~size state ui)
      key
  else (ui, None)

let isatty () = Unix.isatty Unix.stdin && Unix.isatty Unix.stdout
let terminal_size = ANSITerminal.size

let with_terminal run =
  let fd = Unix.stdin in
  let saved = Unix.tcgetattr fd in
  let control text =
    output_string stdout text;
    flush stdout
  in
  let previous =
    Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break))
  in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.tcsetattr fd Unix.TCSANOW saved with Unix.Unix_error _ -> ());
      (try control "\027[?25h\027[?1049l" with Sys_error _ -> ());
      Sys.set_signal Sys.sigint previous)
    (fun () ->
      Unix.tcsetattr fd Unix.TCSANOW
        {
          saved with
          Unix.c_icanon = false;
          c_echo = false;
          c_isig = true;
          c_vmin = 1;
          c_vtime = 0;
        };
      control "\027[?1049h\027[?25l";
      run ())

let read_byte =
  let buffer = Bytes.create 1 in
  fun timeout ->
    let ready, _, _ = Unix.select [ Unix.stdin ] [] [] timeout in
    if ready = [] then None
    else if Unix.read Unix.stdin buffer 0 1 = 0 then raise End_of_file
    else Some (Bytes.get buffer 0)

let poll_event ~size =
  try
    let current = terminal_size () in
    if current <> size then Some (Resize current)
    else
      match read_byte 0.1 with
      | Some '\004' -> Some End_of_input
      | None -> None
      | Some c ->
          View_input.decode ~read_more:(fun () -> read_byte 0.04) c
          |> Option.map (fun key ->
              (* The terminal can shrink while select is waiting for a key. *)
              let current = terminal_size () in
              if current <> size && not (View_input.is_quit key) then
                Resize current
              else Key key)
  with
  | End_of_file | Unix.Unix_error (Unix.EIO, _, _) -> Some End_of_input
  | Unix.Unix_error (Unix.EINTR, _, _) -> None

let rec read_event ~size =
  match poll_event ~size with
  | Some event -> event
  | None -> read_event ~size
