open Monopoly

let dump () =
  List.iter
    (fun id ->
      let tour = View_dummy.jump id in
      let state = View_dummy.snapshot tour in
      Printf.printf "===== %s =====\n%s\n\n" id
        (View.plain
           (View.render ~demo:true ~preview:(View_dummy.is_preview tour) state)))
    (View_dummy.ids ())

let with_play run =
  View.with_terminal run;
  print_endline "Bye."

let require_tty () =
  if View.isatty () then ()
  else (
    prerr_endline "monopoly: not a terminal; use --dump";
    exit 1)

let tour () =
  let rec loop tour_state ui size ~redraw =
    let state = View_dummy.snapshot tour_state in
    if redraw then
      View.show
        (View.render ~size ~ui ~demo:true
           ~preview:(View_dummy.is_preview tour_state)
           state);
    match View.read_event ~size with
    | View.Resize size -> loop tour_state ui size ~redraw:true
    | End_of_input -> ()
    | Key key -> (
        let next_ui, command = View.update ~size state ui key in
        match command with
        | Some View_state.Quit -> ()
        | _ ->
            let next =
              Option.fold ~none:tour_state
                ~some:(View_dummy.apply tour_state)
                command
            in
            if next <> tour_state then
              loop next
                (View.initial (View_dummy.snapshot next))
                size ~redraw:true
            else loop tour_state next_ui size ~redraw:(next_ui <> ui))
  in
  let opening = View_dummy.jump "opening" in
  require_tty ();
  with_play (fun () ->
      loop opening
        (View.initial (View_dummy.snapshot opening))
        (View.terminal_size ()) ~redraw:true)

let play ?seed game =
  let roll = Dice.roller ?seed () in
  let rec loop game ui size ~redraw =
    let state = Play.snapshot game in
    if redraw then View.show (View.render ~size ~ui state);
    match View.read_event ~size with
    | View.Resize size -> loop game ui size ~redraw:true
    | End_of_input -> ()
    | Key key -> (
        let next_ui, command = View.update ~size state ui key in
        match command with
        | Some View_state.Quit -> ()
        | Some (View_state.Next | View_state.Prev) ->
            loop game next_ui size ~redraw:false
        | Some cmd ->
            let game = Play.apply ~roll cmd game in
            loop game (View.initial (Play.snapshot game)) size ~redraw:true
        | None -> loop game next_ui size ~redraw:(next_ui <> ui))
  in
  require_tty ();
  with_play (fun () ->
      let state = Play.snapshot game in
      loop game (View.initial state) (View.terminal_size ()) ~redraw:true)

let die msg =
  prerr_endline msg;
  exit 1

let players_of = function
  | [] -> [ ("alex", "Alex"); ("blake", "Blake") ]
  | names -> List.map (fun name -> (name, name)) names

let load_board () =
  match Board.load "data/board.json" with
  | Error msg -> die ("monopoly: " ^ msg)
  | Ok board when Board.size board <> 40 ->
      die "monopoly: the terminal board is 40 squares"
  | Ok board -> board

let start ?seed names =
  let board = load_board () in
  let game =
    try Game.create board (players_of names)
    with Invalid_argument msg -> die ("monopoly: " ^ msg)
  in
  play ?seed game

let usage () =
  prerr_endline
    "usage: monopoly [--dump | --tour | --seed N] [name ...]\n\
    \       monopoly --host NAME [--port PORT] [--seed N]\n\
    \       monopoly --join HOST NAME [--port PORT]";
  exit 2

let run_lan open_game =
  require_tty ();
  let previous_int =
    Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break))
  in
  let previous_pipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  try
    Fun.protect
      ~finally:(fun () ->
        Sys.set_signal Sys.sigint previous_int;
        Sys.set_signal Sys.sigpipe previous_pipe)
      (fun () ->
        let game = open_game () in
        Fun.protect
          ~finally:(fun () -> Lan.close game)
          (fun () -> with_play (fun () -> Lan_play.run game)))
  with
  | Failure msg -> die ("monopoly: " ^ msg)
  | Unix.Unix_error (error, operation, _) ->
      die
        (Printf.sprintf "monopoly: %s: %s" operation (Unix.error_message error))

let host ~port ?seed name =
  let board = load_board () in
  let roll = Dice.roller ?seed () in
  run_lan (fun () ->
      let listener = Net.listen ~addr:"0.0.0.0" port in
      Fun.protect
        ~finally:(fun () -> Unix.close listener)
        (fun () ->
          Printf.printf "Waiting on port %d for one player to join.\n%!" port;
          Lan.accept listener ~board ~roll ~name))

let rec lan_options ~allow_seed port seed = function
  | [] -> (Option.value port ~default:Net.default_port, seed)
  | "--port" :: value :: rest when Option.is_none port -> (
      match int_of_string_opt value with
      | Some p when p > 0 && p <= 65535 ->
          lan_options ~allow_seed (Some p) seed rest
      | _ -> usage ())
  | "--seed" :: value :: rest when allow_seed && Option.is_none seed -> (
      match int_of_string_opt value with
      | Some n -> lan_options ~allow_seed port (Some n) rest
      | None -> usage ())
  | _ -> usage ()

let rec split_seed seed names = function
  | [] -> Some (seed, List.rev names)
  | "--seed" :: n :: rest -> (
      match (seed, int_of_string_opt n) with
      | None, Some n -> split_seed (Some n) names rest
      | _ -> None)
  | "--seed" :: [] -> None
  | arg :: _ when String.starts_with ~prefix:"--" arg -> None
  | name :: rest -> split_seed seed (name :: names) rest

let run = function
  | [ "--dump" ] -> dump ()
  | [ "--tour" ] -> tour ()
  | "--host" :: name :: options ->
      let port, seed = lan_options ~allow_seed:true None None options in
      host ~port ?seed name
  | "--join" :: address :: name :: options ->
      let port, _ = lan_options ~allow_seed:false None None options in
      run_lan (fun () -> Lan.join ~address ~port ~name)
  | args -> (
      match split_seed None [] args with
      | None -> usage ()
      | Some (_, [ _ ]) ->
          prerr_endline "monopoly: need at least two players";
          exit 2
      | Some (seed, names) -> start ?seed names)

let () = try run (List.tl (Array.to_list Sys.argv)) with Sys.Break -> exit 130
