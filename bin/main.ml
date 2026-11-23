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
  try
    View.with_terminal run;
    print_endline "Bye."
  with Sys.Break -> exit 130

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

let roll () = (Random.int 6 + 1, Random.int 6 + 1)

let play game =
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
  Random.self_init ();
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

let start names =
  match Board.load "data/board.json" with
  | Error msg -> die ("monopoly: " ^ msg)
  | Ok board when Board.size board <> 40 ->
      die "monopoly: the terminal board is 40 squares"
  | Ok board -> (
      match
        try Ok (Game.create board (players_of names))
        with Invalid_argument msg -> Error msg
      with
      | Error msg -> die ("monopoly: " ^ msg)
      | Ok game -> play game)

let () =
  let args =
    match Array.to_list Sys.argv with
    | [] -> []
    | _ :: rest -> rest
  in
  match args with
  | [ "--dump" ] -> dump ()
  | [ "--tour" ] -> tour ()
  | _ when List.exists (String.starts_with ~prefix:"--") args ->
      prerr_endline "usage: monopoly [--dump | --tour] [name ...]";
      exit 2
  | [ _ ] ->
      prerr_endline "monopoly: need at least two players";
      exit 2
  | names -> start names
