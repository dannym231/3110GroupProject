open Monopoly

let dump () =
  List.iter
    (fun id ->
      let tour = View_dummy.jump id in
      let state = View_dummy.snapshot tour in
      Printf.printf "===== %s =====\n%s\n\n" id
        (View.plain (View.render ~preview:(View_dummy.is_preview tour) state)))
    (View_dummy.ids ())

let play () =
  let rec loop tour ui size ~redraw =
    let state = View_dummy.snapshot tour in
    if redraw then
      View.show
        (View.render ~size ~ui ~preview:(View_dummy.is_preview tour) state);
    match View.read_event ~size with
    | View.Resize size -> loop tour ui size ~redraw:true
    | End_of_input -> ()
    | Key key -> (
        let next_ui, command = View.update ~size state ui key in
        match command with
        | Some View_state.Quit -> ()
        | _ ->
            let next =
              Option.fold ~none:tour ~some:(View_dummy.apply tour) command
            in
            if next <> tour then
              loop next
                (View.initial (View_dummy.snapshot next))
                size ~redraw:true
            else loop tour next_ui size ~redraw:(next_ui <> ui))
  in
  let tour = View_dummy.jump "opening" in
  try
    View.with_terminal (fun () ->
        loop tour
          (View.initial (View_dummy.snapshot tour))
          (View.terminal_size ()) ~redraw:true);
    print_endline "Bye."
  with Sys.Break -> exit 130

let () =
  match Array.to_list Sys.argv with
  | [ _; "--dump" ] -> dump ()
  | [ _ ] ->
      if View.isatty () then play ()
      else (
        prerr_endline "monopoly: not a terminal; use --dump";
        exit 1)
  | _ ->
      prerr_endline "usage: monopoly [--dump]";
      exit 2
