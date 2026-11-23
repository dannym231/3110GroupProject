open Monopoly

let poll_interval = 0.3
let next_poll () = Unix.gettimeofday () +. poll_interval

let read_state = function
  | Ok state -> state
  | Error (Lan.Rejected reason | Lan.Disconnected reason) -> failwith reason

let run game =
  let local_player = Lan.player game in
  let state_for_player (snapshot : Session.snapshot) error =
    let state = snapshot.view in
    let menu =
      if state.current = local_player then state.menu
      else
        List.filter
          (fun (item : View_state.menu_item) -> item.command = View_state.Quit)
          state.menu
    in
    let log =
      match error with
      | None -> state.log
      | Some reason -> state.log @ [ reason ]
    in
    { state with menu; log }
  in
  let enter (snapshot : Session.snapshot) =
    View.initial ~local_player snapshot.view
  in
  let rec loop (snapshot : Session.snapshot) ui size error due ~redraw =
    let state = state_for_player snapshot error in
    if redraw then View.show (View.render ~size ~ui state);
    if Unix.gettimeofday () >= due then
      let fresh = read_state (Lan.poll game) in
      if fresh.revision = snapshot.revision then
        loop snapshot ui size error (next_poll ()) ~redraw:false
      else loop fresh (enter fresh) size None (next_poll ()) ~redraw:true
    else
      match View.poll_event ~size with
      | None -> loop snapshot ui size error due ~redraw:false
      | Some (Resize size) -> loop snapshot ui size error due ~redraw:true
      | Some End_of_input -> ()
      | Some (Key key) -> (
          let next_ui, command = View.update ~size state ui key in
          match command with
          | Some View_state.Quit -> ()
          | Some ((Roll | Buy | Pass | Pay) as command) -> (
              match Lan.submit game ~revision:snapshot.revision command with
              | Ok fresh ->
                  loop fresh (enter fresh) size None (next_poll ()) ~redraw:true
              | Error (Lan.Rejected reason) ->
                  loop snapshot next_ui size (Some reason) due ~redraw:true
              | Error (Lan.Disconnected reason) -> failwith reason)
          | Some _ | None ->
              loop snapshot next_ui size error due ~redraw:(ui <> next_ui))
  in
  let snapshot = read_state (Lan.poll game) in
  loop snapshot (enter snapshot) (View.terminal_size ()) None (next_poll ())
    ~redraw:true
