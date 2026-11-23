type snapshot = {
  revision : int;
  view : View_state.t;
}

type t = {
  roll : unit -> int * int;
  mutex : Mutex.t;
  mutable game : Game.t;
  mutable revision : int;
}

let create ~roll game = { roll; game; revision = 0; mutex = Mutex.create () }

let take_snapshot t =
  let view = Play.snapshot t.game in
  (* Keep enough history for the terminal without growing every poll
     response. *)
  let log = view.log |> List.rev |> List.take 40 |> List.rev in
  { revision = t.revision; view = { view with log } }

let snapshot t = Mutex.protect t.mutex (fun () -> take_snapshot t)

let submit t ~player ~revision cmd =
  Mutex.protect t.mutex (fun () ->
      if revision <> t.revision then Error "The board changed. Try again."
      else if player <> (Game.current t.game).id then
        Error "Wait for your turn."
      else
        Play.try_apply ~roll:t.roll cmd t.game
        |> Result.map (fun game ->
            t.game <- game;
            t.revision <- t.revision + 1;
            take_snapshot t))
