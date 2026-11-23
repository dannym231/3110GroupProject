let roller ?seed () =
  let state =
    match seed with
    | Some s -> Random.State.make [| s |]
    | None -> Random.State.make_self_init ()
  in
  fun () -> (1 + Random.State.int state 6, 1 + Random.State.int state 6)