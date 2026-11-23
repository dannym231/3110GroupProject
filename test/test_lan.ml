open OUnit2
open Monopoly

let board =
  match Board.load "../data/board.json" with
  | Ok board -> board
  | Error reason -> failwith reason

let ok = function
  | Ok value -> value
  | Error (Lan.Rejected reason | Lan.Disconnected reason) ->
      assert_failure reason

let assert_rejected result =
  match result with
  | Error (Lan.Rejected _) -> ()
  | Error (Lan.Disconnected reason) -> assert_failure reason
  | Ok _ -> assert_failure "the command was accepted"

let with_listener f =
  let listener = Net.listen ~addr:"127.0.0.1" 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close listener)
    (fun () -> f listener (Net.bound_port listener))

let with_game ?before_join ~roll test =
  with_listener (fun listener port ->
      let host = ref None in
      let thread =
        Thread.create
          (fun () ->
            host :=
              Some
                (try Ok (Lan.accept listener ~board ~roll ~name:"Alex")
                 with exn -> Error exn))
          ()
      in
      Option.iter (fun prepare -> prepare port) before_join;
      let guest = Lan.join ~address:"127.0.0.1" ~port ~name:"Blake" in
      Fun.protect
        ~finally:(fun () -> Lan.close guest)
        (fun () ->
          Thread.join thread;
          let host =
            match !host with
            | Some (Ok host) -> host
            | Some (Error exn) -> raise exn
            | None -> assert_failure "host did not start"
          in
          Fun.protect
            ~finally:(fun () -> Lan.close host)
            (fun () -> test host guest)))

let submit peer (state : Session.snapshot) command =
  Lan.submit peer ~revision:state.revision command

let player (state : Session.snapshot) peer =
  View_state.player state.view (Lan.player peer) |> Option.get

let owner (state : Session.snapshot) index =
  (View_state.square state.view index |> Option.get).owner

let assert_view_equal (expected : Session.snapshot) peer =
  assert_equal expected (ok (Lan.poll peer))

let full_turns () =
  let rolls = Atomic.make 0 in
  let roll () =
    Atomic.incr rolls;
    (1, 2)
  in
  with_game ~roll (fun host guest ->
      let start = ok (Lan.poll guest) in
      assert_equal 0 start.revision;
      assert_equal (Lan.player host) start.view.current;
      assert_view_equal start host;
      assert_rejected (submit guest start Roll);
      assert_rejected (submit host start Buy);
      assert_equal 0 (Atomic.get rolls);
      assert_view_equal start guest;

      let offered = ok (submit host start Roll) in
      assert_equal start.view.turn offered.view.turn;
      assert_equal 1 offered.revision;
      assert_equal 3 (player offered host).position;
      assert_rejected (submit host start Buy);
      assert_rejected (submit host offered Roll);
      assert_equal 1 (Atomic.get rolls);
      assert_view_equal offered guest;

      let bought = ok (submit host offered Buy) in
      assert_equal (Some (Lan.player host)) (owner bought 3);
      assert_equal 1440 (player bought host).cash;
      assert_equal (Lan.player guest) bought.view.current;
      assert_view_equal bought guest;

      assert_rejected (submit guest bought Buy);
      let payment = ok (submit guest bought Roll) in
      assert_equal
        (View_state.Payment
           {
             kind = Rent;
             payer = Lan.player guest;
             amount = 4;
             payee = Some (Lan.player host);
             square = Some 3;
           })
        payment.view.screen;
      assert_view_equal payment host;
      assert_rejected (submit guest bought Pay);

      let paid = ok (submit guest payment Pay) in
      assert_view_equal paid host;
      assert_equal 1444 (player paid host).cash;
      assert_equal 1496 (player paid guest).cash;
      assert_equal (Lan.player host) paid.view.current;

      let offered = ok (submit host paid Roll) in
      let passed = ok (submit host offered Pass) in
      assert_equal None (owner passed 6);
      assert_equal (Lan.player guest) passed.view.current;
      assert_view_equal passed guest)

let rejects_raw_requests () =
  with_listener (fun listener port ->
      let wire = Net.connect "127.0.0.1" port in
      Fun.protect
        ~finally:(fun () -> Net.close wire)
        (fun () ->
          Net.send wire (Protocol.encode_request (Join "Blake"));
          let host =
            Lan.accept listener ~board ~roll:(fun () -> (1, 2)) ~name:"Alex"
          in
          Fun.protect
            ~finally:(fun () -> Lan.close host)
            (fun () ->
              let receive () =
                Protocol.decode_response (Net.recv ~timeout:2. wire)
              in
              let guest_id, initial =
                match receive () with
                | Ok (Joined (id, state)) -> (id, state)
                | _ -> assert_failure "expected join acknowledgement"
              in
              assert_bool "host and guest have separate IDs"
                (Lan.player host <> guest_id);
              List.iter
                (fun request ->
                  Net.send wire request;
                  match receive () with
                  | Ok (Rejected _) -> assert_view_equal initial host
                  | _ -> assert_failure "expected a rejection")
                [
                  "{";
                  {|{"type":"act","revision":0,"action":"roll","player":"host"}|};
                  {|{"type":"act","revision":0,"action":"roll","dice":[6,6]}|};
                  {|{"type":"act","revision":0,"action":"go_to_jail"}|};
                  {|{"type":"act","revision":0,"action":"roll"}|};
                ];
              Net.send wire (Protocol.encode_request Poll);
              assert_equal (Ok (Protocol.State initial)) (receive ()))))

let guest_observes_disconnect () =
  with_game
    ~roll:(fun () -> (1, 2))
    (fun host guest ->
      Lan.close host;
      match Lan.poll guest with
      | Error (Lan.Disconnected _) -> ()
      | _ -> assert_failure "guest did not observe that the host left")

let host_observes_disconnect () =
  with_game
    ~roll:(fun () -> (1, 2))
    (fun host guest ->
      Lan.close guest;
      let deadline = Unix.gettimeofday () +. 2. in
      let rec await_disconnect () =
        match Lan.poll host with
        | Error (Lan.Disconnected _) -> ()
        | Error (Lan.Rejected reason) -> assert_failure reason
        | Ok _ when Unix.gettimeofday () < deadline ->
            Thread.delay 0.01;
            await_disconnect ()
        | Ok _ -> assert_failure "host did not observe that the guest left"
      in
      await_disconnect ())

let join_without_host () =
  let port = with_listener (fun _ port -> port) in
  match Lan.join ~address:"127.0.0.1" ~port ~name:"Blake" with
  | exception Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> ()
  | guest ->
      Lan.close guest;
      assert_failure "joined a port with no listener"

let join_after_rejected_handshakes () =
  let before_join port =
    List.iter
      (fun (request, reason) ->
        let wire = Net.connect "127.0.0.1" port in
        Fun.protect
          ~finally:(fun () -> Net.close wire)
          (fun () ->
            Net.send wire request;
            assert_equal (Ok (Protocol.Rejected reason))
              (Protocol.decode_response (Net.recv ~timeout:2. wire));
            assert_raises End_of_file (fun () -> Net.recv ~timeout:2. wire)))
      [
        (Protocol.encode_request Poll, "Join the game before sending requests.");
        ( {|{"type":"join","version":0,"name":"Blake"}|},
          "Incompatible LAN protocol version." );
        ( Protocol.encode_request (Join " "),
          "Enter a player name of 1 to 32 bytes without control characters." );
      ]
  in
  with_game ~before_join
    ~roll:(fun () -> (1, 2))
    (fun host guest ->
      let state = ok (Lan.poll guest) in
      assert_equal 0 state.revision;
      assert_equal "Blake" (player state guest).name;
      assert_view_equal state host)

let join_after_silent_peer () =
  let before_join port =
    let wire = Net.connect "127.0.0.1" port in
    Fun.protect
      ~finally:(fun () -> Net.close wire)
      (fun () ->
        (* This peer holds the TCP connection open without sending a join. *)
        assert_raises End_of_file (fun () -> Net.recv ~timeout:6. wire))
  in
  with_game ~before_join
    ~roll:(fun () -> (1, 2))
    (fun host guest ->
      let state = ok (Lan.poll guest) in
      assert_equal 0 state.revision;
      assert_view_equal state host)

let with_tcp_pair test =
  with_listener (fun listener port ->
      let sender = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close sender)
        (fun () ->
          Unix.connect sender (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
          let receiver = Net.accept listener in
          Fun.protect
            ~finally:(fun () -> Net.close receiver)
            (fun () -> test sender receiver)))

let write sender payload =
  let length = String.length payload in
  let rec write_from offset =
    if offset < length then
      let count =
        Unix.write_substring sender payload offset (length - offset)
      in
      if count = 0 then assert_failure "test sender stopped writing"
      else write_from (offset + count)
  in
  write_from 0

let recv_frames () =
  with_tcp_pair (fun sender receiver ->
      let payload = String.make 5000 'a' in
      write sender (payload ^ "\nsecond\n");
      assert_equal payload (Net.recv ~timeout:2. receiver);
      assert_equal "second" (Net.recv ~timeout:2. receiver))

let rejects_partial_eof () =
  with_tcp_pair (fun sender receiver ->
      write sender "complete\ntruncated";
      Unix.shutdown sender Unix.SHUTDOWN_SEND;
      assert_equal "complete" (Net.recv ~timeout:2. receiver);
      assert_raises End_of_file (fun () -> Net.recv ~timeout:2. receiver))

let rejects_incomplete_frame_timeout () =
  with_tcp_pair (fun sender receiver ->
      write sender "no newline yet";
      assert_raises (Failure "Timed out waiting for the other player.")
        (fun () -> Net.recv ~timeout:0.05 receiver))

let rejects_oversized_frame () =
  with_tcp_pair (fun sender receiver ->
      (* One byte above the maximum, without waiting for a newline. Write from
         another thread so the test works with small TCP socket buffers. *)
      let payload = String.make ((256 * 1024) + 1) 'x' in
      let sender_error = ref None in
      let writer =
        Thread.create
          (fun () ->
            try write sender payload with exn -> sender_error := Some exn)
          ()
      in
      Fun.protect
        ~finally:(fun () ->
          Unix.shutdown sender Unix.SHUTDOWN_ALL;
          Thread.join writer)
        (fun () ->
          assert_raises (Failure "LAN message is too large.") (fun () ->
              Net.recv ~timeout:2. receiver));
      Option.iter raise !sender_error)

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  run_test_tt_main
    ("lan"
    >::: [
           ("host and guest play complete turns" >:: fun _ -> full_turns ());
           ( "reject malformed, forged and illegal actions" >:: fun _ ->
             rejects_raw_requests () );
           ( "guest detects a host disconnect" >:: fun _ ->
             guest_observes_disconnect () );
           ( "host detects a guest disconnect" >:: fun _ ->
             host_observes_disconnect () );
           ( "join fails when nobody is listening" >:: fun _ ->
             join_without_host () );
           ( "host rejects bad handshakes and accepts the next guest"
           >:: fun _ -> join_after_rejected_handshakes () );
           ( "host times out a silent first peer and accepts the next guest"
           >:: fun _ -> join_after_silent_peer () );
           ( "read frames across socket buffer boundaries" >:: fun _ ->
             recv_frames () );
           ( "EOF cannot finish a partial frame" >:: fun _ ->
             rejects_partial_eof () );
           ( "incomplete frame respects the read timeout" >:: fun _ ->
             rejects_incomplete_frame_timeout () );
           ( "oversized frame is rejected before a newline" >:: fun _ ->
             rejects_oversized_frame () );
         ])
