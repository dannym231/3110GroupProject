open OUnit2
open Monopoly

let has hay needle =
  let n = String.length needle in
  if n = 0 then true
  else
    let rec go i =
      if i + n > String.length hay then false
      else if String.sub hay i n = needle then true
      else go (i + 1)
    in
    go 0

let assert_has msg hay needle =
  assert_bool (msg ^ ": missing `" ^ needle ^ "`") (has hay needle)

let ok = function
  | Ok x -> x
  | Error msg -> failwith msg

let board_text =
  let square i =
    match i with
    | 0 -> {|{ "name": "Go", "kind": "go" }|}
    | 1 ->
        {|{ "name": "Mediterranean Avenue", "kind": "property", "group": "brown", "price": 60, "rent": 2 }|}
    | 3 ->
        {|{ "name": "Baltic Avenue", "kind": "property", "group": "brown", "price": 60, "rent": 4 }|}
    | 4 -> {|{ "name": "Income Tax", "kind": "tax", "amount": 200 }|}
    | 5 -> {|{ "name": "Reading Railroad", "kind": "railroad", "price": 200 }|}
    | 12 -> {|{ "name": "Electric Company", "kind": "utility", "price": 150 }|}
    | n -> Printf.sprintf {|{ "name": "Square %d", "kind": "free_parking" }|} n
  in
  "{\"squares\":[" ^ String.concat "," (List.init 40 square) ^ "]}"

let board () = ok (Board.of_string board_text)
let game () = Game.create (board ()) [ ("alex", "Alex"); ("blake", "Blake") ]
let roll dice g = Play.apply ~roll:(fun () -> dice) View_state.Roll g

let cash state id =
  match View_state.player state id with
  | Some p -> p.cash
  | None -> failwith id

let owner state index =
  match View_state.square state index with
  | Some sq -> sq.owner
  | None -> failwith "missing square"

let square state index =
  match View_state.square state index with
  | Some sq -> sq
  | None -> failwith "missing square"

let log_has (state : View_state.t) needle =
  List.exists (fun line -> has line needle) state.log

let tests =
  "play"
  >::: [
         ( "real board" >:: fun _ ->
           let path =
             match
               List.find_opt Sys.file_exists
                 [ "data/board.json"; "../data/board.json" ]
             with
             | Some path -> path
             | None -> failwith "data/board.json not found"
           in
           let board = ok (Board.load path) in
           let state =
             Play.snapshot
               (Game.create board [ ("alex", "Alex"); ("blake", "Blake") ])
           in
           assert_equal 40 (List.length state.squares);
           assert_equal "Go" (square state 0).name;
           let baltic = square state 3 in
           assert_equal "Baltic Avenue" baltic.name;
           assert_equal (Some 60) baltic.price;
           assert_equal (Some 4) baltic.rent;
           assert_equal View_state.Brown baltic.group;
           assert_equal "Boardwalk" (square state 39).name );
         ( "base rents" >:: fun _ ->
           let state = Play.snapshot (game ()) in
           let reading = square state 5 in
           assert_equal "Reading Railroad" reading.name;
           assert_equal (Some 200) reading.price;
           assert_equal (Some 25) reading.rent;
           assert_equal View_state.Railroad reading.group;
           assert_equal None (square state 12).rent;
           assert_equal View_state.Utility (square state 12).group );
         ( "buy or pass baltic" >:: fun _ ->
           let state = Play.snapshot (roll (1, 2) (game ())) in
           assert_equal
             (View_state.Buy_or_pass { square = 3; price = 60; rent = 4 })
             state.screen;
           assert_equal (Some { View_state.die1 = 1; die2 = 2 }) state.last_dice;
           assert_equal (Some View_state.Buy) (View.command_of_key state 'b');
           assert_equal (Some View_state.Pass) (View.command_of_key state 'p');
           let text = View.plain (View.render state) in
           assert_has "name" text "Baltic";
           assert_has "price" text "$60" );
         ( "buy" >:: fun _ ->
           let landed = roll (1, 2) (game ()) in
           let state =
             Play.snapshot
               (Play.apply ~roll:(fun () -> (0, 0)) View_state.Buy landed)
           in
           assert_equal View_state.Overview state.screen;
           assert_equal "blake" state.current;
           assert_equal 2 state.turn;
           assert_equal (Some "alex") (owner state 3);
           assert_equal 1440 (cash state "alex") );
         ( "pass" >:: fun _ ->
           let landed = roll (1, 2) (game ()) in
           let state =
             Play.snapshot
               (Play.apply ~roll:(fun () -> (0, 0)) View_state.Pass landed)
           in
           assert_equal View_state.Overview state.screen;
           assert_equal "blake" state.current;
           assert_equal None (owner state 3);
           assert_equal 1500 (cash state "alex") );
         ( "rent" >:: fun _ ->
           let bought =
             Play.apply
               ~roll:(fun () -> (0, 0))
               View_state.Buy
               (roll (1, 2) (game ()))
           in
           let owing = roll (1, 2) bought in
           let state = Play.snapshot owing in
           assert_equal
             (View_state.Payment
                {
                  kind = Rent;
                  payer = "blake";
                  amount = 4;
                  payee = Some "alex";
                  square = Some 3;
                })
             state.screen;
           let paid =
             Play.snapshot
               (Play.apply ~roll:(fun () -> (0, 0)) View_state.Pay owing)
           in
           assert_equal View_state.Overview paid.screen;
           assert_equal "alex" paid.current;
           assert_equal (Some View_state.Roll) (View.command_of_key paid 'r');
           assert_equal 1444 (cash paid "alex");
           assert_equal 1496 (cash paid "blake") );
         ( "utility offer" >:: fun _ ->
           let state = Play.snapshot (roll (6, 6) (game ())) in
           assert_equal
             (View_state.Buy_or_pass { square = 12; price = 150; rent = 48 })
             state.screen;
           assert_equal None (square state 12).rent );
         ( "tax skips payment" >:: fun _ ->
           let state = Play.snapshot (roll (1, 3) (game ())) in
           assert_equal View_state.Overview state.screen;
           assert_equal "blake" state.current;
           assert_bool "tax log" (log_has state "Taxes come in MS3.") );
         ( "roll during buy" >:: fun _ ->
           let landed = roll (1, 2) (game ()) in
           let calls = ref 0 in
           let state =
             Play.snapshot
               (Play.apply
                  ~roll:(fun () ->
                    incr calls;
                    (1, 1))
                  View_state.Roll landed)
           in
           assert_equal 0 !calls;
           assert_equal
             (View_state.Buy_or_pass { square = 3; price = 60; rent = 4 })
             state.screen;
           assert_equal None (owner state 3);
           assert_equal 1500 (cash state "alex");
           assert_bool "roll error" (log_has state "You can't roll right now.")
         );
         ( "seeded roll reaches the view" >:: fun _ ->
           let d1, d2 = Dice.roller ~seed:42 () () in
           let roll = Dice.roller ~seed:42 () in
           let state =
             Play.snapshot (Play.apply ~roll View_state.Roll (game ()))
           in
           assert_equal
             (Some { View_state.die1 = d1; die2 = d2 })
             state.last_dice;
           assert_has "last roll"
             (View.plain (View.render state))
             (Printf.sprintf "Last roll %d + %d = %d" d1 d2 (d1 + d2)) );
       ]

let _ = run_test_tt_main tests
