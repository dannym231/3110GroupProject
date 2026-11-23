open OUnit2
open Monopoly

(* A small 6-square board written inline, so the tests don't depend on
   reading data/board.json from disk:
   0 Go | 1 Brown A ($60, rent 4) | 2 Brown B ($60, rent 2)
   3 Chance | 4 Mansion ($2000, rent 100) | 5 Free Parking *)
let board =
  match
    Board.of_string
      {|{"squares":[
        {"name":"Go","kind":"go"},
        {"name":"Brown A","kind":"property","group":"brown","price":60,"rent":4},
        {"name":"Brown B","kind":"property","group":"brown","price":60,"rent":2},
        {"name":"Chance","kind":"chance"},
        {"name":"Mansion","kind":"property","group":"gold","price":2000,"rent":100},
        {"name":"Free Parking","kind":"free_parking"}
      ]}|}
  with
  | Ok b -> b
  | Error e -> failwith e

let new_game () = Game.create board [ ("a", "Alex"); ("b", "Blake") ]

(* Unwrap a result, failing the test with the error message if it's Error. *)
let ok = function
  | Ok g -> g
  | Error e -> assert_failure ("unexpected error: " ^ e)

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let find g id = List.find (fun (p : Game.player) -> p.id = id) (Game.players g)
let cash g id = (find g id).cash
let position g id = (find g id).position
let pp_int = string_of_int

let tests =
  "game tests"
  >::: [
         ( "players start on Go with $1500" >:: fun _ ->
           let g = new_game () in
           assert_equal 0 (position g "a") ~printer:pp_int;
           assert_equal 1500 (cash g "a") ~printer:pp_int;
           assert_equal 1500 (cash g "b") ~printer:pp_int );
         ( "first player is Alex, waiting to roll" >:: fun _ ->
           let g = new_game () in
           assert_equal "a" (Game.current g).id;
           assert_equal Game.Awaiting_roll (Game.phase g) );
         ( "need at least two players" >:: fun _ ->
           assert_raises (Invalid_argument "Game.create: need two players")
             (fun () -> Game.create board [ ("a", "Alex") ]) );
         ( "roll 1+2 moves to square 3" >:: fun _ ->
           let g = ok (Game.roll (1, 2) (new_game ())) in
           assert_equal 3 (position g "a") ~printer:pp_int );
         ( "landing on an unowned property offers it" >:: fun _ ->
           (* 6 squares, so 3+4 = 7 wraps to square 1 *)
           let g = ok (Game.roll (3, 4) (new_game ())) in
           assert_equal (Game.Awaiting_buy 1) (Game.phase g) );
         ( "passing Go pays $200" >:: fun _ ->
           let g = ok (Game.roll (3, 4) (new_game ())) in
           assert_equal 1 (position g "a") ~printer:pp_int;
           assert_equal 1700 (cash g "a") ~printer:pp_int );
         ( "buying costs the price and records the owner" >:: fun _ ->
           let g = ok (Game.roll (3, 4) (new_game ())) in
           let g = ok (Game.buy g) in
           assert_equal 1640 (cash g "a") ~printer:pp_int;
           assert_equal (Some "a") (Game.owner g 1) );
         ( "buying ends the turn" >:: fun _ ->
           let g = ok (Game.buy (ok (Game.roll (3, 4) (new_game ())))) in
           assert_equal "b" (Game.current g).id;
           assert_equal Game.Awaiting_roll (Game.phase g) );
         ( "passing leaves the square unowned" >:: fun _ ->
           let g = ok (Game.pass (ok (Game.roll (3, 4) (new_game ())))) in
           assert_equal None (Game.owner g 1);
           assert_equal "b" (Game.current g).id );
         ( "landing on someone else's property charges rent" >:: fun _ ->
           (* Alex buys square 1; Blake rolls 3+4 and lands on it *)
           let g = ok (Game.buy (ok (Game.roll (3, 4) (new_game ())))) in
           let g = ok (Game.roll (3, 4) g) in
           assert_equal
             (Game.Awaiting_rent { square = 1; owner = "a"; amount = 4 })
             (Game.phase g) );
         ( "paying rent moves the money" >:: fun _ ->
           let g = ok (Game.buy (ok (Game.roll (3, 4) (new_game ())))) in
           let g = ok (Game.pay (ok (Game.roll (3, 4) g))) in
           (* Blake: 1500 + 200 (Go) - 4 rent; Alex: 1640 + 4 *)
           assert_equal 1696 (cash g "b") ~printer:pp_int;
           assert_equal 1644 (cash g "a") ~printer:pp_int );
         ( "can't buy what you can't afford" >:: fun _ ->
           (* 2+2 lands on the $2000 Mansion *)
           let g = ok (Game.roll (2, 2) (new_game ())) in
           assert_equal (Game.Awaiting_buy 4) (Game.phase g);
           assert_bool "buy should fail" (is_error (Game.buy g)) );
         ( "can't buy, pass, or pay before rolling" >:: fun _ ->
           let g = new_game () in
           assert_bool "buy" (is_error (Game.buy g));
           assert_bool "pass" (is_error (Game.pass g));
           assert_bool "pay" (is_error (Game.pay g)) );
         ( "can't roll twice in a row" >:: fun _ ->
           let g = ok (Game.roll (3, 4) (new_game ())) in
           assert_bool "second roll" (is_error (Game.roll (1, 1) g)) );
         ( "dice must be 1 to 6" >:: fun _ ->
           let g = new_game () in
           assert_bool "0" (is_error (Game.roll (0, 3) g));
           assert_bool "7" (is_error (Game.roll (7, 1) g)) );
         ( "non-property squares just end the turn" >:: fun _ ->
           (* 1+2 lands on Chance (cards come in MS3) *)
           let g = ok (Game.roll (1, 2) (new_game ())) in
           assert_equal "b" (Game.current g).id );
         ( "turns go Alex, Blake, Alex" >:: fun _ ->
           let g0 = new_game () in
           let g1 = ok (Game.roll (1, 2) g0) in
           let g2 = ok (Game.roll (1, 2) g1) in
           assert_equal [ "a"; "b"; "a" ]
             (List.map (fun g -> (Game.current g).id) [ g0; g1; g2 ]) );
         ( "same seed gives the same rolls" >:: fun _ ->
           let r1 = Dice.roller ~seed:42 () and r2 = Dice.roller ~seed:42 () in
           let five r = List.init 5 (fun _ -> r ()) in
           assert_equal (five r1) (five r2) );
         ( "dice are always 1 to 6" >:: fun _ ->
           let r = Dice.roller ~seed:7 () in
           for _ = 1 to 1000 do
             let a, b = r () in
             assert_bool "in range" (a >= 1 && a <= 6 && b >= 1 && b <= 6)
           done );
       ]

let _ = run_test_tt_main tests