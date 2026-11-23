(* Tests for Board: loading, validation, lookups, and rent rules.

   Expected values come from data/board.json and the standard Monopoly rules,
   worked out by hand rather than by calling the implementation:
   - streets charge base rent, doubled when one owner holds the whole group;
   - railroads charge 25 / 50 / 100 / 200 for 1 / 2 / 3 / 4 owned;
   - utilities charge 4x the dice, or 10x when one owner holds both.

   Board indices used below (from data/board.json):
     0 Go, 1 Mediterranean (brown, rent 2), 3 Baltic (brown, rent 4),
     4 Income Tax (200), 5/15/25/35 railroads, 6/8/9 light blue (rent 6/6/8),
     7 Chance, 10 Jail, 12 Electric Company, 20 Free Parking, 28 Water Works,
     30 Go to Jail, 37 Park Place (dark blue, rent 35),
     38 Luxury Tax (100), 39 Boardwalk (dark blue, price 400, rent 50). *)

     open OUnit2
     open Monopoly
     
     let board_path = "../data/board.json"
     
     let real_board =
       match Board.load board_path with
       | Ok b -> b
       | Error e -> failwith ("could not load " ^ board_path ^ ": " ^ e)
     
     (* ---------- helpers ---------- *)
     
     let pp_int_list l = "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"
     
     let pp_int_opt = function
       | None -> "None"
       | Some n -> "Some " ^ string_of_int n
     
     (** [owners pairs] is an [owner_of] function where square [i] belongs to the
         player paired with [i] in [pairs], and every other square is unowned. *)
     let owners pairs i = List.assoc_opt i pairs
     
     (** [all_to p squares] gives every square in [squares] to player [p]. *)
     let all_to p squares = List.map (fun i -> (i, p)) squares
     
     let rent_test name ~expected ?(dice = 7) pairs i =
       name >:: fun _ ->
       assert_equal expected
         (Board.rent real_board ~owner_of:(owners pairs) ~dice_total:dice i)
         ~printer:string_of_int
     
     let accepts name json =
       name >:: fun _ ->
       match Board.of_string json with
       | Ok _ -> ()
       | Error e -> assert_failure ("expected Ok, got Error: " ^ e)
     
     let rejects name json =
       name >:: fun _ ->
       match Board.of_string json with
       | Ok _ -> assert_failure "expected Error, got Ok"
       | Error _ -> ()
     
     (** Wrap a single extra square after Go so each malformed case only has to
         describe the square under test. *)
     let with_square sq =
       {|{ "squares": [ { "name": "Go", "kind": "go" }, |} ^ sq ^ " ] }"
     
     let count_kind pred =
       let n = ref 0 in
       for i = 0 to Board.size real_board - 1 do
         if pred (Board.square real_board i).Board.kind then incr n
       done;
       !n
     
     (* ---------- loading the real board ---------- *)
     
     let real_board_tests =
       [
         ( "real board has 40 squares" >:: fun _ ->
           assert_equal 40 (Board.size real_board) ~printer:string_of_int );
         ( "square 0 is Go" >:: fun _ ->
           let sq = Board.square real_board 0 in
           assert_equal "Go" sq.name ~printer:Fun.id;
           assert_bool "kind should be Go" (sq.kind = Board.Go) );
         ( "square 39 is Boardwalk" >:: fun _ ->
           assert_equal "Boardwalk" (Board.square real_board 39).name
             ~printer:Fun.id );
         ( "corners are Jail, Free Parking, Go to Jail" >:: fun _ ->
           assert_bool "10 is Jail" ((Board.square real_board 10).kind = Board.Jail);
           assert_bool "20 is Free Parking"
             ((Board.square real_board 20).kind = Board.Free_parking);
           assert_bool "30 is Go to Jail"
             ((Board.square real_board 30).kind = Board.Go_to_jail) );
         ( "tax amounts are 200 and 100" >:: fun _ ->
           assert_bool "Income Tax is 200"
             ((Board.square real_board 4).kind = Board.Tax { amount = 200 });
           assert_bool "Luxury Tax is 100"
             ((Board.square real_board 38).kind = Board.Tax { amount = 100 }) );
         ( "22 streets, 4 railroads, 2 utilities" >:: fun _ ->
           let is_prop = function Board.Property _ -> true | _ -> false in
           let is_rr = function Board.Railroad _ -> true | _ -> false in
           let is_util = function Board.Utility _ -> true | _ -> false in
           assert_equal 22 (count_kind is_prop) ~printer:string_of_int;
           assert_equal 4 (count_kind is_rr) ~printer:string_of_int;
           assert_equal 2 (count_kind is_util) ~printer:string_of_int );
       ]
     
     (* ---------- price ---------- *)
     
     let price_of i = Board.price (Board.square real_board i)
     
     let price_tests =
       [
         ( "Boardwalk costs 400" >:: fun _ ->
           assert_equal (Some 400) (price_of 39) ~printer:pp_int_opt );
         ( "Mediterranean costs 60" >:: fun _ ->
           assert_equal (Some 60) (price_of 1) ~printer:pp_int_opt );
         ( "Reading Railroad costs 200" >:: fun _ ->
           assert_equal (Some 200) (price_of 5) ~printer:pp_int_opt );
         ( "Electric Company costs 150" >:: fun _ ->
           assert_equal (Some 150) (price_of 12) ~printer:pp_int_opt );
         ( "Go, Chance, tax, and Jail cannot be bought" >:: fun _ ->
           List.iter
             (fun i -> assert_equal None (price_of i) ~printer:pp_int_opt)
             [ 0; 7; 4; 10; 20; 30 ] );
       ]
     
     (* ---------- group_members ---------- *)
     
     let group_tests =
       let check name group expected =
         name >:: fun _ ->
         assert_equal expected
           (Board.group_members real_board group)
           ~printer:pp_int_list
       in
       [
         check "brown is [1; 3]" "brown" [ 1; 3 ];
         check "light blue is [6; 8; 9]" "light_blue" [ 6; 8; 9 ];
         check "green is [31; 32; 34]" "green" [ 31; 32; 34 ];
         check "dark blue is [37; 39]" "dark_blue" [ 37; 39 ];
         check "unknown group is empty" "purple" [];
       ]
     
     (* ---------- of_string: valid and malformed input ---------- *)
     
     let parse_tests =
       [
         accepts "a board with only Go is valid"
           {|{ "squares": [ { "name": "Go", "kind": "go" } ] }|};
         accepts "a valid property square"
           (with_square
              {|{ "name": "A", "kind": "property", "group": "g", "price": 10, "rent": 1 }|});
         accepts "zero price and rent are allowed"
           (with_square
              {|{ "name": "A", "kind": "property", "group": "g", "price": 0, "rent": 0 }|});
         ( "a board with only Go has size 1" >:: fun _ ->
           match Board.of_string {|{ "squares": [ { "name": "Go", "kind": "go" } ] }|} with
           | Ok b -> assert_equal 1 (Board.size b) ~printer:string_of_int
           | Error e -> assert_failure e );
         rejects "text that is not JSON" "this is not json";
         rejects "truncated JSON" {|{ "squares": [ |};
         rejects "top level is a list, not an object" {|[ { "name": "Go", "kind": "go" } ]|};
         rejects "missing squares field" "{}";
         rejects "squares is not a list" {|{ "squares": 5 }|};
         rejects "empty squares list" {|{ "squares": [] }|};
         rejects "square 0 is not Go"
           {|{ "squares": [ { "name": "Jail", "kind": "jail" } ] }|};
         rejects "a square is not an object" (with_square "42");
         rejects "unknown kind" (with_square {|{ "name": "X", "kind": "hotel" }|});
         rejects "missing name" (with_square {|{ "kind": "jail" }|});
         rejects "name is not a string" (with_square {|{ "name": 7, "kind": "jail" }|});
         rejects "missing kind" (with_square {|{ "name": "X" }|});
         rejects "property missing rent"
           (with_square {|{ "name": "A", "kind": "property", "group": "g", "price": 10 }|});
         rejects "property missing group"
           (with_square {|{ "name": "A", "kind": "property", "price": 10, "rent": 1 }|});
         rejects "railroad missing price"
           (with_square {|{ "name": "R", "kind": "railroad" }|});
         rejects "tax missing amount" (with_square {|{ "name": "T", "kind": "tax" }|});
         rejects "negative price"
           (with_square
              {|{ "name": "A", "kind": "property", "group": "g", "price": -10, "rent": 1 }|});
         rejects "negative rent"
           (with_square
              {|{ "name": "A", "kind": "property", "group": "g", "price": 10, "rent": -1 }|});
         rejects "negative tax" (with_square {|{ "name": "T", "kind": "tax", "amount": -5 }|});
         rejects "price given as a string"
           (with_square
              {|{ "name": "A", "kind": "property", "group": "g", "price": "60", "rent": 1 }|});
         rejects "price given as a float"
           (with_square
              {|{ "name": "A", "kind": "property", "group": "g", "price": 1.5, "rent": 1 }|});
         ( "unknown-kind error names the bad kind" >:: fun _ ->
           match Board.of_string (with_square {|{ "name": "X", "kind": "hotel" }|}) with
           | Ok _ -> assert_failure "expected Error"
           | Error msg ->
               let contains s sub =
                 let n = String.length sub in
                 let rec go i =
                   i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
                 in
                 go 0
               in
               assert_bool ("message should mention \"hotel\": " ^ msg)
                 (contains msg "hotel") );
       ]
     
     (* ---------- load: files on disk ---------- *)
     
     let load_tests =
       [
         ( "missing file is an Error, not an exception" >:: fun _ ->
           match Board.load "no/such/board.json" with
           | Ok _ -> assert_failure "expected Error"
           | Error _ -> () );
         ( "malformed file on disk is an Error" >:: fun _ ->
           let path = Filename.temp_file "bad_board" ".json" in
           Out_channel.with_open_bin path (fun oc ->
               Out_channel.output_string oc {|{ "squares": [] }|});
           let result = Board.load path in
           Sys.remove path;
           match result with
           | Ok _ -> assert_failure "expected Error"
           | Error _ -> () );
       ]
     
     (* ---------- rent: streets ---------- *)
     
     let street_rent_tests =
       [
         rent_test "unowned street costs nothing" ~expected:0 [] 1;
         rent_test "Mediterranean alone: base rent 2" ~expected:2 [ (1, "A") ] 1;
         rent_test "Baltic alone: base rent 4" ~expected:4 [ (3, "A") ] 3;
         rent_test "both browns: Mediterranean doubles to 4" ~expected:4
           (all_to "A" [ 1; 3 ]) 1;
         rent_test "both browns: Baltic doubles to 8" ~expected:8
           (all_to "A" [ 1; 3 ]) 3;
         rent_test "browns split between players: no doubling" ~expected:2
           [ (1, "A"); (3, "B") ] 1;
         rent_test "two of three light blues: no doubling" ~expected:6
           (all_to "A" [ 6; 8 ]) 6;
         rent_test "all three light blues: Connecticut doubles to 16" ~expected:16
           (all_to "A" [ 6; 8; 9 ]) 9;
         rent_test "Park Place alone: 35" ~expected:35 [ (37, "A") ] 37;
         rent_test "both dark blues: Boardwalk doubles to 100" ~expected:100
           (all_to "A" [ 37; 39 ]) 39;
         rent_test "a full set elsewhere does not double this street" ~expected:6
           (all_to "A" [ 1; 3; 6 ]) 6;
         rent_test "dice total does not affect street rent" ~expected:2 ~dice:12
           [ (1, "A") ] 1;
       ]
     
     (* ---------- rent: railroads ---------- *)
     
     let railroads = [ 5; 15; 25; 35 ]
     
     let railroad_rent_tests =
       [
         rent_test "unowned railroad costs nothing" ~expected:0 [] 5;
         rent_test "1 railroad: 25" ~expected:25 [ (5, "A") ] 5;
         rent_test "2 railroads: 50" ~expected:50 (all_to "A" [ 5; 15 ]) 5;
         rent_test "3 railroads: 100" ~expected:100 (all_to "A" [ 5; 15; 25 ]) 25;
         rent_test "4 railroads: 200" ~expected:200 (all_to "A" railroads) 35;
         rent_test "railroads split 2-2: each owner charges 50" ~expected:50
           [ (5, "A"); (15, "A"); (25, "B"); (35, "B") ]
           25;
         rent_test "dice total does not affect railroad rent" ~expected:25 ~dice:2
           [ (5, "A") ] 5;
       ]
     
     (* ---------- rent: utilities ---------- *)
     
     let utility_rent_tests =
       [
         rent_test "unowned utility costs nothing" ~expected:0 [] 12;
         rent_test "one utility, dice 7: 4 x 7 = 28" ~expected:28 [ (12, "A") ] 12;
         rent_test "one utility, dice 12: 4 x 12 = 48" ~expected:48 ~dice:12
           [ (28, "A") ] 28;
         rent_test "both utilities, dice 7: 10 x 7 = 70" ~expected:70
           (all_to "A" [ 12; 28 ]) 12;
         rent_test "utilities split between players: 4 x dice" ~expected:28
           [ (12, "A"); (28, "B") ] 28;
       ]
     
     (* ---------- rent: squares that cannot be owned ---------- *)
     
     let non_buyable_rent_tests =
       [
         rent_test "Go costs nothing even if owner_of claims it" ~expected:0
           [ (0, "A") ] 0;
         rent_test "a tax square costs nothing through rent" ~expected:0
           [ (4, "A") ] 4;
         rent_test "Chance costs nothing through rent" ~expected:0 [ (7, "A") ] 7;
       ]
     
     let suite =
       "Board tests"
       >::: List.concat
              [
                real_board_tests;
                price_tests;
                group_tests;
                parse_tests;
                load_tests;
                street_rent_tests;
                railroad_rent_tests;
                utility_rent_tests;
                non_buyable_rent_tests;
              ]
     
     let () = run_test_tt_main suite