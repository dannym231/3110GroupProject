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

let state id = View_dummy.snapshot (View_dummy.jump id)
let plain id = View.plain (View.render (state id))
let board id = View.plain (View_board.render (state id))
let lines text = String.split_on_char '\n' text

let line_index text needle =
  let rec go i = function
    | [] -> failwith ("line not found: " ^ needle)
    | line :: rest -> if has line needle then i else go (i + 1) rest
  in
  go 0 (lines text)

let cell_parts text name =
  let cells = String.split_on_char '|' in
  let rec go = function
    | labels :: info :: pawns :: rest -> (
        match
          List.find_index (fun label -> String.trim label = name) (cells labels)
        with
        | Some column ->
            let at row = String.trim (List.nth (cells row) column) in
            (at info, at pawns)
        | None -> go (info :: pawns :: rest))
    | _ -> failwith ("cell not found: " ^ name)
  in
  go (lines text)

let size = View.minimum_size

let keys state ui keys =
  List.fold_left (fun ui key -> fst (View.update ~size state ui key)) ui keys

let view ?(size = size) state ui = View.plain (View.render ~size ~ui state)

let highlighted state ui =
  View.render ~ui state |> List.concat
  |> List.filter_map (fun (p : View_style.piece) ->
      if p.style = View_style.selected then Some p.text else None)

let no_escape text =
  assert_bool "plain text has an escape" (not (String.contains text '\027'))

let overview_tests =
  "overview"
  >::: [
         ( "corners" >:: fun _ ->
           let board = board "opening" in
           let top = line_index board "Free P" in
           let bottom = line_index board "Jail  " in
           assert_bool "Free Parking is above Jail" (top < bottom);
           assert_has "top row" (List.nth (lines board) top) "G2J ";
           assert_has "bottom row" (List.nth (lines board) bottom) "Go  " );
         ( "token on baltic" >:: fun _ ->
           assert_equal ("$60", "꩜") (cell_parts (board "buy") "Baltic") );
         ( "two tokens" >:: fun _ ->
           assert_equal ("꩜ H", "꩜ᨳ") (cell_parts (board "developed") "Baltic");
           assert_equal ("", "꩜ଓᨳ🐇") (cell_parts (board "opening") "Go") );
         ( "all five symbols fit and match the names in the roster" >:: fun _ ->
           let opening = state "opening" in
           let fifth =
             {
               (List.hd opening.players) with
               id = "emery";
               name = "Emery";
               token = "E";
             }
           in
           let state = { opening with players = opening.players @ [ fifth ] } in
           let board = View_board.render state in
           let third_row = List.nth board (List.length board - 2) in
           let names = List.concat (View_panel.render state) in
           let glyphs = [ "꩜"; "ଓ"; "ᨳ"; "🐇"; "ᡣ" ] in
           let tokens =
             List.filter
               (fun (p : View_style.piece) -> List.mem p.text glyphs)
               third_row
           in
           assert_equal
             (List.map2
                (fun glyph columns -> (glyph, columns))
                glyphs [ 1; 1; 1; 2; 1 ])
             (List.map
                (fun (p : View_style.piece) -> (p.text, p.columns))
                tokens);
           assert_equal
             (View_style.line_width (List.hd board))
             (View_style.line_width third_row);
           let colors =
             List.map (fun (p : View_style.piece) -> p.style.bg) tokens
           in
           assert_bool "each pawn has its own background"
             ((not (List.mem None colors))
             && List.length (List.sort_uniq compare colors) = 5);
           List.iter2
             (fun (player : View_state.player) (token : View_style.piece) ->
               let name_piece =
                 List.find
                   (fun (p : View_style.piece) -> p.text = player.name)
                   names
               in
               assert_equal ~msg:player.name token.style name_piece.style)
             state.players tokens );
         ( "cash and names" >:: fun _ ->
           let text = plain "opening" in
           List.iter
             (assert_has "opening" text)
             [ "Alex"; "Blake"; "Casey"; "Drew"; "$1500"; "Alex to roll." ] );
         ( "owned deed" >:: fun _ ->
           let text = plain "after_buy" in
           assert_has "cash" text "$1440";
           assert_has "deed count" text "1 deed" );
         ( "missing square" >:: fun _ ->
           let state = state "opening" in
           let squares =
             List.filter (fun sq -> sq.View_state.index <> 7) state.squares
           in
           let text = View.plain (View_board.render { state with squares }) in
           assert_has "placeholder" text "sq 7" );
       ]

let development_tests =
  "development"
  >::: [
         ( "marks" >:: fun _ ->
           let text = board "developed" in
           assert_equal ("꩜ 2", "") (cell_parts text "Med");
           assert_equal ("꩜ H", "꩜ᨳ") (cell_parts text "Baltic");
           assert_equal ("ଓ *", "ଓ") (cell_parts text "ReadRR");
           no_escape text );
         ( "brown style" >:: fun _ ->
           let doc = View.render (state "opening") in
           let found =
             List.exists
               (fun (piece : View_style.piece) ->
                 piece.style = View_style.of_group View_state.Brown
                 && has piece.text "Balt")
               (List.concat doc)
           in
           assert_bool "Baltic uses the brown style" found );
       ]

let prompt_tests =
  "prompts"
  >::: List.map
         (fun (id, needles) ->
           id >:: fun _ ->
           let text = plain id in
           List.iter (assert_has id text) needles;
           no_escape text)
         [
           ( "buy",
             [
               "Buy Baltic Avenue?";
               "Price $60";
               "Rent $4";
               "Alex has $1500";
               "[B] Buy";
               "[P] Pass";
             ] );
           ("rent", [ "Blake owes Alex $4 rent for Baltic Avenue."; "[Y] Pay" ]);
           ( "chance",
             [ "Chance"; "Advance to Go. Collect $200."; "[K] Acknowledge" ] );
           ( "jail",
             [
               "Drew is in jail. 2 turns left. Fine $50.";
               "[P] Pay $50";
               "[C] Use card";
               "[R] Roll for doubles";
             ] );
           ( "build",
             [
               "Build";
               "Mediterranean Avenue  houses 0  cost $50";
               "[1] Build Mediterranean";
             ] );
           ( "mortgage",
             [
               "Reading Railroad  value $100  open";
               "Short Line  value $100  mortgaged";
               "[M] Mortgage";
               "[U] Unmortgage";
             ] );
           ( "trade",
             [
               "Alex offers $50 and Baltic Avenue";
               "for Blake's $0 and Reading Railroad";
               "[Y] Accept";
               "[D] Decline";
             ] );
           ( "host",
             [
               "Address 0.0.0.0  port 3110";
               "preview only, no socket opened";
               "[H] Host";
               "[J] Join";
             ] );
           ( "bankrupt",
             [
               "Casey is bankrupt: owes $200 tax";
               "Still in: Alex, Blake, Drew";
               "[ᨳ] Casey  OUT";
             ] );
           ( "winner",
             [
               "Alex wins"; "Alex $2500"; "Blake $400"; "Casey $0"; "Drew $100";
             ] );
         ]

let screen_kind (screen : View_state.screen) =
  match screen with
  | Overview -> "overview"
  | Buy_or_pass _ -> "buy"
  | Payment _ -> "payment"
  | Card _ -> "card"
  | Jail _ -> "jail"
  | Build _ -> "build"
  | Mortgage _ -> "mortgage"
  | Trade _ -> "trade"
  | Host_join _ -> "host"
  | Bankruptcy _ -> "bankruptcy"
  | Winner _ -> "winner"

let tour_tests =
  "tour"
  >::: [
         ( "every screen" >:: fun _ ->
           let present =
             List.map
               (fun id -> screen_kind (state id).screen)
               (View_dummy.ids ())
           in
           List.iter
             (fun kind -> assert_bool kind (List.mem kind present))
             [
               "overview";
               "buy";
               "payment";
               "card";
               "jail";
               "build";
               "mortgage";
               "trade";
               "host";
               "bankruptcy";
               "winner";
             ] );
         ( "keys" >:: fun _ ->
           let opening = state "opening" in
           let buy = state "buy" in
           assert_equal None (View.command_of_key opening 'z');
           assert_equal (Some View_state.Next) (View.command_of_key opening 'n');
           assert_equal (Some View_state.Prev) (View.command_of_key opening 'p');
           assert_equal (Some View_state.Roll) (View.command_of_key opening 'R');
           assert_equal (Some View_state.Pass) (View.command_of_key buy 'p');
           assert_equal (Some View_state.Buy) (View.command_of_key buy 'b');
           assert_equal (Some View_state.Prev) (View.command_of_key buy '[');
           assert_equal (Some View_state.Next) (View.command_of_key buy ']') );
         ( "complete turn on buy or pass" >:: fun _ ->
           let rolled =
             View_dummy.apply (View_dummy.jump "opening") View_state.Roll
           in
           assert_equal "buy" (View_dummy.id rolled);
           let landing = View_dummy.snapshot rolled in
           assert_equal (Some 3)
             (Option.map
                (fun (p : View_state.player) -> p.position)
                (View_state.player landing "alex"));
           assert_equal
             (Some { View_state.die1 = 1; die2 = 2 })
             landing.last_dice;
           let bought = View_dummy.apply rolled View_state.Buy in
           let passed = View_dummy.apply rolled View_state.Pass in
           List.iter
             (fun (tour, id, owner, cash) ->
               assert_equal id (View_dummy.id tour);
               let state = View_dummy.snapshot tour in
               assert_equal "blake" state.current;
               assert_equal 2 state.turn;
               assert_equal owner
                 (Option.bind (View_state.square state 3) (fun sq -> sq.owner));
               assert_equal (Some cash)
                 (Option.map
                    (fun (p : View_state.player) -> p.cash)
                    (View_state.player state "alex")))
             [
               (bought, "after_buy", Some "alex", 1440);
               (passed, "after_pass", None, 1500);
             ] );
         ( "unlinked key stays" >:: fun _ ->
           let buy = View_dummy.jump "buy" in
           assert_equal "buy"
             (View_dummy.id (View_dummy.apply buy View_state.Roll)) );
         ( "next and prev" >:: fun _ ->
           let opening = View_dummy.jump "opening" in
           assert_equal "developed"
             (View_dummy.id (View_dummy.apply opening View_state.Next));
           assert_equal "winner"
             (View_dummy.id (View_dummy.apply opening View_state.Prev)) );
       ]

let interaction_tests =
  let open View in
  "interaction"
  >::: [
         ( "arrows follow perimeter only" >:: fun _ ->
           let state = state "opening" in
           let go = initial state in
           assert_equal [ " Go    "; "       "; "  " ] (highlighted state go);
           let boardwalk = keys state go [ Arrow Up ] in
           assert_has "up from Go" (view state boardwalk)
             "Tile 39  |  Boardwalk";
           assert_equal
             [ " Bwalk "; "$400   "; "       " ]
             (highlighted state boardwalk);
           assert_equal boardwalk
             (keys state boardwalk [ Arrow Left; Arrow Right ]);
           let med = keys state boardwalk [ Arrow Down; Arrow Left ] in
           assert_has "left from Go" (view state med)
             "Tile 01  |  Mediterranean Avenue";
           assert_equal
             [ " Med   "; "$60    "; "       " ]
             (highlighted state med);
           assert_equal med (keys state med [ Arrow Up; Arrow Down ]);
           assert_equal go (keys state med [ Arrow Right; Arrow Right ]) );
         ( "tile inspector is modal and restores focus" >:: fun _ ->
           let state = state "buy" in
           let board = initial state in
           let detail = keys state board [ Enter ] in
           let text = view state detail in
           List.iter
             (assert_has "tile detail" text)
             [
               "TILE   03  |  Baltic Avenue";
               "Group: Brown";
               "Price: $60";
               "Base rent: $4";
               "Owner: Unowned";
               "Occupants: [꩜] Alex";
               "[Esc] Back";
             ];
           assert_bool "board is replaced" (not (has text "PLAYERS"));
           assert_equal (detail, None) (update ~size state detail (Letter 'b'));
           assert_equal
             (detail, Some View_state.Quit)
             (update ~size state detail (Letter 'Q'));
           assert_equal board (keys state detail [ Escape ]) );
         ( "other player's portfolio and focus" >:: fun _ ->
           let state = state "developed" in
           let board = initial state in
           let roster = keys state board [ Tab; Arrow Down ] in
           let overview = view state roster in
           assert_has "focused player" overview ">[ଓ] Blake";
           assert_has "jailed player" overview "[🐇] Drew  JAIL";
           let detail = keys state roster [ Enter ] in
           let text = view state detail in
           List.iter
             (assert_has "Blake's portfolio" text)
             [
               "PLAYER   Blake";
               "Cash: $1100";
               "1 deed";
               "1 mortgaged";
               "Reading Railroad  |  Railroad  |  No improvements  |  Mortgaged";
             ];
           assert_equal roster (keys state detail [ Escape ]);
           assert_equal [] (highlighted state roster);
           assert_equal
             [ " Baltic"; " H"; "    "; "     " ]
             (highlighted state (keys state roster [ Tab; Tab ]));
           assert_equal roster (keys state roster [ Tab; Tab; Tab ]) );
         ( "portfolio scroll survives a resize below minimum" >:: fun _ ->
           let base = state "opening" in
           let squares =
             List.map
               (fun (sq : View_state.square) ->
                 if Option.is_some sq.price then
                   let name =
                     if sq.index < 20 then sq.name ^ " " ^ String.make 110 'x'
                     else sq.name
                   in
                   { sq with owner = Some "alex"; name }
                 else sq)
               base.squares
           in
           let state = { base with squares } in
           let first = keys state (initial state) [ Tab; Enter ] in
           assert_bool "last property starts off-screen"
             (not (has (view state first) "Boardwalk"));
           let last = keys state first (List.init 20 (fun _ -> Arrow Down)) in
           let last_page = view state last in
           assert_has "last property reachable" last_page "Boardwalk";
           assert_has "resizing suspends the inspector"
             (view ~size:(119, 47) state last)
             "need 120x47 (current 119x47)";
           let paused, command = update ~size:(119, 47) state last Escape in
           assert_equal None command;
           assert_equal last_page (view state paused);
           assert_equal first
             (keys state last (List.init 20 (fun _ -> Arrow Up))) );
         ( "size gate on either dimension" >:: fun _ ->
           let state = state "opening" in
           let ui = initial state in
           List.iter
             (fun size ->
               assert_has "resize hint" (view ~size state ui)
                 "Resize terminal: need 120x47";
               assert_equal (ui, None) (update ~size state ui (Arrow Left));
               assert_equal (ui, None) (update ~size state ui (Letter 'r'));
               assert_equal (ui, Some View_state.Quit)
                 (update ~size state ui (Letter 'q')))
             [ (119, 47); (120, 46) ];
           assert_has "minimum is playable" (view state ui) "[Enter] Inspect" );
         ( "demo screens fit the minimum" >:: fun _ ->
           List.iter
             (fun id ->
               let tour = View_dummy.jump id in
               let rows =
                 render ~preview:(View_dummy.is_preview tour) (state id)
               in
               assert_equal ~msg:id 47 (List.length rows);
               assert_bool (id ^ " width")
                 (List.for_all
                    (fun row -> View_style.line_width row < 120)
                    rows);
               if View_dummy.is_preview tour then
                 assert_has id (View.plain rows) "PREVIEW")
             (View_dummy.ids ()) );
       ]

let inventory_tests =
  let open View in
  let enter_inventory state ui = keys state ui [ Tab; Tab ] in
  let summary heading state ui =
    let text = view state ui in
    lines text |> List.drop (line_index text heading) |> String.concat "\n"
  in
  "inventory"
  >::: [
         ( "inspect a current player's card and deed" >:: fun _ ->
           let state = state "developed" in
           let board = initial state in
           let pane = enter_inventory state board in
           let text = view state pane in
           assert_bool "inventory follows roster"
             (line_index text "PLAYERS" < line_index text "CURRENT PLAYER");
           List.iter
             (assert_has "inventory" (summary "CURRENT PLAYER" state pane))
             [
               "[꩜] Alex";
               "Cash: $800";
               "Cards: 1  |  Deeds: 2";
               ">Card: Get Out of Jail Free";
             ];
           let card, command = update ~size state pane Enter in
           assert_equal None command;
           List.iter
             (assert_has "card details" (view state card))
             [
               "HELD CARD   Get Out of Jail Free";
               "Deck: Chance";
               "Keep this card until you use it to leave jail.";
             ];
           assert_equal pane (keys state card [ Escape ]);
           let deed = keys state pane [ Arrow Down; Enter ] in
           List.iter
             (assert_has "deed details" (view state deed))
             [
               "TILE   01  |  Mediterranean Avenue";
               "Owner: Alex";
               "Improvements: 2 houses";
             ];
           assert_equal
             (keys state pane [ Arrow Down ])
             (keys state deed [ Escape ]);
           let next = { state with current = "casey" } in
           let empty_pane = enter_inventory next board in
           assert_has "empty inventory"
             (summary "CURRENT PLAYER" next empty_pane)
             "No cards or deeds.";
           assert_equal (empty_pane, None) (update ~size next empty_pane Enter)
         );
         ( "server view follows the local player across turn changes"
         >:: fun _ ->
           let base = state "developed" in
           let local = initial ~local_player:"alex" base in
           assert_has "before" (summary "YOUR ITEMS" base local) "Cash: $800";
           let players =
             List.map
               (fun (p : View_state.player) ->
                 if p.id = "alex" then { p with cash = 735 } else p)
               base.players
           in
           let next = { base with current = "blake"; players } in
           assert_has "another player's turn" (view next local) "Blake to play";
           let local_items = summary "YOUR ITEMS" next local in
           List.iter
             (assert_has "local items" local_items)
             [
               "[꩜] Alex";
               "Cash: $735";
               "Card: Get Out of Jail Free";
               "Deed: Baltic Avenue";
             ];
           let local_card = keys next (enter_inventory next local) [ Enter ] in
           assert_has "inspect during another turn" (view next local_card)
             "Deck: Chance";
           let current_items = summary "CURRENT PLAYER" next (initial next) in
           List.iter
             (assert_has "current items" current_items)
             [ "[ଓ] Blake"; "Cash: $1100"; "Cards: 0  |  Deeds: 1" ];
           let missing = initial ~local_player:"missing" base in
           let missing_items = summary "YOUR ITEMS" base missing in
           assert_has "missing local player" missing_items "Player unavailable.";
           assert_bool "no fallback to current player"
             (not (has missing_items "Card: Get Out of Jail Free")) );
         ( "inspect beyond the first page of items" >:: fun _ ->
           let base = state "opening" in
           let squares =
             List.map
               (fun (s : View_state.square) ->
                 if List.mem s.index [ 1; 3; 5; 6; 8; 9; 11; 12 ] then
                   { s with owner = Some "alex" }
                 else s)
               base.squares
           in
           let state = { base with squares } in
           let pane = enter_inventory state (initial state) in
           assert_has "first page" (view state pane) "Items 1-7 of 8";
           let last = keys state pane (List.init 7 (fun _ -> Arrow Down)) in
           assert_has "last page" (view state last) "Items 8-8 of 8";
           assert_has "last deed focused" (view state last)
             ">Deed: Electric Company";
           assert_has "last deed inspector"
             (view state (keys state last [ Enter ]))
             "TILE   12  |  Electric Company" );
       ]

let decoding_test =
  "decodes and consumes escape sequences" >:: fun _ ->
  let decode_bytes input =
    let tail = ref (List.of_seq (String.to_seq input)) in
    let read_more () =
      match !tail with
      | [] -> None
      | c :: rest ->
          tail := rest;
          Some c
    in
    let key = View_input.decode ~read_more (Option.get (read_more ())) in
    (key, !tail)
  in
  let open View_input in
  assert_equal (Some (Arrow Left), [ 'r' ]) (decode_bytes "\027[Dr");
  assert_equal (Some (Arrow Down), []) (decode_bytes "\027OB");
  assert_equal (None, [ 'b' ]) (decode_bytes "\027[1;5pb");
  assert_equal (None, [ 'q' ]) (decode_bytes "\027[[Bq");
  assert_equal (None, [ 'q' ]) (decode_bytes "\027(Bq");
  assert_equal (None, []) (decode_bytes "\027]ignored r\007");
  assert_equal (None, []) (decode_bytes "\027[1;");
  assert_equal (Some Escape, []) (decode_bytes "\027");
  assert_equal (Some Enter, []) (decode_bytes "\r");
  assert_equal (Some Enter, []) (decode_bytes "\n");
  assert_equal (Some Tab, []) (decode_bytes "\t")

let tax_test =
  "tax payment" >:: fun _ ->
  let rent = state "rent" in
  let tax =
    {
      rent with
      screen =
        View_state.Payment
          {
            kind = Tax;
            payer = "casey";
            amount = 200;
            payee = None;
            square = Some 4;
          };
    }
  in
  assert_has "tax"
    (View.plain (View.render tax))
    "Casey owes $200 tax for Income Tax."

let tests =
  "view"
  >::: [
         overview_tests;
         development_tests;
         prompt_tests;
         tour_tests;
         interaction_tests;
         inventory_tests;
         decoding_test;
         tax_test;
       ]

let () = run_test_tt_main tests
