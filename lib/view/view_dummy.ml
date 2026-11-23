type t = { index : int }

let spec name group price rent = (name, group, price, rent)

let specs =
  [|
    spec "Go" View_state.None None None;
    spec "Mediterranean Avenue" View_state.Brown (Some 60) (Some 2);
    spec "Community Chest" View_state.None None None;
    spec "Baltic Avenue" View_state.Brown (Some 60) (Some 4);
    spec "Income Tax" View_state.None None None;
    spec "Reading Railroad" View_state.Railroad (Some 200) (Some 25);
    spec "Oriental Avenue" View_state.Light_blue (Some 100) (Some 6);
    spec "Chance" View_state.None None None;
    spec "Vermont Avenue" View_state.Light_blue (Some 100) (Some 6);
    spec "Connecticut Avenue" View_state.Light_blue (Some 120) (Some 8);
    spec "Jail" View_state.None None None;
    spec "St. Charles Place" View_state.Pink (Some 140) (Some 10);
    spec "Electric Company" View_state.Utility (Some 150) None;
    spec "States Avenue" View_state.Pink (Some 140) (Some 10);
    spec "Virginia Avenue" View_state.Pink (Some 160) (Some 12);
    spec "Pennsylvania Railroad" View_state.Railroad (Some 200) (Some 25);
    spec "St. James Place" View_state.Orange (Some 180) (Some 14);
    spec "Community Chest" View_state.None None None;
    spec "Tennessee Avenue" View_state.Orange (Some 180) (Some 14);
    spec "New York Avenue" View_state.Orange (Some 200) (Some 16);
    spec "Free Parking" View_state.None None None;
    spec "Kentucky Avenue" View_state.Red (Some 220) (Some 18);
    spec "Chance" View_state.None None None;
    spec "Indiana Avenue" View_state.Red (Some 220) (Some 18);
    spec "Illinois Avenue" View_state.Red (Some 240) (Some 20);
    spec "B. & O. Railroad" View_state.Railroad (Some 200) (Some 25);
    spec "Atlantic Avenue" View_state.Yellow (Some 260) (Some 22);
    spec "Ventnor Avenue" View_state.Yellow (Some 260) (Some 22);
    spec "Water Works" View_state.Utility (Some 150) None;
    spec "Marvin Gardens" View_state.Yellow (Some 280) (Some 24);
    spec "Go to Jail" View_state.None None None;
    spec "Pacific Avenue" View_state.Green (Some 300) (Some 26);
    spec "North Carolina Avenue" View_state.Green (Some 300) (Some 26);
    spec "Community Chest" View_state.None None None;
    spec "Pennsylvania Avenue" View_state.Green (Some 320) (Some 28);
    spec "Short Line" View_state.Railroad (Some 200) (Some 25);
    spec "Chance" View_state.None None None;
    spec "Park Place" View_state.Dark_blue (Some 350) (Some 35);
    spec "Luxury Tax" View_state.None None None;
    spec "Boardwalk" View_state.Dark_blue (Some 400) (Some 50);
  |]

let board =
  Array.to_list
    (Array.mapi
       (fun index (name, group, price, rent) ->
         {
           View_state.index;
           name;
           group;
           price;
           rent;
           owner = None;
           houses = 0;
           hotel = false;
           mortgaged = false;
         })
       specs)

let set index f squares =
  List.map (fun sq -> if sq.View_state.index = index then f sq else sq) squares

let give index owner squares =
  set index (fun sq -> { sq with owner = Some owner }) squares

let houses index count squares =
  set index (fun sq -> { sq with houses = count }) squares

let hotel index squares = set index (fun sq -> { sq with hotel = true }) squares

let mortgage index squares =
  set index (fun sq -> { sq with mortgaged = true }) squares

let player id name token cash position ?(in_jail = false) ?(bankrupt = false)
    ?(cards = []) () =
  { View_state.id; name; token; cash; position; in_jail; bankrupt; cards }

let alex = player "alex" "Alex" "A"
let blake = player "blake" "Blake" "B"
let casey = player "casey" "Casey" "C"
let drew = player "drew" "Drew" "D"
let item key label command = { View_state.key; label; command }
let quit = item 'q' "Quit" View_state.Quit

let jail_card deck =
  {
    View_state.deck;
    title = "Get Out of Jail Free";
    body = "Keep this card until you use it to leave jail.";
  }

let frame ~current ~turn ~screen ~menu ~log ?dice ?(squares = board) players =
  {
    View_state.squares;
    players;
    current;
    turn;
    screen;
    menu;
    log;
    last_dice = dice;
  }

let opening =
  frame ~current:"alex" ~turn:1 ~screen:View_state.Overview
    ~menu:[ item 'r' "Roll" View_state.Roll; quit ]
    ~log:[ "Alex to roll." ]
    [ alex 1500 0 (); blake 1500 0 (); casey 1500 0 (); drew 1500 0 () ]

let developed_squares =
  board |> give 1 "alex" |> houses 1 2 |> give 3 "alex" |> hotel 3
  |> give 5 "blake" |> mortgage 5

let developed =
  frame ~current:"alex" ~turn:6 ~screen:View_state.Overview
    ~menu:[ item 'r' "Roll" View_state.Roll; quit ]
    ~log:[ "Sample: houses, a hotel, and a mortgage." ]
    ~squares:developed_squares
    [
      alex 800 3 ~cards:[ jail_card View_state.Chance ] ();
      blake 1100 5 ();
      casey 900 3 ();
      drew 700 10 ~in_jail:true ();
    ]

let buy =
  frame ~current:"alex" ~turn:1
    ~screen:(View_state.Buy_or_pass { square = 3; price = 60; rent = 4 })
    ~menu:
      [ item 'b' "Buy" View_state.Buy; item 'p' "Pass" View_state.Pass; quit ]
    ~log:[ "Alex rolled 1 + 2 and landed on Baltic Avenue." ]
    ~dice:{ die1 = 1; die2 = 2 }
    [ alex 1500 3 (); blake 1500 0 (); casey 1500 0 (); drew 1500 0 () ]

let after_buy =
  frame ~current:"blake" ~turn:2 ~screen:View_state.Overview ~menu:[ quit ]
    ~log:[ "Alex bought Baltic Avenue for $60."; "Demo turn complete." ]
    ~dice:{ die1 = 1; die2 = 2 } ~squares:(give 3 "alex" board)
    [ alex 1440 3 (); blake 1500 0 (); casey 1500 0 (); drew 1500 0 () ]

let after_pass =
  {
    buy with
    current = "blake";
    turn = 2;
    screen = View_state.Overview;
    menu = [ quit ];
    log = [ "Alex passed on Baltic Avenue."; "Demo turn complete." ];
  }

let rent =
  frame ~current:"blake" ~turn:2
    ~screen:
      (View_state.Payment
         {
           kind = Rent;
           payer = "blake";
           amount = 4;
           payee = Some "alex";
           square = Some 3;
         })
    ~menu:[ item 'y' "Pay" View_state.Pay; quit ]
    ~log:[ "Blake owes Alex $4 rent." ]
    ~squares:(give 3 "alex" board)
    [ alex 1440 3 (); blake 1500 3 (); casey 1500 0 (); drew 1500 0 () ]

let chance =
  frame ~current:"casey" ~turn:3
    ~screen:
      (View_state.Card { deck = Chance; body = "Advance to Go. Collect $200." })
    ~menu:[ item 'k' "Acknowledge" View_state.Ack_card; quit ]
    ~log:[ "Casey drew a Chance card." ]
    [ alex 1440 3 (); blake 1496 0 (); casey 1500 7 (); drew 1500 0 () ]

let jail =
  frame ~current:"drew" ~turn:4
    ~screen:
      (View_state.Jail
         { player = "drew"; turns_left = 2; fine = 50; has_card = true })
    ~menu:
      [
        item 'p' "Pay $50" View_state.Pay_jail;
        item 'c' "Use card" View_state.Use_jail_card;
        item 'r' "Roll for doubles" View_state.Roll_doubles;
        quit;
      ]
    ~log:[ "Drew is in jail." ]
    [
      alex 1440 3 ();
      blake 1496 0 ();
      casey 1500 0 ();
      drew 1500 10 ~in_jail:true ~cards:[ jail_card View_state.Community ] ();
    ]

let build =
  frame ~current:"alex" ~turn:5
    ~screen:
      (View_state.Build
         {
           options =
             [
               { square = 1; houses = 0; cost = 50 };
               { square = 3; houses = 0; cost = 50 };
             ];
         })
    ~menu:
      [
        item '1' "Build Mediterranean" View_state.Build;
        item '2' "Build Baltic" View_state.Build;
        quit;
      ]
    ~log:[ "Alex may build on the brown set." ]
    ~squares:(board |> give 1 "alex" |> give 3 "alex")
    [ alex 1440 1 (); blake 1496 0 (); casey 1500 0 (); drew 1500 0 () ]

let mortgage_squares = board |> give 5 "blake" |> give 35 "blake" |> mortgage 35

let mortgage =
  frame ~current:"blake" ~turn:6
    ~screen:
      (View_state.Mortgage
         {
           options =
             [
               { square = 5; value = 100; mortgaged = false };
               { square = 35; value = 100; mortgaged = true };
             ];
         })
    ~menu:
      [
        item 'm' "Mortgage" View_state.Mortgage;
        item 'u' "Unmortgage" View_state.Unmortgage;
        quit;
      ]
    ~log:[ "Blake looks at mortgages." ]
    ~squares:mortgage_squares
    [ alex 1440 0 (); blake 1496 5 (); casey 1500 0 (); drew 1500 0 () ]

let trade =
  frame ~current:"blake" ~turn:7
    ~screen:
      (View_state.Trade
         {
           offer = { player = "alex"; cash = 50; squares = [ 3 ] };
           request = { player = "blake"; cash = 0; squares = [ 5 ] };
         })
    ~menu:
      [
        item 'y' "Accept" View_state.Accept_trade;
        item 'd' "Decline" View_state.Decline_trade;
        quit;
      ]
    ~log:[ "Alex offers Baltic Avenue and $50 for Reading Railroad." ]
    ~squares:(board |> give 3 "alex" |> give 5 "blake")
    [ alex 1440 3 (); blake 1496 5 (); casey 1500 0 (); drew 1500 0 () ]

let host =
  frame ~current:"alex" ~turn:0
    ~screen:(View_state.Host_join { host = "0.0.0.0"; port = 3110 })
    ~menu:
      [ item 'h' "Host" View_state.Host; item 'j' "Join" View_state.Join; quit ]
    ~log:[ "Choose host or join." ]
    [ alex 1500 0 (); blake 1500 0 (); casey 1500 0 (); drew 1500 0 () ]

let bankrupt =
  frame ~current:"alex" ~turn:12
    ~screen:
      (View_state.Bankruptcy { player = "casey"; reason = "owes $200 tax" })
    ~menu:[ quit ] ~log:[ "Casey is bankrupt." ]
    [
      alex 1800 0 ();
      blake 1200 0 ();
      casey 0 4 ~bankrupt:true ();
      drew 900 0 ();
    ]

let winner =
  frame ~current:"alex" ~turn:20
    ~screen:(View_state.Winner { player = "alex" })
    ~menu:[ quit ] ~log:[ "Alex wins." ]
    [ alex 2500 0 (); blake 400 0 (); casey 0 0 (); drew 100 0 () ]

let tour =
  [|
    ("opening", opening);
    ("developed", developed);
    ("buy", buy);
    ("after_buy", after_buy);
    ("after_pass", after_pass);
    ("rent", rent);
    ("chance", chance);
    ("jail", jail);
    ("build", build);
    ("mortgage", mortgage);
    ("trade", trade);
    ("host", host);
    ("bankrupt", bankrupt);
    ("winner", winner);
  |]

let count = Array.length tour
let ids () = Array.to_list (Array.map fst tour)

let index_of id =
  let rec go i =
    if i = count then failwith ("unknown view frame " ^ id)
    else if fst tour.(i) = id then i
    else go (i + 1)
  in
  go 0

let jump id = { index = index_of id }
let id tour_state = fst tour.(tour_state.index)
let snapshot tour_state = snd tour.(tour_state.index)

let is_preview tour_state =
  match id tour_state with
  | "opening" | "buy" | "after_buy" | "after_pass" -> false
  | _ -> true

let apply tour_state command =
  let here = id tour_state in
  let dest =
    match (here, command) with
    | _, View_state.Next -> Some ((tour_state.index + 1) mod count)
    | _, View_state.Prev -> Some ((tour_state.index + count - 1) mod count)
    | "opening", View_state.Roll -> Some (index_of "buy")
    | "buy", View_state.Buy -> Some (index_of "after_buy")
    | "buy", View_state.Pass -> Some (index_of "after_pass")
    | _ -> None
  in
  match dest with
  | None -> tour_state
  | Some index -> { index }
