let item key label command = { View_state.key; label; command }
let quit = item 'q' "Quit" View_state.Quit

let group_of (sq : Board.square) =
  match sq.kind with
  | Property { group; _ } -> (
      match group with
      | "brown" -> View_state.Brown
      | "light_blue" -> View_state.Light_blue
      | "pink" -> View_state.Pink
      | "orange" -> View_state.Orange
      | "red" -> View_state.Red
      | "yellow" -> View_state.Yellow
      | "green" -> View_state.Green
      | "dark_blue" -> View_state.Dark_blue
      | _ -> View_state.None)
  | Railroad _ -> View_state.Railroad
  | Utility _ -> View_state.Utility
  | _ -> View_state.None

let base_rent (sq : Board.square) =
  match sq.kind with
  | Property { rent; _ } -> Some rent
  | Railroad _ -> Some 25
  | _ -> None

let dice_total g =
  match Game.last_dice g with
  | Some (a, b) -> a + b
  | None -> 0

let offered_rent g i =
  match (Board.square (Game.board g) i).kind with
  | Property { rent; _ } -> rent
  | Railroad _ -> 25
  | Utility _ -> 4 * dice_total g
  | _ -> 0

let square_view g i =
  let sq = Board.square (Game.board g) i in
  {
    View_state.index = i;
    name = sq.name;
    group = group_of sq;
    price = Board.price sq;
    rent = base_rent sq;
    owner = Game.owner g i;
    houses = 0;
    hotel = false;
    mortgaged = false;
  }

let player_view (p : Game.player) =
  {
    View_state.id = p.id;
    name = p.name;
    token = "";
    cash = p.cash;
    position = p.position;
    in_jail = false;
    bankrupt = false;
    cards = [];
  }

let screen_and_menu g =
  match Game.phase g with
  | Awaiting_roll ->
      (View_state.Overview, [ item 'r' "Roll" View_state.Roll; quit ])
  | Awaiting_buy i ->
      let price =
        Option.value (Board.price (Board.square (Game.board g) i)) ~default:0
      in
      ( View_state.Buy_or_pass { square = i; price; rent = offered_rent g i },
        [ item 'b' "Buy" View_state.Buy; item 'p' "Pass" View_state.Pass; quit ]
      )
  | Awaiting_rent { square; owner; amount } ->
      ( View_state.Payment
          {
            kind = Rent;
            payer = (Game.current g).id;
            amount;
            payee = Some owner;
            square = Some square;
          },
        [ item 'y' "Pay" View_state.Pay; quit ] )

let snapshot g =
  let screen, menu = screen_and_menu g in
  {
    View_state.squares = List.init (Board.size (Game.board g)) (square_view g);
    players = List.map player_view (Game.players g);
    current = (Game.current g).id;
    turn = Game.turn g;
    screen;
    menu;
    log = Game.log g;
    last_dice =
      Option.map
        (fun (die1, die2) -> { View_state.die1; die2 })
        (Game.last_dice g);
  }

let apply ~roll cmd g =
  let result =
    match cmd with
    | View_state.Roll ->
        let dice = if Game.phase g = Awaiting_roll then roll () else (1, 1) in
        Game.roll dice g
    | Buy -> Game.buy g
    | Pass -> Game.pass g
    | Pay -> Game.pay g
    | Quit | Next | Prev -> Ok g
    | Ack_card
    | Pay_jail
    | Use_jail_card
    | Roll_doubles
    | Build
    | Mortgage
    | Unmortgage
    | Accept_trade
    | Decline_trade
    | Host
    | Join -> Error "That action is not part of this game yet."
  in
  match result with
  | Ok g -> g
  | Error msg -> Game.note msg g
