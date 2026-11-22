type player_id = string

type color_group =
  | Brown
  | Light_blue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | Dark_blue
  | Railroad
  | Utility
  | None

type square = {
  index : int;
  name : string;
  group : color_group;
  price : int option;
  rent : int option;
  owner : player_id option;
  houses : int;
  hotel : bool;
  mortgaged : bool;
}

type deck =
  | Chance
  | Community

type held_card = {
  deck : deck;
  title : string;
  body : string;
}

type player = {
  id : player_id;
  name : string;
  token : string;
  cash : int;
  position : int;
  in_jail : bool;
  bankrupt : bool;
  cards : held_card list;
}

type dice = {
  die1 : int;
  die2 : int;
}

type command =
  | Roll
  | Buy
  | Pass
  | Pay
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
  | Join
  | Next
  | Prev
  | Quit

type menu_item = {
  key : char;
  label : string;
  command : command;
}

type build_option = {
  square : int;
  houses : int;
  cost : int;
}

type mortgage_option = {
  square : int;
  value : int;
  mortgaged : bool;
}

type trade_side = {
  player : player_id;
  cash : int;
  squares : int list;
}

type payment_kind =
  | Rent
  | Tax

type screen =
  | Overview
  | Buy_or_pass of {
      square : int;
      price : int;
      rent : int;
    }
  | Payment of {
      kind : payment_kind;
      payer : player_id;
      amount : int;
      payee : player_id option;
      square : int option;
    }
  | Card of {
      deck : deck;
      body : string;
    }
  | Jail of {
      player : player_id;
      turns_left : int;
      fine : int;
      has_card : bool;
    }
  | Build of { options : build_option list }
  | Mortgage of { options : mortgage_option list }
  | Trade of {
      offer : trade_side;
      request : trade_side;
    }
  | Host_join of {
      host : string;
      port : int;
    }
  | Bankruptcy of {
      player : player_id;
      reason : string;
    }
  | Winner of { player : player_id }

type t = {
  squares : square list;
  players : player list;
  current : player_id;
  turn : int;
  screen : screen;
  menu : menu_item list;
  log : string list;
  last_dice : dice option;
}

let money n = if n < 0 then "-$" ^ string_of_int (-n) else "$" ^ string_of_int n

let deck_name = function
  | Chance -> "Chance"
  | Community -> "Community Chest"

let player state id = List.find_opt (fun p -> p.id = id) state.players

let player_name state id =
  match player state id with
  | Some p -> p.name
  | None -> id

let square state index = List.find_opt (fun s -> s.index = index) state.squares

let square_name state index =
  match square state index with
  | Some s -> s.name
  | None -> "square " ^ string_of_int index
