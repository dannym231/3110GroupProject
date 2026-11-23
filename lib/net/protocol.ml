type request =
  | Join of string
  | Poll
  | Act of {
      revision : int;
      command : View_state.command;
    }

type response =
  | Joined of View_state.player_id * Session.snapshot
  | State of Session.snapshot
  | Rejected of string

module Json = Yojson.Safe
module Read = Json.Util

let obj fields : Json.t = `Assoc fields
let str value : Json.t = `String value
let int value : Json.t = `Int value
let bool value : Json.t = `Bool value
let list write values : Json.t = `List (List.map write values)

let opt write = function
  | None -> `Null
  | Some value -> write value

let get name read value = read (Read.member name value)
let read_str = get "type" Read.to_string

let read_opt read = function
  | `Null -> None
  | value -> Some (read value)

let read_list read value = List.map read (Read.to_list value)
let version = 1

let actions : (string * View_state.command) list =
  [ ("roll", Roll); ("buy", Buy); ("pass", Pass); ("pay", Pay) ]

let enum read values key =
  match List.assoc_opt key values with
  | Some value -> value
  | None -> failwith ("Unknown " ^ read ^ ".")

let encode_enum values value =
  str (fst (List.find (fun (_, candidate) -> candidate = value) values))

let encode_request request =
  Json.to_string
    (match request with
    | Join name ->
        obj
          [ ("type", str "join"); ("version", int version); ("name", str name) ]
    | Poll -> obj [ ("type", str "poll") ]
    | Act { revision; command } ->
        obj
          [
            ("type", str "act");
            ("revision", int revision);
            ("action", encode_enum actions command);
          ])

let decode read data =
  try Ok (read (Json.from_string data)) with
  | Yojson.Json_error _ | Read.Type_error _ -> Error "Malformed LAN message."
  | Failure reason -> Error reason

let expect_fields fields json =
  let actual = List.map fst (Read.to_assoc json) in
  if List.sort String.compare fields <> List.sort String.compare actual then
    failwith "Unexpected fields in LAN request."

let decode_request =
  decode (fun json ->
      match read_str json with
      | "join" ->
          expect_fields [ "type"; "version"; "name" ] json;
          if get "version" Read.to_int json <> version then
            failwith "Incompatible LAN protocol version.";
          Join (get "name" Read.to_string json)
      | "poll" ->
          expect_fields [ "type" ] json;
          Poll
      | "act" ->
          expect_fields [ "type"; "revision"; "action" ] json;
          let revision = get "revision" Read.to_int json in
          let command =
            enum "action" actions (get "action" Read.to_string json)
          in
          Act { revision; command }
      | _ -> failwith "Unknown LAN request.")

let groups : (string * View_state.color_group) list =
  [
    ("brown", Brown);
    ("light_blue", Light_blue);
    ("pink", Pink);
    ("orange", Orange);
    ("red", Red);
    ("yellow", Yellow);
    ("green", Green);
    ("dark_blue", Dark_blue);
    ("railroad", Railroad);
    ("utility", Utility);
    ("none", None);
  ]

let decks : (string * View_state.deck) list =
  [ ("chance", Chance); ("community", Community) ]

let payments : (string * View_state.payment_kind) list =
  [ ("rent", Rent); ("tax", Tax) ]

let write_square (sq : View_state.square) =
  obj
    [
      ("index", int sq.index);
      ("name", str sq.name);
      ("group", encode_enum groups sq.group);
      ("price", opt int sq.price);
      ("rent", opt int sq.rent);
      ("owner", opt str sq.owner);
      ("houses", int sq.houses);
      ("hotel", bool sq.hotel);
      ("mortgaged", bool sq.mortgaged);
    ]

let read_square json : View_state.square =
  {
    index = get "index" Read.to_int json;
    name = get "name" Read.to_string json;
    group = enum "color group" groups (get "group" Read.to_string json);
    price = get "price" (read_opt Read.to_int) json;
    rent = get "rent" (read_opt Read.to_int) json;
    owner = get "owner" (read_opt Read.to_string) json;
    houses = get "houses" Read.to_int json;
    hotel = get "hotel" Read.to_bool json;
    mortgaged = get "mortgaged" Read.to_bool json;
  }

let write_card (card : View_state.held_card) =
  obj
    [
      ("deck", encode_enum decks card.deck);
      ("title", str card.title);
      ("body", str card.body);
    ]

let read_card json : View_state.held_card =
  {
    deck = enum "deck" decks (get "deck" Read.to_string json);
    title = get "title" Read.to_string json;
    body = get "body" Read.to_string json;
  }

let write_player (player : View_state.player) =
  obj
    [
      ("id", str player.id);
      ("name", str player.name);
      ("token", str player.token);
      ("cash", int player.cash);
      ("position", int player.position);
      ("in_jail", bool player.in_jail);
      ("bankrupt", bool player.bankrupt);
      ("cards", list write_card player.cards);
    ]

let read_player json : View_state.player =
  {
    id = get "id" Read.to_string json;
    name = get "name" Read.to_string json;
    token = get "token" Read.to_string json;
    cash = get "cash" Read.to_int json;
    position = get "position" Read.to_int json;
    in_jail = get "in_jail" Read.to_bool json;
    bankrupt = get "bankrupt" Read.to_bool json;
    cards = get "cards" (read_list read_card) json;
  }

let write_screen (screen : View_state.screen) =
  match screen with
  | Overview -> obj [ ("type", str "overview") ]
  | Buy_or_pass { square; price; rent } ->
      obj
        [
          ("type", str "buy_or_pass");
          ("square", int square);
          ("price", int price);
          ("rent", int rent);
        ]
  | Payment { kind; payer; amount; payee; square } ->
      obj
        [
          ("type", str "payment");
          ("kind", encode_enum payments kind);
          ("payer", str payer);
          ("amount", int amount);
          ("payee", opt str payee);
          ("square", opt int square);
        ]
  | _ -> failwith "Unsupported LAN screen."

let read_screen json : View_state.screen =
  match read_str json with
  | "overview" -> Overview
  | "buy_or_pass" ->
      Buy_or_pass
        {
          square = get "square" Read.to_int json;
          price = get "price" Read.to_int json;
          rent = get "rent" Read.to_int json;
        }
  | "payment" ->
      Payment
        {
          kind = enum "payment" payments (get "kind" Read.to_string json);
          payer = get "payer" Read.to_string json;
          amount = get "amount" Read.to_int json;
          payee = get "payee" (read_opt Read.to_string) json;
          square = get "square" (read_opt Read.to_int) json;
        }
  | _ -> failwith "Unsupported LAN screen."

let menu_commands = ("quit", View_state.Quit) :: actions

let write_menu_item (item : View_state.menu_item) =
  obj
    [
      ("key", str (String.make 1 item.key));
      ("label", str item.label);
      ("command", encode_enum menu_commands item.command);
    ]

let read_menu_item json : View_state.menu_item =
  let key = get "key" Read.to_string json in
  if String.length key <> 1 then failwith "Invalid menu key.";
  {
    key = key.[0];
    label = get "label" Read.to_string json;
    command =
      enum "menu command" menu_commands (get "command" Read.to_string json);
  }

let write_dice (dice : View_state.dice) =
  obj [ ("die1", int dice.die1); ("die2", int dice.die2) ]

let read_dice json : View_state.dice =
  { die1 = get "die1" Read.to_int json; die2 = get "die2" Read.to_int json }

let write_snapshot ({ revision; view } : Session.snapshot) =
  obj
    [
      ("revision", int revision);
      ("squares", list write_square view.squares);
      ("players", list write_player view.players);
      ("current", str view.current);
      ("turn", int view.turn);
      ("screen", write_screen view.screen);
      ("menu", list write_menu_item view.menu);
      ("log", list str view.log);
      ("last_dice", opt write_dice view.last_dice);
    ]

let read_snapshot json : Session.snapshot =
  {
    revision = get "revision" Read.to_int json;
    view =
      {
        squares = get "squares" (read_list read_square) json;
        players = get "players" (read_list read_player) json;
        current = get "current" Read.to_string json;
        turn = get "turn" Read.to_int json;
        screen = get "screen" read_screen json;
        menu = get "menu" (read_list read_menu_item) json;
        log = get "log" (read_list Read.to_string) json;
        last_dice = get "last_dice" (read_opt read_dice) json;
      };
  }

let encode_response response =
  Json.to_string
    (match response with
    | Joined (player, state) ->
        obj
          [
            ("type", str "joined");
            ("player", str player);
            ("state", write_snapshot state);
          ]
    | State state ->
        obj [ ("type", str "state"); ("state", write_snapshot state) ]
    | Rejected reason ->
        obj [ ("type", str "rejected"); ("reason", str reason) ])

let decode_response =
  decode (fun json ->
      match read_str json with
      | "joined" ->
          Joined
            (get "player" Read.to_string json, get "state" read_snapshot json)
      | "state" -> State (get "state" read_snapshot json)
      | "rejected" -> Rejected (get "reason" Read.to_string json)
      | _ -> failwith "Unknown LAN response.")
