let width = 28
let line = View_style.line

let quantity n noun =
  Printf.sprintf "%d %s%s" n noun (if n = 1 then "" else "s")

let deeds (state : View_state.t) (player : View_state.player) =
  state.squares
  |> List.filter (fun (sq : View_state.square) -> sq.owner = Some player.id)
  |> List.sort (fun (a : View_state.square) (b : View_state.square) ->
      compare a.index b.index)

type item =
  | Card of View_state.held_card
  | Deed of View_state.square

let items state (player : View_state.player) =
  List.map (fun card -> Card card) player.cards
  @ List.map (fun square -> Deed square) (deeds state player)

let item_index items index = max 0 (min index (List.length items - 1))

let flags state (p : View_state.player) =
  let turn = if p.id = state.View_state.current then [ "TURN" ] else [] in
  if p.bankrupt then turn @ [ "OUT" ]
  else if p.in_jail then turn @ [ "JAIL" ]
  else turn

let player_label state (p : View_state.player) =
  let token = View_board.token state p in
  [
    View_style.plain "[";
    token;
    View_style.plain "] ";
    View_style.piece token.style p.name;
  ]

let render ?selected (state : View_state.t) =
  let selected_index =
    List.find_index
      (fun (p : View_state.player) -> Some p.id = selected)
      state.players
    |> Option.value ~default:0
  in
  let start = selected_index / 8 * 8 in
  let section =
    if List.length state.players <= 8 then "PLAYERS"
    else Printf.sprintf "PLAYERS  %d/%d" (start + 1) (List.length state.players)
  in
  View_style.title section
  :: (state.players |> List.drop start |> List.take 8
     |> List.concat_map (fun (p : View_state.player) ->
         let mark = if selected = Some p.id then ">" else " " in
         let flags = flags state p in
         let status =
           match flags with
           | [] -> ""
           | flags -> "  " ^ String.concat " " flags
         in
         [
           View_style.plain mark
           :: (View_style.crop
                 (width - 1 - String.length status)
                 (player_label state p)
              @ [ View_style.plain status ]);
           line
             (Printf.sprintf "     %s  |  %s" (View_state.money p.cash)
                (quantity (List.length (deeds state p)) "deed"));
           line ("     " ^ View_state.square_name state p.position);
           [];
         ]))

let inventory ~heading ?selected state player =
  let body =
    match player with
    | None -> [ line "Player unavailable." ]
    | Some (p : View_state.player) ->
        let items = items state p in
        let total = List.length items in
        let page_size = 7 in
        let selected = Option.map (item_index items) selected in
        let start = Option.value selected ~default:0 / page_size * page_size in
        let visible =
          items |> List.drop start |> List.take page_size
          |> List.mapi (fun i item ->
              let mark = if selected = Some (start + i) then ">" else " " in
              let label =
                match item with
                | Card card -> "Card: " ^ card.title
                | Deed square -> "Deed: " ^ square.name
              in
              line (mark ^ label))
        in
        [
          player_label state p;
          line ("Cash: " ^ View_state.money p.cash);
          line
            (Printf.sprintf "Cards: %d  |  Deeds: %d" (List.length p.cards)
               (total - List.length p.cards));
        ]
        @ (if items = [] then [ line "No cards or deeds." ] else visible)
        @
        if total > page_size then
          [
            line
              (Printf.sprintf "Items %d-%d of %d" (start + 1)
                 (start + List.length visible)
                 total);
          ]
        else []
  in
  View_style.title (if Option.is_some selected then "> " ^ heading else heading)
  :: body

let group_name = function
  | View_state.Brown -> "Brown"
  | View_state.Light_blue -> "Light blue"
  | View_state.Pink -> "Pink"
  | View_state.Orange -> "Orange"
  | View_state.Red -> "Red"
  | View_state.Yellow -> "Yellow"
  | View_state.Green -> "Green"
  | View_state.Dark_blue -> "Dark blue"
  | View_state.Railroad -> "Railroad"
  | View_state.Utility -> "Utility"
  | View_state.None -> "Special square"

let development (square : View_state.square) =
  if square.hotel then "Hotel"
  else if square.houses > 0 then quantity square.houses "house"
  else "No improvements"

let tile_details state index =
  match View_state.square state index with
  | None -> [ line "Tile unavailable." ]
  | Some square ->
      let occupants =
        match View_board.occupants state index with
        | [] -> line "Occupants: None"
        | first :: rest ->
            (View_style.plain "Occupants: " :: player_label state first)
            @ List.concat_map
                (fun p -> View_style.plain ", " :: player_label state p)
                rest
      in
      let optional_money label = function
        | None -> []
        | Some value -> [ line (label ^ ": " ^ View_state.money value) ]
      in
      [ line ("Group: " ^ group_name square.group) ]
      @ (if square.group = View_state.None then []
         else
           optional_money "Price" square.price
           @ optional_money "Base rent" square.rent
           @ [
               line
                 ("Owner: "
                 ^ Option.fold ~none:"Unowned"
                     ~some:(View_state.player_name state)
                     square.owner);
               line ("Improvements: " ^ development square);
               line ("Mortgaged: " ^ if square.mortgaged then "Yes" else "No");
             ])
      @ [ []; occupants ]

let player_details state id =
  match View_state.player state id with
  | None -> [ line "Player unavailable." ]
  | Some p -> (
      let portfolio = deeds state p in
      let count predicate = List.length (List.filter predicate portfolio) in
      let status =
        match flags state p with
        | [] -> "IN PLAY"
        | tags -> String.concat " / " tags
      in
      [
        line ("Status: " ^ status);
        line ("Cash: " ^ View_state.money p.cash);
        line ("Position: " ^ View_state.square_name state p.position);
        [];
        View_style.title "PORTFOLIO";
        line
          (Printf.sprintf "%s  |  %s  |  %s  |  %d mortgaged"
             (quantity (List.length portfolio) "deed")
             (quantity
                (List.fold_left
                   (fun sum (s : View_state.square) ->
                     sum + if s.hotel then 0 else s.houses)
                   0 portfolio)
                "house")
             (quantity (count (fun s -> s.hotel)) "hotel")
             (count (fun s -> s.mortgaged)));
        [];
      ]
      @
      match portfolio with
      | [] -> [ line "No deeds." ]
      | squares ->
          List.map
            (fun (s : View_state.square) ->
              [
                View_style.piece
                  (View_style.of_group s.group)
                  (Printf.sprintf " %02d " s.index);
                View_style.plain
                  (Printf.sprintf "  %s  |  %s  |  %s%s" s.name
                     (group_name s.group) (development s)
                     (if s.mortgaged then "  |  Mortgaged" else ""));
              ])
            squares)
