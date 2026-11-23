type direction =
  | Up
  | Down
  | Left
  | Right

type key =
  | Arrow of direction
  | Enter
  | Escape
  | Tab
  | Letter of char

type focus =
  | Board
  | Roster
  | Inventory

type detail =
  | Tile of int
  | Player of View_state.player_id
  | Held_card of View_state.held_card

type t = {
  focus : focus;
  tile : int option;
  player : View_state.player_id option;
  local_player : View_state.player_id option;
  item_index : int;
  detail : detail option;
  scroll : int;
}

let initial ?local_player (state : View_state.t) =
  let active = View_state.player state state.current in
  let tile =
    match Option.bind active (fun p -> View_state.square state p.position) with
    | Some square -> Some square.index
    | None ->
        Option.map
          (fun (s : View_state.square) -> s.index)
          (List.nth_opt state.squares 0)
  in
  let player =
    Option.map
      (fun (p : View_state.player) -> p.id)
      (match active with
      | Some _ -> active
      | None -> List.nth_opt state.players 0)
  in
  {
    focus = Board;
    tile;
    player;
    local_player;
    item_index = 0;
    detail = None;
    scroll = 0;
  }

let inventory_player (state : View_state.t) ui =
  View_state.player state (Option.value ui.local_player ~default:state.current)

let inventory_items state ui =
  Option.fold ~none:[] ~some:(View_panel.items state)
    (inventory_player state ui)

let is_quit = function
  | Letter ('q' | 'Q') -> true
  | _ -> false

let command_of_key (state : View_state.t) key =
  let key = Char.lowercase_ascii key in
  match
    List.find_opt
      (fun (item : View_state.menu_item) -> Char.lowercase_ascii item.key = key)
      state.menu
  with
  | Some item -> Some item.command
  | None -> (
      match key with
      | 'n' | ']' -> Some View_state.Next
      | 'p' | '[' -> Some View_state.Prev
      | 'q' -> Some View_state.Quit
      | _ -> None)

let move_tile state selected direction =
  let dx, dy =
    match direction with
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)
  in
  match
    Option.bind selected (fun index -> View_board.neighbor index ~dx ~dy)
  with
  | Some neighbor when Option.is_some (View_state.square state neighbor) ->
      Some neighbor
  | _ -> selected

let move_player (state : View_state.t) selected step =
  let index =
    List.find_index
      (fun (p : View_state.player) -> Some p.id = selected)
      state.players
  in
  match index with
  | Some i when i + step >= 0 -> (
      match List.nth_opt state.players (i + step) with
      | Some p -> Some p.id
      | None -> selected)
  | _ -> selected

let update state ui ~scroll_limit key =
  if is_quit key then (ui, Some View_state.Quit)
  else
    let stay ui = (ui, None) in
    match (ui.detail, key) with
    | Some _, Escape -> stay { ui with detail = None; scroll = 0 }
    | Some _, Arrow ((Up | Down) as direction) ->
        let last = scroll_limit () in
        let step = if direction = Up then -1 else 1 in
        stay { ui with scroll = max 0 (min last (min last ui.scroll + step)) }
    | Some _, _ -> stay ui
    | None, Letter key -> (ui, command_of_key state key)
    | None, _ when not (View_screens.on_board state.View_state.screen) ->
        stay ui
    | None, Tab ->
        stay
          {
            ui with
            focus =
              (match ui.focus with
              | Board -> Roster
              | Roster -> Inventory
              | Inventory -> Board);
          }
    | None, Enter ->
        let detail =
          match ui.focus with
          | Board -> Option.map (fun i -> Tile i) ui.tile
          | Roster -> Option.map (fun id -> Player id) ui.player
          | Inventory ->
              let items = inventory_items state ui in
              List.nth_opt items (View_panel.item_index items ui.item_index)
              |> Option.map (function
                | View_panel.Card card -> Held_card card
                | View_panel.Deed square -> Tile square.index)
        in
        stay { ui with detail; scroll = 0 }
    | None, Arrow direction -> (
        match (ui.focus, direction) with
        | Board, _ -> stay { ui with tile = move_tile state ui.tile direction }
        | Roster, (Up | Down) ->
            let step = if direction = Up then -1 else 1 in
            stay { ui with player = move_player state ui.player step }
        | Inventory, (Up | Down) ->
            let items = inventory_items state ui in
            let step = if direction = Up then -1 else 1 in
            let index = View_panel.item_index items ui.item_index in
            stay
              {
                ui with
                item_index = View_panel.item_index items (index + step);
              }
        | _ -> stay ui)
    | None, Escape -> stay ui

let arrow = function
  | 'A' -> Some (Arrow Up)
  | 'B' -> Some (Arrow Down)
  | 'C' -> Some (Arrow Right)
  | 'D' -> Some (Arrow Left)
  | _ -> None

let decode ~read_more = function
  | '\r' | '\n' -> Some Enter
  | '\t' -> Some Tab
  | '\027' -> (
      let rec sequence plain =
        match read_more () with
        | Some '[' when plain -> sequence false
        | Some c when c >= '@' && c <= '~' -> if plain then arrow c else None
        | Some _ -> sequence false
        | None -> None
      in
      let rec string_escape previous =
        match read_more () with
        | None | Some '\007' -> None
        | Some '\\' when previous = '\027' -> None
        | Some c -> string_escape c
      in
      let rec intermediate () =
        match read_more () with
        | Some c when c >= ' ' && c <= '/' -> intermediate ()
        | _ -> None
      in
      match read_more () with
      | None -> Some Escape
      | Some ('[' | 'O') -> sequence true
      | Some (']' | 'P' | '^' | '_') -> string_escape '\000'
      | Some c when c >= ' ' && c <= '/' -> intermediate ()
      | Some _ -> None)
  | c when c >= ' ' && c < '\127' -> Some (Letter c)
  | _ -> None
