let minimum_size = (120, View_board.height + 2)

let fits (columns, rows) =
  columns >= fst minimum_size && rows >= snd minimum_size

let line = View_style.line
let title = View_style.title

let header (state : View_state.t) =
  let last_roll =
    match state.last_dice with
    | None -> ""
    | Some d ->
        Printf.sprintf "  |  Last roll %d + %d = %d" d.die1 d.die2
          (d.die1 + d.die2)
  in
  title "MONOPOLY"
  @ line
      (Printf.sprintf "   Turn %02d  |  %s to play%s" state.turn
         (View_state.player_name state state.current)
         last_roll)

let menu (state : View_state.t) =
  state.menu
  |> List.filter_map (fun (item : View_state.menu_item) ->
      if item.command = View_state.Quit then None
      else
        Some
          (Printf.sprintf "[%c] %s" (Char.uppercase_ascii item.key) item.label))
  |> String.concat "   "

let recent (state : View_state.t) =
  state.log |> List.rev |> List.take 3 |> List.rev
  |> List.map (fun entry -> line ("Log: " ^ entry))

let preview_note = line "PREVIEW  |  Sample actions shown for illustration."

let wrap width doc =
  List.concat_map
    (fun row ->
      if View_style.line_width row <= width then [ row ]
      else View_style.text [ row ] |> View_style.wrap width |> List.map line)
    doc

let ring state (ui : View_input.t) ~preview =
  let prompt = View_screens.render state in
  let actions = menu state in
  let selection =
    match ui.tile with
    | None -> []
    | Some index ->
        [
          line
            (Printf.sprintf "Tile %02d  |  %s" index
               (View_state.square_name state index));
          [];
        ]
  in
  let center =
    [ [] ] @ selection
    @ (if preview then [ preview_note; [] ] else [])
    @ (if prompt = [] && actions = "" then []
       else
         (title "TURN ACTION" :: prompt)
         @ if actions = "" then [ [] ] else [ []; line actions; [] ])
    @ (title "RECENT ACTIVITY" :: recent state)
    |> wrap (View_board.center_width - 4)
    |> List.map (fun row -> View_style.plain "  " :: row)
  in
  let board =
    let selected = if ui.focus = View_input.Board then ui.tile else None in
    View_board.render ?selected ~center state
  in
  let sidebar =
    let selected = if ui.focus = View_input.Roster then ui.player else None in
    let roster = View_panel.render ?selected state in
    let heading =
      if Option.is_some ui.local_player then "YOUR ITEMS" else "CURRENT PLAYER"
    in
    let selected =
      if ui.focus = View_input.Inventory then Some ui.item_index else None
    in
    roster
    @ View_panel.inventory ~heading ?selected state
        (View_input.inventory_player state ui)
  in
  List.mapi
    (fun i row ->
      match List.nth_opt sidebar i with
      | None -> row
      | Some panel ->
          row @ (View_style.plain "  " :: View_style.crop View_panel.width panel))
    board

let inspector columns state = function
  | View_input.Tile index ->
      ( title "TILE"
        @ line
            (Printf.sprintf "   %02d  |  %s" index
               (View_state.square_name state index)),
        wrap (columns - 1) ([] :: View_panel.tile_details state index) )
  | Player id ->
      ( title "PLAYER" @ line ("   " ^ View_state.player_name state id),
        wrap (columns - 1) ([] :: View_panel.player_details state id) )
  | Held_card card ->
      ( title "HELD CARD" @ line ("   " ^ card.title),
        wrap (columns - 1)
          [
            [];
            line ("Deck: " ^ View_state.deck_name card.deck);
            [];
            line card.body;
          ] )

let scroll_limit ~size:(columns, rows) state (ui : View_input.t) =
  match ui.detail with
  | None -> 0
  | Some detail ->
      let _, body = inspector columns state detail in
      max 0 (List.length body - (rows - 2))

let page rows heading body footer =
  let visible = List.take (rows - 2) body in
  let gap = List.init (rows - 2 - List.length visible) (fun _ -> []) in
  (heading :: visible) @ gap @ [ footer ]

let render ~size:((columns, rows) as size) ~preview ~demo (ui : View_input.t)
    (state : View_state.t) =
  let doc =
    if not (fits size) then
      [
        title
          (Printf.sprintf "Resize terminal: need %dx%d (current %dx%d)"
             (fst minimum_size) (snd minimum_size) columns rows);
        line "[Q] Quit";
      ]
    else
      match ui.detail with
      | Some detail ->
          let heading, body = inspector columns state detail in
          let length = List.length body in
          let offset = min ui.scroll (max 0 (length - (rows - 2))) in
          let scroll =
            if length > rows - 2 then
              Printf.sprintf "[Up/Down] Scroll  %d-%d/%d   " (offset + 1)
                (offset + rows - 2)
                length
            else ""
          in
          page rows heading (List.drop offset body)
            (line (scroll ^ "[Esc] Back   [Q] Quit"))
      | None ->
          let browse = if demo then "[ / ] Browse demo   " else "" in
          let quit = "[Q] Quit" in
          if View_screens.on_board state.screen then
            page rows (header state) (ring state ui ~preview)
              (line
                 ("[Arrows] Move   [Tab] Board / players / items   [Enter] \
                   Inspect   " ^ browse ^ quit))
          else
            let body =
              [ [] ]
              @ (if preview then [ preview_note; [] ] else [])
              @ View_screens.render state
              @ [ []; line (menu state); [] ]
              @ recent state
              |> wrap (columns - 1)
            in
            page rows (title "MONOPOLY") body (line (browse ^ quit))
  in
  doc |> List.take (max 0 rows) |> List.map (View_style.crop (columns - 1))
