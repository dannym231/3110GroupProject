let width = 7
let edge = 10
let center_width = ((edge - 1) * (width + 1)) - 1
let height = ((edge + 1) * 4) + 1

let at row col =
  if row < 0 || row > edge || col < 0 || col > edge then None
  else if row = 0 then Some (20 + col)
  else if row = edge then Some (10 - col)
  else if col = 0 then Some (20 - row)
  else if col = edge then Some (30 + row)
  else None

let positions =
  List.init (edge + 1) (fun row ->
      List.init (edge + 1) (fun col ->
          Option.map (fun index -> (index, (row, col))) (at row col)))
  |> List.concat |> List.filter_map Fun.id

let neighbor index ~dx ~dy =
  Option.bind (List.assoc_opt index positions) (fun (row, col) ->
      at (row + dy) (col + dx))

let abbrev name =
  match name with
  | "Go" -> "Go"
  | "Jail" -> "Jail"
  | "Free Parking" -> "Free P"
  | "Go to Jail" -> "G2J"
  | "Mediterranean Avenue" -> "Med"
  | "Community Chest" -> "Chest"
  | "Baltic Avenue" -> "Baltic"
  | "Income Tax" -> "IncTax"
  | "Reading Railroad" -> "ReadRR"
  | "Oriental Avenue" -> "Orient"
  | "Chance" -> "Chance"
  | "Vermont Avenue" -> "Vermnt"
  | "Connecticut Avenue" -> "Conn"
  | "St. Charles Place" -> "StCh"
  | "Electric Company" -> "Electr"
  | "States Avenue" -> "States"
  | "Virginia Avenue" -> "Virgin"
  | "Pennsylvania Railroad" -> "PennRR"
  | "St. James Place" -> "StJa"
  | "Tennessee Avenue" -> "Tenn"
  | "New York Avenue" -> "NewYrk"
  | "Kentucky Avenue" -> "Kentky"
  | "Indiana Avenue" -> "Ind"
  | "Illinois Avenue" -> "Ill"
  | "B. & O. Railroad" -> "B&O RR"
  | "Atlantic Avenue" -> "Atlant"
  | "Ventnor Avenue" -> "Ventnr"
  | "Water Works" -> "Water"
  | "Marvin Gardens" -> "Marv"
  | "Pacific Avenue" -> "Pacifc"
  | "North Carolina Avenue" -> "N Car"
  | "Pennsylvania Avenue" -> "Penn"
  | "Short Line" -> "ShortL"
  | "Park Place" -> "ParkPl"
  | "Luxury Tax" -> "LuxTax"
  | "Boardwalk" -> "Bwalk"
  | other -> other

let fallback_token (player : View_state.player) =
  if player.token = "" || player.token.[0] <= ' ' || player.token.[0] >= '\127'
  then "?"
  else String.sub player.token 0 1

let symbols = [| ("꩜", 1); ("ଓ", 1); ("ᨳ", 1); ("🐇", 2); ("ᡣ", 1) |]

let colors =
  let open ANSITerminal in
  [|
    (Black, Cyan);
    (Black, Yellow);
    (White, Magenta);
    (Black, Green);
    (White, Blue);
    (White, Red);
    (Black, White);
    (White, Black);
  |]

let token (state : View_state.t) (player : View_state.player) =
  let index =
    List.find_index
      (fun (p : View_state.player) -> p.id = player.id)
      state.players
    |> Option.value ~default:0
  in
  let glyph, columns =
    if index < Array.length symbols then symbols.(index)
    else (fallback_token player, 1)
  in
  let fg, bg = colors.(index mod Array.length colors) in
  View_style.piece ~columns
    { View_style.bold with fg = Some fg; bg = Some bg }
    glyph

let mark (sq : View_state.square) =
  if sq.mortgaged then "*"
  else if sq.hotel then "H"
  else if sq.houses > 0 then string_of_int (min 9 sq.houses)
  else ""

let occupants (state : View_state.t) index =
  List.filter
    (fun (p : View_state.player) -> p.position = index && not p.bankrupt)
    state.players

let cell state selected index =
  let cell_style style =
    if selected = Some index then View_style.selected else style
  in
  let fill = cell_style View_style.none in
  let row style text =
    [ View_style.piece (cell_style style) (View_style.fit width text) ]
  in
  let pad parts =
    let extra = width - View_style.line_width parts in
    if extra = 0 then parts
    else parts @ [ View_style.piece fill (String.make extra ' ') ]
  in
  let label name style = row style (" " ^ abbrev name) in
  let info = row View_style.none in
  match View_state.square state index with
  | None ->
      (label ("sq " ^ string_of_int index) View_style.none, info "", info "")
  | Some sq ->
      let pawns = List.map (token state) (occupants state index) in
      let pawns =
        if View_style.line_width pawns <= width then pawns
        else View_style.crop (width - 1) pawns @ [ View_style.piece fill "+" ]
      in
      let stats =
        match (sq.owner, sq.price) with
        | None, Some price -> info (View_state.money price)
        | None, None -> info ""
        | Some id, _ ->
            let owner =
              match View_state.player state id with
              | Some p -> token state p
              | None -> View_style.piece fill "?"
            in
            pad [ owner; View_style.piece fill (" " ^ mark sq) ]
      in
      (label sq.name (View_style.of_group sq.group), stats, pad pawns)

let render ?selected ?(center = []) state =
  let cell_at row col = cell state selected (Option.get (at row col)) in
  let bar = View_style.plain "|" in
  let dash = String.make width '-' in
  let divider = View_style.plain ("+" ^ dash ^ "+") in
  let full_sep =
    View_style.line
      ("+" ^ String.concat "+" (List.init (edge + 1) (fun _ -> dash)) ^ "+")
  in
  let full_row row =
    let cells = List.init (edge + 1) (cell_at row) in
    let join field =
      List.concat_map (fun cell -> bar :: field cell) cells @ [ bar ]
    in
    [
      join (fun (name, _, _) -> name);
      join (fun (_, info, _) -> info);
      join (fun (_, _, pawns) -> pawns);
    ]
  in
  let take = function
    | line :: rest -> (View_style.pad center_width line, rest)
    | [] -> (View_style.pad center_width [], [])
  in
  let rec sides row center =
    if row = edge then []
    else
      let left_name, left_info, left_pawns = cell_at row 0 in
      let right_name, right_info, right_pawns = cell_at row edge in
      let first, center = take center in
      let second, center = take center in
      let third, center = take center in
      let fourth, center = take center in
      let side l mid r = (bar :: l) @ (bar :: mid) @ (bar :: r) @ [ bar ] in
      side left_name first right_name
      :: side left_info second right_info
      :: side left_pawns third right_pawns
      :: ((divider :: fourth) @ [ divider ])
      :: sides (row + 1) center
  in
  (full_sep :: full_row 0)
  @ (full_sep :: sides 1 center)
  @ full_row edge @ [ full_sep ]
