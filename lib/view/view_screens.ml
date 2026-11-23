let line = View_style.line

let on_board = function
  | View_state.Host_join _ | View_state.Winner _ -> false
  | _ -> true

let names (state : View_state.t) indexes =
  match indexes with
  | [] -> "nothing"
  | _ -> String.concat ", " (List.map (View_state.square_name state) indexes)

let render (state : View_state.t) =
  match state.View_state.screen with
  | Overview -> []
  | Buy_or_pass { square; price; rent } ->
      [
        line ("Buy " ^ View_state.square_name state square ^ "?");
        line
          (Printf.sprintf "Price %s  Rent %s  %s has %s"
             (View_state.money price) (View_state.money rent)
             (View_state.player_name state state.current)
             (View_state.money
                (match View_state.player state state.current with
                | Some p -> p.cash
                | None -> 0)));
      ]
  | Payment { kind; payer; amount; payee; square } ->
      let kind_name =
        match kind with
        | View_state.Rent -> "rent"
        | View_state.Tax -> "tax"
      in
      let dest =
        match payee with
        | None -> ""
        | Some id -> " " ^ View_state.player_name state id
      in
      let where =
        match square with
        | None -> ""
        | Some index -> " for " ^ View_state.square_name state index
      in
      [
        line
          (Printf.sprintf "%s owes%s %s %s%s."
             (View_state.player_name state payer)
             dest (View_state.money amount) kind_name where);
      ]
  | Card { deck; body } -> [ line (View_state.deck_name deck); line body ]
  | Jail { player; turns_left; fine; has_card } ->
      [
        line
          (Printf.sprintf
             "%s is in jail. %d turns left. Fine %s. Get Out of Jail card: %s"
             (View_state.player_name state player)
             turns_left (View_state.money fine)
             (if has_card then "yes" else "no"));
      ]
  | Build { options } ->
      line "Build"
      :: List.map
           (fun (opt : View_state.build_option) ->
             line
               (Printf.sprintf "%s  houses %d  cost %s"
                  (View_state.square_name state opt.square)
                  opt.houses
                  (View_state.money opt.cost)))
           options
  | Mortgage { options } ->
      line "Mortgage"
      :: List.map
           (fun (opt : View_state.mortgage_option) ->
             line
               (Printf.sprintf "%s  value %s  %s"
                  (View_state.square_name state opt.square)
                  (View_state.money opt.value)
                  (if opt.mortgaged then "mortgaged" else "open")))
           options
  | Trade { offer; request } ->
      [
        line
          (Printf.sprintf "%s offers %s and %s"
             (View_state.player_name state offer.player)
             (View_state.money offer.cash)
             (names state offer.squares));
        line
          (Printf.sprintf "for %s's %s and %s"
             (View_state.player_name state request.player)
             (View_state.money request.cash)
             (names state request.squares));
      ]
  | Host_join { host; port } ->
      [
        line "Host or join";
        line (Printf.sprintf "Address %s  port %d" host port);
        line "preview only, no socket opened";
      ]
  | Bankruptcy { player; reason } ->
      let still =
        state.players
        |> List.filter (fun (p : View_state.player) ->
            (not p.bankrupt) && p.id <> player)
        |> List.map (fun (p : View_state.player) -> p.name)
        |> String.concat ", "
      in
      [
        line
          (Printf.sprintf "%s is bankrupt: %s"
             (View_state.player_name state player)
             reason);
        line ("Still in: " ^ still);
      ]
  | Winner { player } ->
      line (View_state.player_name state player ^ " wins")
      :: List.map
           (fun (p : View_state.player) ->
             line (p.name ^ " " ^ View_state.money p.cash))
           state.players
