type player = {
  id : string;
  name : string;
  cash : int;
  position : int;
}

type phase =
  | Awaiting_roll
  | Awaiting_buy of int
  | Awaiting_rent of {
      square : int;
      owner : string;
      amount : int;
    }

type t = {
  board : Board.t;
  players : player list;  (** in turn order *)
  current : int;  (** index into [players] *)
  turn : int;
  phase : phase;
  owners : (int * string) list;  (** square index -> owner id *)
  log : string list;  (** newest first *)
  last_dice : (int * int) option;
}

let starting_cash = 1500
let go_salary = 200

let create board ids =
  if List.length ids < 2 then invalid_arg "Game.create: need two players";
  let unique = List.sort_uniq compare (List.map fst ids) in
  if List.length unique <> List.length ids then
    invalid_arg "Game.create: duplicate player id";
  let players =
    List.map
      (fun (id, name) -> { id; name; cash = starting_cash; position = 0 })
      ids
  in
  let first = List.hd players in
  {
    board;
    players;
    current = 0;
    turn = 1;
    phase = Awaiting_roll;
    owners = [];
    log = [ first.name ^ " to roll." ];
    last_dice = None;
  }

let board g = g.board
let players g = g.players
let current g = List.nth g.players g.current
let turn g = g.turn
let phase g = g.phase
let owner g i = List.assoc_opt i g.owners
let last_dice g = g.last_dice
let log g = List.rev g.log
let note msg g = { g with log = msg :: g.log }
let name_of g id = (List.find (fun p -> p.id = id) g.players).name

let update_player id f g =
  { g with players = List.map (fun p -> if p.id = id then f p else p) g.players }

let add_cash id amount =
  update_player id (fun p -> { p with cash = p.cash + amount })

let square_name g i = (Board.square g.board i).name

let end_turn g =
  let next = (g.current + 1) mod List.length g.players in
  let g = { g with current = next; turn = g.turn + 1; phase = Awaiting_roll } in
  note ((current g).name ^ " to roll.") g

let resolve_landing g =
  let p = current g in
  let i = p.position in
  let sq = Board.square g.board i in
  match Board.price sq with
  | Some _ -> (
      match owner g i with
      | None -> { g with phase = Awaiting_buy i }
      | Some o when o = p.id ->
          end_turn (note (p.name ^ " owns " ^ sq.name ^ " already.") g)
      | Some o ->
          let dice_total =
            match g.last_dice with
            | Some (a, b) -> a + b
            | None -> 0
          in
          let amount = Board.rent g.board ~owner_of:(owner g) ~dice_total i in
          let g =
            note
              (Printf.sprintf "%s owes %s $%d rent." p.name (name_of g o) amount)
              g
          in
          { g with phase = Awaiting_rent { square = i; owner = o; amount } })
  | None ->
      let msg =
        match sq.kind with
        | Board.Tax _ -> Some "Taxes come in MS3."
        | Board.Chance | Board.Community_chest -> Some "Cards come in MS3."
        | Board.Go_to_jail -> Some "Jail comes in MS3."
        | _ -> None
      in
      let g = match msg with Some m -> note m g | None -> g in
      end_turn g

let roll (d1, d2) g =
  if g.phase <> Awaiting_roll then Error "You can't roll right now."
  else if d1 < 1 || d1 > 6 || d2 < 1 || d2 > 6 then
    Error "Dice must be between 1 and 6."
  else
    let p = current g in
    let size = Board.size g.board in
    let raw = p.position + d1 + d2 in
    let position = raw mod size in
    let g = update_player p.id (fun p -> { p with position }) g in
    let g = { g with last_dice = Some (d1, d2) } in
    let g =
      note
        (Printf.sprintf "%s rolled %d + %d and landed on %s." p.name d1 d2
           (square_name g position))
        g
    in
    let g =
      if raw >= size then
        add_cash p.id go_salary g
        |> note
             (Printf.sprintf "%s collected $%d for passing Go." p.name go_salary)
      else g
    in
    Ok (resolve_landing g)

let buy g =
  match g.phase with
  | Awaiting_buy i ->
      let p = current g in
      let sq = Board.square g.board i in
      let price = Option.get (Board.price sq) in
      if p.cash < price then
        Error (Printf.sprintf "%s can't afford %s." p.name sq.name)
      else
        let g = add_cash p.id (-price) g in
        let g = { g with owners = (i, p.id) :: g.owners } in
        let g =
          note (Printf.sprintf "%s bought %s for $%d." p.name sq.name price) g
        in
        Ok (end_turn g)
  | _ -> Error "There's nothing to buy right now."

let pass g =
  match g.phase with
  | Awaiting_buy i ->
      let p = current g in
      Ok (end_turn (note (p.name ^ " passed on " ^ square_name g i ^ ".") g))
  | _ -> Error "There's nothing to pass on right now."

let pay g =
  match g.phase with
  | Awaiting_rent { owner; amount; _ } ->
      let p = current g in
      let g = add_cash p.id (-amount) g |> add_cash owner amount in
      let g =
        note (Printf.sprintf "%s paid %s $%d." p.name (name_of g owner) amount) g
      in
      Ok (end_turn g)
  | _ -> Error "There's no rent to pay right now."