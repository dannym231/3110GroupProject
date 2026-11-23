type kind =
  | Go
  | Property of {
      group : string;
      price : int;
      rent : int;
    }
  | Railroad of { price : int }
  | Utility of { price : int }
  | Tax of { amount : int }
  | Chance
  | Community_chest
  | Jail
  | Free_parking
  | Go_to_jail

type square = {
  name : string;
  kind : kind;
}

type t = square array

exception Bad of string

let fail fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

let field name json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some v -> v
      | None -> fail "missing field %S" name)
  | _ -> fail "expected an object"

let to_str name json =
  match field name json with
  | `String s -> s
  | _ -> fail "field %S must be a string" name

let to_amount name json =
  match field name json with
  | `Int n when n >= 0 -> n
  | `Int _ -> fail "field %S must not be negative" name
  | _ -> fail "field %S must be an integer" name

let square_of_json i json =
  let name = to_str "name" json in
  let kind =
    match to_str "kind" json with
    | "go" -> Go
    | "property" ->
        Property
          {
            group = to_str "group" json;
            price = to_amount "price" json;
            rent = to_amount "rent" json;
          }
    | "railroad" -> Railroad { price = to_amount "price" json }
    | "utility" -> Utility { price = to_amount "price" json }
    | "tax" -> Tax { amount = to_amount "amount" json }
    | "chance" -> Chance
    | "community_chest" -> Community_chest
    | "jail" -> Jail
    | "free_parking" -> Free_parking
    | "go_to_jail" -> Go_to_jail
    | other -> fail "square %d: unknown kind %S" i other
  in
  { name; kind }

let of_string text =
  try
    let json = Yojson.Safe.from_string text in
    let squares =
      match field "squares" json with
      | `List l -> Array.of_list (List.mapi square_of_json l)
      | _ -> fail "field \"squares\" must be a list"
    in
    if Array.length squares = 0 then fail "board has no squares"
    else if squares.(0).kind <> Go then fail "square 0 must be Go"
    else Ok squares
  with
  | Bad msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON: " ^ msg)

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> of_string text
  | exception Sys_error msg -> Error msg

let size = Array.length
let square b i = b.(i)

let price sq =
  match sq.kind with
  | Property { price; _ } | Railroad { price } | Utility { price } -> Some price
  | _ -> None

let indices_where b pred =
  let acc = ref [] in
  for i = Array.length b - 1 downto 0 do
    if pred b.(i) then acc := i :: !acc
  done;
  !acc

let group_members b group =
  indices_where b (fun sq ->
      match sq.kind with
      | Property p -> p.group = group
      | _ -> false)

let count_owned b ~owner_of owner pred =
  indices_where b pred
  |> List.filter (fun i -> owner_of i = Some owner)
  |> List.length

let is_railroad sq =
  match sq.kind with
  | Railroad _ -> true
  | _ -> false

let is_utility sq =
  match sq.kind with
  | Utility _ -> true
  | _ -> false

let rent b ~owner_of ~dice_total i =
  match owner_of i with
  | None -> 0
  | Some owner -> (
      match b.(i).kind with
      | Property { group; rent; _ } ->
          let set = group_members b group in
          if List.for_all (fun j -> owner_of j = Some owner) set then 2 * rent
          else rent
      | Railroad _ ->
          let n = count_owned b ~owner_of owner is_railroad in
          25 * (1 lsl (n - 1))
      | Utility _ ->
          let n = count_owned b ~owner_of owner is_utility in
          (if n >= 2 then 10 else 4) * dice_total
      | _ -> 0)
