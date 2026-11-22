type t = {
  fg : ANSITerminal.color option;
  bg : ANSITerminal.color option;
  bold : bool;
  inverse : bool;
}

type piece = {
  style : t;
  text : string;
  columns : int;
}

type line = piece list
type doc = line list

let none = { fg = None; bg = None; bold = false; inverse = false }
let heading = { none with fg = Some ANSITerminal.Cyan; bold = true }
let bold = { none with bold = true }
let selected = { none with inverse = true }

let of_group = function
  | View_state.Brown ->
      { none with fg = Some ANSITerminal.Black; bg = Some ANSITerminal.Yellow }
  | View_state.Light_blue ->
      { none with fg = Some ANSITerminal.Black; bg = Some ANSITerminal.Cyan }
  | View_state.Pink ->
      { none with fg = Some ANSITerminal.Black; bg = Some ANSITerminal.Magenta }
  | View_state.Orange ->
      { none with fg = Some ANSITerminal.White; bg = Some ANSITerminal.Red }
  | View_state.Red ->
      { bold with fg = Some ANSITerminal.White; bg = Some ANSITerminal.Red }
  | View_state.Yellow ->
      { bold with fg = Some ANSITerminal.Black; bg = Some ANSITerminal.Yellow }
  | View_state.Green ->
      { none with fg = Some ANSITerminal.Black; bg = Some ANSITerminal.Green }
  | View_state.Dark_blue ->
      { none with fg = Some ANSITerminal.White; bg = Some ANSITerminal.Blue }
  | View_state.Railroad ->
      { bold with fg = Some ANSITerminal.White; bg = Some ANSITerminal.Black }
  | View_state.Utility ->
      { none with fg = Some ANSITerminal.Black; bg = Some ANSITerminal.White }
  | View_state.None -> none

let piece ?columns style text =
  let text =
    String.map (fun c -> if c < ' ' || c = '\127' then ' ' else c) text
  in
  { style; text; columns = Option.value columns ~default:(String.length text) }

let plain text = piece none text
let title text = [ piece heading text ]
let line text = [ plain text ]

(* shortcut: count UTF-8 bytes to stay within terminal width; use display widths
   if localized boards need to align wide or combining glyphs. *)
let clip width text =
  let rec boundary i =
    if i = 0 || Char.code text.[i] land 0xc0 <> 0x80 then i else boundary (i - 1)
  in
  if width <= 0 then ""
  else if String.length text <= width then text
  else String.sub text 0 (boundary width)

let fit width text =
  let text = clip width text in
  text ^ String.make (width - String.length text) ' '

let line_width = List.fold_left (fun n p -> n + p.columns) 0

let rec crop width = function
  | [] -> []
  | _ when width <= 0 -> []
  | p :: rest ->
      if p.columns <= width then p :: crop (width - p.columns) rest
      else if p.columns = String.length p.text then
        [ piece p.style (clip width p.text) ]
      else []

let pad width line =
  let line = crop width line in
  let gap = width - line_width line in
  if gap = 0 then line else line @ [ plain (String.make gap ' ') ]

let wrap width text =
  let rec go text =
    if String.length text <= width then [ text ]
    else
      let prefix = clip width text in
      let split =
        match String.rindex_opt prefix ' ' with
        | Some i when i > 0 -> i
        | _ -> String.length prefix
      in
      String.sub text 0 split
      :: go (String.trim (String.sub text split (String.length text - split)))
  in
  (* Callers use this for the board center or the full inspector. *)
  go (String.trim text)

let text doc =
  String.concat "\n"
    (List.map
       (fun line -> String.concat "" (List.map (fun p -> p.text) line))
       doc)

let ansi_of style =
  (if style.bold then [ ANSITerminal.Bold ] else [])
  @ (match style.fg with
    | None -> []
    | Some c -> [ ANSITerminal.Foreground c ])
  @
  match style.bg with
  | None -> []
  | Some c -> [ ANSITerminal.Background c ]

let show doc =
  let frame = Buffer.create 4096 in
  let color = Sys.getenv_opt "NO_COLOR" = None in
  Buffer.add_string frame "\027[H";
  List.iteri
    (fun i line ->
      if i > 0 then Buffer.add_string frame "\r\n";
      List.iter
        (fun p ->
          let styles =
            if p.style.inverse then [ ANSITerminal.Inverse ]
            else if color then ansi_of p.style
            else []
          in
          let text =
            match styles with
            | [] -> p.text
            | _ -> ANSITerminal.sprintf styles "%s" p.text
          in
          Buffer.add_string frame text)
        line;
      Buffer.add_string frame "\027[K")
    doc;
  Buffer.add_string frame "\027[J";
  Buffer.output_buffer stdout frame;
  flush stdout
