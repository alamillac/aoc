let file = Util.get_input_filename 5
(* let file = Util.get_test_filename 5 *)

module Inventory = struct
  type id_range = int * int
  type t = { fresh_ids : id_range list; available_ids : int list }

  let add_fresh_range (inventory : t) (range : id_range) : t =
    { inventory with fresh_ids = range :: inventory.fresh_ids }

  let add_available_id (inventory : t) (id : int) : t =
    { inventory with available_ids = id :: inventory.available_ids }

  let init () : t = { fresh_ids = []; available_ids = [] }

  let pprint (inventory : t) : string =
    List.map string_of_int inventory.available_ids |> String.concat "\n"

  let is_fresh (inventory : t) (id : int) : bool =
    List.exists
      (fun range -> id >= fst range && id <= snd range)
      inventory.fresh_ids

  let fresh_available (inventory : t) : int =
    List.fold_left
      (fun acc id -> if is_fresh inventory id then acc + 1 else acc)
      0 inventory.available_ids

  let join_ranges (range1 : id_range) (range2 : id_range) : id_range option =
    let join (x1, x2) (y1, y2) =
      if y1 >= x1 && y1 <= x2 then
        if y2 > x2 then Some (x1, y2) else Some (x1, x2)
      else None
    in
    let new_range = join range1 range2 in
    match new_range with
    | None -> join range2 range1
    | Some new_range -> Some new_range

  let fresh_total (inventory : t) : int =
    let rec aux (range : id_range) (ranges : id_range list)
        (acc : id_range list) : id_range list =
      match ranges with
      | [] -> range :: acc
      | h :: tl -> (
          let new_range = join_ranges range h in
          match new_range with
          | None -> aux range tl (h :: acc)
          | Some new_range -> aux new_range (List.append tl acc) [])
    in
    let unique_ranges =
      List.fold_left (fun acc range -> aux range acc []) [] inventory.fresh_ids
    in
    unique_ranges |> List.fold_left (fun acc (x1, x2) -> acc + x2 - x1 + 1) 0
end

module InventoryParser = struct
  type parser_state = ParseFresh | ParseAvailable
  type t = parser_state * Inventory.t

  let init () : t = (ParseFresh, Inventory.init ())

  let parse_line (s : string option) (acc : t) : t =
    match s with
    | None -> acc
    | Some "" ->
        let _, inventory = acc in
        (ParseAvailable, inventory)
    | Some s -> (
        let state, inventory = acc in
        match state with
        | ParseFresh ->
            let range =
              String.split_on_char '-' s |> function
              | [ r1; r2 ] -> (int_of_string r1, int_of_string r2)
              | _ -> failwith "Invalid range"
            in
            (ParseFresh, Inventory.add_fresh_range inventory range)
        | ParseAvailable ->
            let id = int_of_string s in
            (ParseAvailable, Inventory.add_available_id inventory id))

  let parse (file : string) : Inventory.t =
    let new_parser = init () in
    let res = Util.parse_lines file parse_line new_parser in
    let _, inventory = res in
    inventory
end

let parse_line (s : string option) (acc : string list) : string list =
  match s with None -> acc | Some s -> s :: acc

let part1 () =
  let inventory = InventoryParser.parse file in
  Printf.printf "Num fresh available ingredients: (%d)\n"
    (Inventory.fresh_available inventory)

let part2 () =
  let inventory = InventoryParser.parse file in
  Printf.printf "Num fresh total ingredients: (%d)\n"
    (Inventory.fresh_total inventory)
