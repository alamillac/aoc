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
