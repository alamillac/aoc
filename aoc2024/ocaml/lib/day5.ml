let file = Util.get_input_filename 5
(* let file = Util.get_test_filename 5 *)

module OrderType = struct
  type t = int * int

  let compare r1 r2 =
    let fcmp = fst r1 - fst r2 in
    if fcmp = 0 then snd r1 - snd r2 else fcmp
end

module RuleSet = Set.Make (OrderType)

let read_lines (name : string) : string list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | None ->
        close_in ic;
        List.rev acc
    | Some s -> loop (s :: acc)
  in
  loop []

let to_tuple = function
  | [ v1; v2 ] -> (v1, v2)
  | _ -> failwith "Argument out of range"

let parse_rule (s : string) : int * int =
  s |> String.split_on_char '|'
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string |> to_tuple

let parse_page (s : string) : int list =
  s |> String.split_on_char ','
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

let parse (lst : string list) : RuleSet.t * int list list =
  let rec aux lst is_rule (rules, pages) =
    match lst with
    | [] -> (rules, pages)
    | "" :: t -> aux t false (rules, pages)
    | h :: t ->
        if is_rule then aux t is_rule (RuleSet.add (parse_rule h) rules, pages)
        else aux t is_rule (rules, parse_page h :: pages)
  in
  aux lst true (RuleSet.empty, [])

let get_middle (lst : 'a list) : 'a =
  let pos = List.length lst / 2 in
  List.nth lst pos

let is_sorted_list (lst : int list) (rules : RuleSet.t) : bool =
  match lst with
  | [] -> failwith "List can't be empty"
  | h :: t ->
      let rec aux el lst =
        match lst with
        | [] -> true
        | h :: t ->
            if RuleSet.find_opt (h, el) rules = None then aux h t else false
      in
      aux h t

let apply_rules (rules : RuleSet.t) (pages : int list list) : int =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        if is_sorted_list h rules then aux t (acc + get_middle h) else aux t acc
  in
  aux pages 0

let sort_list (lst : int list) (rules : RuleSet.t) : int list =
  List.sort
    (fun a b ->
      if
        RuleSet.find_opt (a, b) rules = None
        && RuleSet.find_opt (b, a) rules = None
      then 0
      else if RuleSet.find_opt (b, a) rules <> None then 1
      else -1)
    lst

let apply_sorted_rules (rules : RuleSet.t) (pages : int list list) : int =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        if is_sorted_list h rules = false then
          let sorted_lst = sort_list h rules in
          aux t (acc + get_middle sorted_lst)
        else aux t acc
  in
  aux pages 0

let part1 () =
  let rules, pages = read_lines file |> parse in
  let sum_ordered_pages = apply_rules rules pages in
  Printf.printf "Sum correct: (%d)\n" sum_ordered_pages

let part2 () =
  let rules, pages = read_lines file |> parse in
  let sum_fixed_pages = apply_sorted_rules rules pages in
  Printf.printf "Sum fixed: (%d)\n" sum_fixed_pages
