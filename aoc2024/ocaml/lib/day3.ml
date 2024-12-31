let file = Util.get_input_filename 3

(* let print_list lst = *)
(*   List.iter print_endline lst; *)
(*   lst *)

let multiply = function
  | [ v1; v2 ] -> v1 * v2
  | _ -> failwith "Argument out of range"

let concat = List.fold_left ( ^ ) ""

let read_lines (name : string) : string =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop (acc : string list) : string list =
    match try_read () with
    | None ->
        close_in ic;
        List.rev acc
    | Some s -> loop (s :: acc)
  in
  loop [] |> concat

let find_all_matches regex text =
  let re = Re.Str.regexp regex in
  let rec aux pos acc =
    try
      let start = Re.Str.search_forward re text pos in
      let matched = Re.Str.matched_string text in
      aux (start + 1) (matched :: acc)
    with Not_found -> List.rev acc
  in
  aux 0 []

let find_digits text =
  let re = Re.Str.regexp "[mul()]" in
  Re.Str.global_replace re "" text
  |> String.split_on_char ','
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string |> multiply

let rec filter_results_aux (lst : string list) (filter : bool)
    (acc : string list) : string list =
  match lst with
  | [] -> acc
  | h :: t -> (
      match h with
      | "do()" -> filter_results_aux t false acc
      | "don't()" -> filter_results_aux t true acc
      | x ->
          if filter then filter_results_aux t filter acc
          else filter_results_aux t filter (x :: acc))

let filter_results lst = filter_results_aux lst false []
let rec sum = function [] -> 0 | h :: t -> h + sum t

let part1 () =
  let text = read_lines file in
  let matches =
    find_all_matches "mul([0-9]+,[0-9]+)" text |> List.map find_digits
  in
  let results = sum matches in
  Printf.printf "Results: (%d)\n" results

let part2 () =
  let text = read_lines file in
  let matches_filtered =
    find_all_matches "mul([0-9]+,[0-9]+)\\|do()\\|don't()" text
    |> filter_results
    (* |> List.map Util.print_return *)
    |> List.map find_digits
  in
  let results_filtered = sum matches_filtered in
  Printf.printf "Filtered results: (%d)\n" results_filtered
