let file = Util.get_input_filename 2

(* let print_list lst = *)
(*   List.iter (Printf.printf "%d ") lst; *)
(*   Printf.printf "\n" *)

let split (txt : string) : int list =
  txt |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

let read_lines (name : string) fn (ac : 'a) : 'a =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | None ->
        close_in ic;
        fn None acc
    | s -> loop (fn s acc)
  in
  loop ac

let parse_line (s : string option) (acc : 'a list) : 'a list =
  match s with None -> acc | Some s -> split s :: acc

let rec is_safe_aux (lst : int list) (prev : int) (prev2 : int) (is_dec : bool)
    (can_fail : bool) : bool =
  match lst with
  | [] -> true
  | h :: t -> (
      match is_dec with
      | true ->
          if h < prev && prev - h <= 3 then is_safe_aux t h prev is_dec can_fail
          else if can_fail then
            is_safe_aux lst prev2 0 is_dec false
            || is_safe_aux (prev :: t) prev2 0 is_dec false
          else false
      | false ->
          if h > prev && h - prev <= 3 then is_safe_aux t h prev is_dec can_fail
          else if can_fail then
            is_safe_aux lst prev2 0 is_dec false
            || is_safe_aux (prev :: t) prev2 0 is_dec false
          else false)

let is_safe = function
  | e1 :: e2 :: t ->
      if e1 = e2 || Int.abs (e1 - e2) > 3 then false
      else is_safe_aux t e2 e1 (e1 > e2) false
  | [] -> failwith "List can't be empty"
  | _ -> failwith "Invalid number of elements"

let is_safe_dampener = function
  | e1 :: e2 :: t ->
      if e1 = e2 || Int.abs (e1 - e2) > 3 then
        is_safe (e2 :: t) || is_safe (e1 :: t)
      else if is_safe_aux t e2 e1 (e1 > e2) true then true
      else is_safe (e2 :: t) || is_safe (e1 :: t)
  | [] -> failwith "List can't be empty"
  | _ -> failwith "Invalid number of elements"

let rec count_safe_aux (lst : int list list) (acc : int) is_safe_fn : int =
  match lst with
  | [] -> acc
  | h :: t ->
      let ac = (if is_safe_fn h then 1 else 0) + acc in
      count_safe_aux t ac is_safe_fn

let count_safe lst = count_safe_aux lst 0 is_safe
let count_safe_dampener lst = count_safe_aux lst 0 is_safe_dampener

let part1 () =
  let lst = read_lines file parse_line [] in
  let report_safes = count_safe lst in
  Printf.printf "Report safes: (%d)\n" report_safes

let part2 () =
  let lst = read_lines file parse_line [] in
  let report_safes_dampener = count_safe_dampener lst in
  Printf.printf "Report safes Dampener: (%d)\n" report_safes_dampener
