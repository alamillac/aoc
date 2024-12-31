let file = Util.get_input_filename 7
(* let file = Util.get_test_filename 7 *)

let to_tuple = function
  | [ v1; v2 ] -> (v1, v2)
  | _ -> failwith "Argument out of range"

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

let split_calibration (s : string) : int * int list =
  let test, nums = s |> String.split_on_char ':' |> to_tuple in
  let nums =
    nums |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
  in
  (int_of_string test, nums)

let parse_line (s : string option) (acc : (int * int list) list) :
    (int * int list) list =
  match s with None -> acc | Some s -> split_calibration s :: acc

let is_valid_calibration ((test, nums) : int * int list) ops : bool =
  let rec aux nums op acc =
    match nums with
    | [] -> if acc = test then true else false
    | h :: t ->
        if acc = 0 then aux t op h
        else if acc > test then false
        else List.exists (fun next_op -> aux t next_op (op acc h)) ops
  in
  List.exists (fun next_op -> aux nums next_op 0) ops

let get_total_calibration (lst : (int * int list) list) ops : int =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        if is_valid_calibration h ops then aux t (acc + fst h) else aux t acc
  in
  aux lst 0

let concatenation (x : int) (y : int) : int =
  int_of_string (Int.to_string x ^ Int.to_string y)

let part1 () =
  let lst = read_lines file parse_line [] in
  let total_calibration = get_total_calibration lst [ ( + ); ( * ) ] in
  Printf.printf "Total calibration result: (%d)\n" total_calibration

let part2 () =
  let lst = read_lines file parse_line [] in
  let total_calibration_complete =
    get_total_calibration lst [ ( + ); ( * ); concatenation ]
  in
  Printf.printf "Total calibration result: (%d)\n" total_calibration_complete
