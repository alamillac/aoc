let file = Util.get_input_filename 11
(* let file = Util.get_test_filename 11 *)

let to_int (txt : string) : int list =
  String.split_on_char ' ' txt |> List.map int_of_string

let read_file (name : string) : int list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | None ->
        close_in ic;
        List.rev acc
    | Some s -> loop (to_int s :: acc)
  in
  loop [] |> List.flatten

let has_even_digits (stone : int) : bool =
  let stone_length = Int.to_string stone |> String.length in
  stone_length mod 2 = 0

let split_stone (stone : int) : int list =
  let str_stone = Int.to_string stone in
  let stone_length = String.length str_stone in
  let new_length = stone_length / 2 in
  let str_stone1 = String.sub str_stone 0 new_length in
  let str_stone2 = String.sub str_stone new_length new_length in
  [ int_of_string str_stone1; int_of_string str_stone2 ]

let apply_rule (stone : int) : int list =
  match stone with
  | 0 -> [ 1 ]
  | s when has_even_digits s -> split_stone s
  | s -> [ 2024 * s ]

let rec stones_after_blink (stone, iter) =
  match iter with
  | i when i <= 0 -> failwith "Invalid iter"
  | 1 -> apply_rule stone |> List.length
  | i -> (
      let gen_stones = apply_rule stone in
      match gen_stones with
      | s :: [] -> stones_after_blink (s, i - 1)
      | [ s1; s2 ] ->
          stones_after_blink (s1, i - 1) + stones_after_blink (s2, i - 1)
      | _ -> failwith "Invalid result")

let stones_after_blink_memo =
  let stones_after_blink self (stone, iter) =
    match iter with
    | i when i <= 0 -> failwith "Invalid iter"
    | 1 -> apply_rule stone |> List.length
    | i -> (
        let gen_stones = apply_rule stone in
        match gen_stones with
        | s :: [] -> self (s, i - 1)
        | [ s1; s2 ] -> self (s1, i - 1) + self (s2, i - 1)
        | _ -> failwith "Invalid result")
  in
  Util.memo_rec stones_after_blink

let get_num_stones stones iter fn =
  List.fold_left (fun acc s -> acc + fn (s, iter)) 0 stones

let part1 () =
  let stones = read_file file in
  let num_stones = get_num_stones stones 25 stones_after_blink in
  Printf.printf "Number of stones for 25: %d\n" num_stones

let part2 () =
  let stones = read_file file in
  let num_stones = get_num_stones stones 75 stones_after_blink_memo in
  Printf.printf "Number of stones for 75: %d\n" num_stones
