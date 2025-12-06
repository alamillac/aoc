let file = Util.get_input_filename 3
(* let file = Util.get_test_filename 3 *)

let ascii_zero = int_of_char '0'
let to_int (c : char) : int = int_of_char c - ascii_zero
let rec pow a n = if n = 0 then 1 else a * pow a (n - 1)

module BatteryBank = struct
  type t = int list

  let parse (s : string) : t =
    String.fold_left (fun acc c -> to_int c :: acc) [] s |> List.rev

  let pprint (b : t) : string = b |> List.map string_of_int |> String.concat ""

  let joltage_brute_force (b : t) : int =
    let rec aux (e : int) (b : int list) : int =
      match b with
      | [] -> 0
      | h1 :: tl ->
          let v = (e * 10) + h1 in
          let fut = aux e tl in
          let fut2 = aux h1 tl in
          max (max v fut) fut2
    in
    aux (List.hd b) (List.tl b)

  let joltage_2_bat (b : t) : int =
    let bat_bank = b |> Array.of_list in
    let length = Array.length bat_bank in
    let minus_last = Array.sub bat_bank 0 (length - 1) in
    let first = Array.fold_left max 0 minus_last in
    let first_pos =
      Array.find_index (fun x -> x = first) bat_bank |> Option.get
    in
    let reminder =
      Array.sub bat_bank (first_pos + 1) (length - first_pos - 1)
    in
    let second = Array.fold_left max 0 reminder in
    (first * 10) + second

  let find_max_bat (b : int array) (i : int) : int * int array =
    let length = Array.length b in
    let minus_lasts = Array.sub b 0 (length - i + 1) in
    let v = Array.fold_left max 0 minus_lasts in
    let v_pos = Array.find_index (fun x -> x = v) b |> Option.get in
    let reminder = Array.sub b (v_pos + 1) (length - v_pos - 1) in
    (v, reminder)

  let joltage_12_bat (b : t) : int =
    let bat_bank = b |> Array.of_list in
    let rec aux (i : int) (b : int array) : int =
      if i = 0 then 0
      else
        let v, reminder = find_max_bat b i in
        (v * pow 10 (i - 1)) + aux (i - 1) reminder
    in
    aux 12 bat_bank

  let total_output_joltage fn (l : t list) : int =
    List.map fn l |> List.fold_left ( + ) 0
end

let parse_line (s : string option) (acc : BatteryBank.t list) :
    BatteryBank.t list =
  match s with None -> acc | Some s -> BatteryBank.parse s :: acc

let part1 () =
  let bateries = Util.read_lines file parse_line in
  let total_output_joltage =
    BatteryBank.total_output_joltage BatteryBank.joltage_2_bat
  in
  Printf.printf "Total output joltage: (%d)\n" (total_output_joltage bateries)

let part2 () =
  let bateries = Util.read_lines file parse_line in
  let total_output_joltage =
    BatteryBank.total_output_joltage BatteryBank.joltage_12_bat
  in
  Printf.printf "Total output joltage: (%d)\n" (total_output_joltage bateries)
