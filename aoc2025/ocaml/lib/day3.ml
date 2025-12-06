(* let file = Util.get_input_filename 3 *)

let file = Util.get_test_filename 3
let ascii_zero = int_of_char '0'
let to_int (c : char) : int = int_of_char c - ascii_zero

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

  let joltage (b : t) : int =
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

  let total_output_joltage (l : t list) : int =
    List.map joltage l |> List.fold_left ( + ) 0
end

let parse_line (s : string option) (acc : BatteryBank.t list) :
    BatteryBank.t list =
  match s with None -> acc | Some s -> BatteryBank.parse s :: acc

let part1 () =
  let bateries = Util.read_lines file parse_line in
  Printf.printf "Total output joltage: (%d)\n"
    (BatteryBank.total_output_joltage bateries)
