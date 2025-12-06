let file = Util.get_input_filename 2
(* let file = Util.get_test_filename 2 *)

module IDRange = struct
  type t = int * int

  let parse (s : string) : t =
    String.split_on_char '-' s |> function
    | [ s1; s2 ] -> (int_of_string s1, int_of_string s2)
    | _ -> failwith "Invalid ID range"

  let pprint (idr : t) : string = Printf.sprintf "%d-%d" (fst idr) (snd idr)

  let print_list (l : t list) : string =
    l |> List.map pprint |> String.concat "\n"

  let seq (idr : t) : int Seq.t =
    Seq.ints (fst idr) |> Seq.take (snd idr - fst idr + 1)

  let is_invalid_id (id : int) : bool =
    (* you can find the invalid IDs by looking for any ID which is made only of
     some sequence of digits repeated twice. So, 55 (5 twice), 6464 (64 twice),
     and 123123 (123 twice) would all be invalid IDs *)
    let str_id = Int.to_string id in
    let num_digits = String.length str_id in
    let has_pair_of_digits = num_digits mod 2 = 0 in
    if has_pair_of_digits then
      let half_num_digits = num_digits / 2 in
      let d1 = String.sub str_id 0 half_num_digits in
      let d2 = String.sub str_id half_num_digits half_num_digits in
      if d1 = d2 then true else false
    else false

  let find_invalid_ids (idr : t) : int list =
    let s = seq idr in
    Seq.filter is_invalid_id s |> List.of_seq
end

let parse_line (s : string option) (acc : IDRange.t list) : IDRange.t list =
  match s with
  | None -> acc
  | Some s ->
      String.split_on_char ',' s |> List.map IDRange.parse |> List.append acc

let part1 () =
  let id_ranges = Util.read_lines file parse_line in
  let invalid_ids =
    List.map IDRange.find_invalid_ids id_ranges |> List.flatten
  in
  let sum_invalid_ids = List.fold_left ( + ) 0 invalid_ids in
  Printf.printf "Invalid ids: (%d)\n" sum_invalid_ids
