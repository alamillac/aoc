let file = Util.get_input_filename 9
(* let file = Util.get_test_filename 9 *)

let int_value (c : char) : int = int_of_char c - int_of_char '0'

let string_to_int_list s =
  s |> String.to_seq |> Seq.map int_value |> List.of_seq

let read_lines (name : string) : int list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | None ->
        close_in ic;
        List.rev acc
    | Some s -> loop (string_to_int_list s :: acc)
  in
  loop [] |> List.flatten

let rec add_to_list (v : int option) (len : int) (lst : int option list) :
    int option list =
  if len <= 0 then lst else add_to_list v (len - 1) (v :: lst)

let expand_disk (disk_map : int list) : int option list =
  let rec aux l acc next_id is_space =
    match l with
    | [] -> acc
    | h :: t ->
        if is_space then aux t (add_to_list None h acc) next_id false
        else aux t (add_to_list (Some next_id) h acc) (next_id + 1) true
  in
  aux disk_map [] 0 false |> List.rev

let print_disk (disk : int option list) =
  List.iter
    (fun el ->
      match el with None -> Printf.printf "." | Some e -> Printf.printf "%d" e)
    disk;
  Printf.printf "\n"

let sort_blocks (disk : int option list) : int option list =
  let disk_arr = Array.of_list disk in
  let rec aux left_idx right_idx =
    if left_idx = right_idx then Array.to_list disk_arr
    else
      match (disk_arr.(left_idx), disk_arr.(right_idx)) with
      | _, None -> aux left_idx (right_idx - 1)
      | Some _, _ -> aux (left_idx + 1) right_idx
      | None, Some e ->
          disk_arr.(left_idx) <- Some e;
          disk_arr.(right_idx) <- None;
          aux (left_idx + 1) (right_idx - 1)
  in
  aux 0 (Array.length disk_arr - 1)

let extract_info (disk_map : int list) : int list * (int * int) list =
  let rec aux lst spaces_acc idx_acc next_id is_space =
    match lst with
    | [] -> (List.rev spaces_acc, List.rev idx_acc)
    | h :: t ->
        if is_space then aux t (h :: spaces_acc) idx_acc next_id false
        else aux t spaces_acc ((h, next_id) :: idx_acc) (next_id + 1) true
  in
  aux disk_map [] [] 0 false

let expand_disk_2 (spaces : int list) (idx : (int * int) list) : int option list
    =
  List.fold_left2
    (fun acc num_spaces (num_idx, id) ->
      add_to_list None num_spaces (add_to_list (Some id) num_idx acc))
    [] spaces idx

let sort_file ((num_idx, id) : int * int) (spaces : int list)
    (idx : (int * int) list) =
  let rec aux idx_lst sp_lst acc_id acc_sp =
    match (idx_lst, sp_lst) with
    | [], [] -> (acc_id, acc_sp)
    | hi :: ti, hs :: ts ->
        if num_idx > hs then aux ti ts (hi :: acc_id) (hs :: acc_sp)
        else
          ( List.rev acc_id @ [ hi ]
            @ List.filter (fun x -> x = (num_idx, id)) idx_lst,
            acc_sp )
    | _, _ -> failwith "Error"
  in
  aux idx spaces [] []

let sort_files (disk_map : int list) : int option list =
  let spaces, idx = extract_info disk_map in
  let rec aux lst spaces idx =
    match lst with
    | [] -> (spaces, idx)
    | h :: t ->
        let new_idx, new_spaces = sort_file h spaces idx in
        aux t new_spaces new_idx
  in
  let sorted_spaces, sorted_idx = aux (List.rev idx) spaces idx in
  expand_disk_2 sorted_spaces sorted_idx

let get_checksum (disk : int option list) : int =
  let acc, _ =
    List.fold_left
      (fun (acc, idx) el ->
        match el with None -> (acc, idx) | Some e -> (acc + (idx * e), idx + 1))
      (0, 0) disk
  in
  acc

let part1 () =
  let disk_map = read_lines file in
  let full_disk = expand_disk disk_map in
  (* let () = print_disk full_disk *)

  (* let () = full_disk |> sort_blocks |> print_disk *)
  let checksum = full_disk |> sort_blocks |> get_checksum in
  Printf.printf "Checksum: (%d)\n" checksum

let part2 () =
  let disk_map = read_lines file in
  let checksum_sorted_files = disk_map |> sort_files |> get_checksum in
  Printf.printf "Checksum sorted files: (%d)\n" checksum_sorted_files
