let file = Util.get_input_filename 1

let to_tuple = function
  | [ v1; v2 ] -> (v1, v2)
  | _ -> failwith "Argument out of range"

let split2 (txt : string) : int * int =
  txt |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string |> to_tuple

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

let parse_line (s : string option) (acc : 'a list * 'b list) : 'a list * 'b list
    =
  match s with
  | None ->
      let a1, a2 = acc in
      (List.sort compare a1, List.sort compare a2)
      (* Sort list *)
      (* (List.rev a1, List.rev a2) *)
  | Some s ->
      let t1, t2 = split2 s in
      let a1, a2 = acc in
      (t1 :: a1, t2 :: a2)

let rec join_list (l1 : 'a list) (l2 : 'a list) (acc : ('a * 'a) list) :
    ('a * 'a) list =
  match (l1, l2) with
  | [], [] -> acc
  | h1 :: t1, h2 :: t2 -> join_list t1 t2 ((h1, h2) :: acc)
  | [], _ -> failwith "Invalid argument l2"
  | _, [] -> failwith "Invalid argument l1"

let rec get_distance_aux (lst : (int * int) list) (acc : int) : int =
  match lst with
  | [] -> acc
  | (d1, d2) :: t -> get_distance_aux t (acc + Int.abs (d1 - d2))

let get_distance lst = get_distance_aux lst 0

let rec count_val (x : int) (lst : int list) : int =
  match lst with [] -> 0 | h :: t -> (if x = h then 1 else 0) + count_val x t

let rec get_similarity_aux (l1 : int list) (l2 : int list) (acc : int) : int =
  match l1 with
  | [] -> acc
  | h :: t ->
      let ac = (h * count_val h l2) + acc in
      get_similarity_aux t l2 ac

let get_similarity l1 l2 = get_similarity_aux l1 l2 0

let part1 () =
  let l1, l2 = read_lines file parse_line ([], []) in
  let distance = join_list l1 l2 [] |> get_distance in
  Printf.printf "Distance: (%d)\n" distance

let part2 () =
  let l1, l2 = read_lines file parse_line ([], []) in
  let similarity = get_similarity l1 l2 in
  Printf.printf "Similarity: (%d)\n" similarity
