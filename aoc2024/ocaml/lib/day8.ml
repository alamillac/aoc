let file = Util.get_input_filename 8
(* let file = Util.get_test_filename 8 *)

module OrderType = struct
  type t = char

  let compare = compare
end

module LocMap = Map.Make (OrderType)

let explode (s : string) : char array =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [] |> Array.of_list

let read_lines (name : string) : char array array =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop (acc : char array list) : char array list =
    match try_read () with
    | None ->
        close_in ic;
        List.rev acc
    | Some s -> loop (explode s :: acc)
  in
  loop [] |> Array.of_list

let iter_map (map : char array array) fn acc =
  let irow = Array.to_seqi map in
  Seq.fold_left
    (fun acc (i, row) ->
      let jc = Array.to_seqi row in
      Seq.fold_left (fun ac (j, c) -> fn i j c ac) acc jc)
    acc irow

let get_antennas (map : char array array) =
  iter_map map
    (fun i j c acc -> if c <> '.' then LocMap.add_to_list c (i, j) acc else acc)
    LocMap.empty

let print_antennas loc =
  loc
  |> LocMap.iter (fun c lst ->
         Printf.printf "%c: " c;
         List.iter (fun (x, y) -> Printf.printf "(%d,%d) " x y) lst;
         Printf.printf "\n")

let print_antinodes lst =
  let res =
    List.map
      (fun (x, y) ->
        Printf.printf "(%d,%d) " x y;
        (x, y))
      lst
  in
  Printf.printf "\n";
  res

let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let remove_from_left xs = List.fold_left cons_uniq [] xs

let get_antinode ((ai, aj) : int * int) ((bi, bj) : int * int)
    (map_size : int * int) : bool * (int * int) =
  let xi = (2 * ai) - bi in
  let xj = (2 * aj) - bj in
  if xi >= 0 && xi < fst map_size && xj >= 0 && xj < snd map_size then
    (true, (xi, xj))
  else (false, (0, 0))

let count_antinode loc map_size has_resonant_harmonics =
  let get_antinodes (tlst : (int * int) list) : (int * int) list =
    let rec aux a lst acc =
      match lst with
      (* | [] -> acc |> print_antinodes *)
      | [] -> acc
      | b :: t ->
          let is_antinode, antinode = get_antinode a b map_size in
          let ac1 =
            if (not is_antinode) || List.exists (fun e -> antinode = e) acc then
              acc
            else if has_resonant_harmonics then
              aux antinode tlst (antinode :: acc)
            else antinode :: acc
          in
          let is_antinode, antinode = get_antinode b a map_size in
          let ac2 =
            if (not is_antinode) || List.exists (fun e -> antinode = e) ac1 then
              ac1
            else if has_resonant_harmonics then
              aux antinode tlst (antinode :: ac1)
            else antinode :: ac1
          in
          aux a t (aux b t ac2)
    in
    aux (List.hd tlst) (List.tl tlst) []
  in
  let agg acc (_, h) : (int * int) list list = get_antinodes h :: acc in
  let total_antinodes = List.fold_left agg [] (LocMap.to_list loc) in
  total_antinodes |> List.flatten |> remove_from_left
  (* |> print_antinodes *)
  |> List.length

let part1 () =
  let antennas_map = read_lines file in
  let loc = get_antennas antennas_map in
  (* let () = print_antennas loc *)
  let num_antinode =
    count_antinode loc
      (Array.length antennas_map, Array.length antennas_map.(0))
      false
  in
  Printf.printf "Num antinodes: (%d)\n" num_antinode

let part2 () =
  let antennas_map = read_lines file in
  let loc = get_antennas antennas_map in
  let num_antinode_with_harmonics =
    count_antinode loc
      (Array.length antennas_map, Array.length antennas_map.(0))
      true
  in
  Printf.printf "Num antinodes with harmonics: (%d)\n"
    num_antinode_with_harmonics
