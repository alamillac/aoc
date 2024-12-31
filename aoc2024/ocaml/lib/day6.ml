let file = Util.get_input_filename 6
(* let file = Util.get_test_filename 6 *)

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

let is_guard (c : char) : bool = c = '<' || c = '>' || c = '^' || c = 'v'

let get_guard_pos (map : char array array) : int * int =
  let rec aux (i : int) (j : int) : int * int =
    if is_guard map.(i).(j) then (i, j)
    else if i - 1 < 0 then aux (Array.length map - 1) (j - 1)
    else if j - 1 < 0 then failwith "Guard not found"
    else aux (i - 1) j
  in
  aux (Array.length map - 1) (Array.length map.(0) - 1)

module type Day6 = sig
  val count : char array array -> int
end

module Part1 : Day6 = struct
  let predict_route (map : char array array) (pos : int * int) :
      char array array =
    let m = Array.(map copy) map in
    let next_pos g (i, j) : int * int =
      match g with
      | '<' -> (i, j - 1)
      | '>' -> (i, j + 1)
      | '^' -> (i - 1, j)
      | 'v' -> (i + 1, j)
      | _ -> failwith "Invalid guard"
    in
    let rotate_guardian = function
      | '<' -> '^'
      | '>' -> 'v'
      | '^' -> '>'
      | 'v' -> '<'
      | _ -> failwith "Invalid guard"
    in
    let move_guard g (i0, j0) (i1, j1) : char * (int * int) * (int * int) =
      match (g, m.(i1).(j1)) with
      | g, '#' ->
          let new_g = rotate_guardian g in
          (new_g, (i0, j0), next_pos new_g (i0, j0))
      | g, _ ->
          m.(i0).(j0) <- 'X';
          (g, (i1, j1), next_pos g (i1, j1))
    in
    let rec loop g pos (i, j) =
      if i < 0 || j < 0 || i >= Array.length map || j >= Array.length map.(0)
      then m
      else
        let g, cpos, npos = move_guard g pos (i, j) in
        loop g cpos npos
    in
    let g = map.(fst pos).(snd pos) in
    loop g pos (next_pos g pos)

  let count_positions (map : char array array) : int =
    let rec aux i j acc =
      if i < 0 then aux (Array.length map - 1) (j - 1) acc
      else if j < 0 then acc
      else if map.(i).(j) = 'X' then aux (i - 1) j (acc + 1)
      else aux (i - 1) j acc
    in
    aux (Array.length map - 1) (Array.length map.(0) - 1) 1

  let count map = map |> get_guard_pos |> predict_route map |> count_positions
end

module Part2 : Day6 = struct
  let guard_to_int = function
    | '<' -> 1
    | '>' -> 2
    | '^' -> 3
    | 'v' -> 4
    | _ -> failwith "Invalid guard"

  module OrderType = struct
    type t = char * int * int

    let compare (r1 : t) (r2 : t) =
      let r11, r12, r13 = r1 in
      let r21, r22, r23 = r2 in
      let r = guard_to_int r11 - guard_to_int r21 in
      if r = 0 then
        let r = r12 - r22 in
        if r = 0 then r13 - r23 else r
      else r
  end

  module PosSet = Set.Make (OrderType)

  let is_guard_in_loop (m : char array array) : bool =
    let pos = get_guard_pos m in
    let next_pos g (i, j) : int * int =
      match g with
      | '<' -> (i, j - 1)
      | '>' -> (i, j + 1)
      | '^' -> (i - 1, j)
      | 'v' -> (i + 1, j)
      | _ -> failwith "Invalid guard"
    in
    let rotate_guardian = function
      | '<' -> '^'
      | '>' -> 'v'
      | '^' -> '>'
      | 'v' -> '<'
      | _ -> failwith "Invalid guard"
    in
    let move_guard g (i0, j0) (i1, j1) : char * (int * int) * (int * int) =
      match (g, m.(i1).(j1)) with
      | g, '#' ->
          let new_g = rotate_guardian g in
          (new_g, (i0, j0), next_pos new_g (i0, j0))
      | g, _ -> (g, (i1, j1), next_pos g (i1, j1))
    in
    let update_positions g pos pos_set =
      PosSet.add (g, fst pos, snd pos) pos_set
    in
    let exist_guard_pos g pos pos_set =
      if PosSet.find_opt (g, fst pos, snd pos) pos_set = None then false
      else true
    in
    let rec loop g pos (i, j) pos_set =
      if exist_guard_pos g pos pos_set then true
      else if i < 0 || j < 0 || i >= Array.length m || j >= Array.length m.(0)
      then false
      else
        let pos_set = update_positions g pos pos_set in
        let g, cpos, npos = move_guard g pos (i, j) in
        loop g cpos npos pos_set
    in
    let g = m.(fst pos).(snd pos) in
    loop g pos (next_pos g pos) PosSet.empty

  let count (map : char array array) : int =
    let rec aux i j acc =
      if i < 0 then aux (Array.length map - 1) (j - 1) acc
      else if j < 0 then acc
      else if map.(i).(j) = '#' || is_guard map.(i).(j) then aux (i - 1) j acc
      else if
        let m = Array.(map copy) map in
        m.(i).(j) <- '#';
        is_guard_in_loop m
      then aux (i - 1) j (acc + 1)
      else aux (i - 1) j acc
    in
    aux (Array.length map - 1) (Array.length map.(0) - 1) 0
end

let part1 () =
  let guard_map = read_lines file in
  let num_pos = Part1.count guard_map in
  Printf.printf "Num guard positions visited: (%d)\n" num_pos

let part2 () =
  let guard_map = read_lines file in
  let num_obstruction = Part2.count guard_map in
  Printf.printf "Num posible obstruction: (%d)\n" num_obstruction
