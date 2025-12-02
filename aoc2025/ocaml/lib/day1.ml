let file = Util.get_input_filename 1
(* let file = Util.get_test_filename 1 *)

module Rotation = struct
  type direction = Left | Right
  type t = direction * int

  let parse_direction (s : char) : direction =
    match s with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "Invalid direction"

  let direction (r : t) : char = match r with Left, _ -> 'L' | Right, _ -> 'R'
  let num (r : t) : int = match r with _, n -> n
  let p_print (r : t) : string = Printf.sprintf "%c%d" (direction r) (num r)

  let parse (s : string) : t =
    let dir = parse_direction s.[0] in
    let num = int_of_string (String.sub s 1 (String.length s - 1)) in
    (dir, num)

  let print_list (l : t list) : string =
    l |> List.map p_print |> String.concat "\n"

  let apply (r : t) (pos : int) : int =
    match r with
    | Left, n ->
        let next_pos = (pos - n) mod 100 in
        if next_pos < 0 then next_pos + 100 else next_pos
    | Right, n -> (pos + n) mod 100

  let rotate (rl : t list) (pos : int) : int list =
    List.fold_left
      (fun acc r ->
        let p = List.hd acc in
        let next_p = apply r p in
        next_p :: acc)
      [ pos ] rl
    |> List.rev

  let get_password (rl : t list) : int =
    let positions = rotate rl 50 in
    List.fold_left
      (fun acc p -> match p with 0 -> acc + 1 | _ -> acc)
      0 positions

  let count_zero_rotations (r : t) (pos : int) : int =
    match r with
    | Left, n ->
        let next_pos = pos - n in
        if next_pos < 0 && pos != 0 then (-next_pos / 100) + 1
        else if next_pos < 0 then -next_pos / 100
        else if next_pos = 0 then 1
        else 0
    | Right, n -> (pos + n) / 100

  let rotate_and_count ?(debug : bool = false) (rl : t list) (pos : int) :
      (int * int) list =
    List.fold_left
      (fun acc r ->
        let p, _ = List.hd acc in
        let next_p = apply r p in
        let zero_rotations =
          count_zero_rotations r p
        in
        let () =
          if debug then
            Printf.printf "%d %s -> %d: %d\n" p (p_print r) next_p
              zero_rotations
        in
        (next_p, zero_rotations) :: acc)
      [ (pos, 0) ]
      rl

  let x434C49434B (rl : t list) : int =
    let positions_with_zero_rotations = rotate_and_count ~debug:false rl 50 in
    List.fold_left (fun acc (_, c) -> acc + c) 0 positions_with_zero_rotations
end

let parse_line (s : string option) (acc : Rotation.t list) : Rotation.t list =
  match s with
  | None -> acc
  | Some s ->
      let rotation = Rotation.parse s in
      rotation :: acc

let part1 () =
  let rotations = Util.read_lines file parse_line in
  Printf.printf "Password: (%d)\n" (Rotation.get_password rotations)

let part2 () =
  let rotations = Util.read_lines file parse_line in
  Printf.printf "Password: (%d)\n" (Rotation.x434C49434B rotations)
