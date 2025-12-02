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

  let direction (d : direction) : string =
    match d with Left -> "Left" | Right -> "Right"

  let p_print (r : t) : string =
    match r with d, n -> Printf.sprintf "%s: %d" (direction d) n

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
end

let read_lines (name : string) fn =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | None ->
        close_in ic;
        fn None acc
    | s -> loop (fn s acc)
  in
  loop [] |> List.rev

let parse_line (s : string option) (acc : Rotation.t list) : Rotation.t list =
  match s with
  | None -> acc
  | Some s ->
      let rotation = Rotation.parse s in
      rotation :: acc

let part1 () =
  let rotations = read_lines file parse_line in
  Printf.printf "Password: (%d)\n" (Rotation.get_password rotations)
