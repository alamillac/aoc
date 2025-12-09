(* let file = Util.get_input_filename 4 *)
let file = Util.get_test_filename 4

module Grid = struct
  type t = int array array
  type pos = int * int

  let create (input : string list) : t =
    List.map
      (fun row ->
        String.fold_left (fun acc c -> (if c = '@' then 1 else 0) :: acc) [] row
        |> List.rev |> Array.of_list)
      input
    |> Array.of_list

  let pprint (grid : t) : string =
    let rows =
      Array.map
        (fun row ->
          Array.to_list row
          |> List.map (fun f -> if f = 1 then "@" else ".")
          |> String.concat "")
        grid
      |> Array.to_list
    in
    String.concat "\n" rows

  let pos_adjacent (grid : t) (pos : pos) : pos list =
    let x, y = pos in
    let adjacents =
      [
        (x - 1, y - 1);
        (x - 1, y);
        (x - 1, y + 1);
        (x, y - 1);
        (x, y + 1);
        (x + 1, y - 1);
        (x + 1, y);
        (x + 1, y + 1);
      ]
    in
    List.filter
      (fun (x, y) ->
        if
          x >= 0 && x < Array.length grid && y >= 0 && y < Array.length grid.(0)
        then true
        else false)
      adjacents

  let count_adjacent (grid : t) (pos : pos) : int =
    let adjacents = pos_adjacent grid pos in
    List.map (fun (x, y) -> grid.(x).(y)) adjacents |> List.fold_left ( + ) 0

  let paper_in_pos (grid : t) (pos : pos) : bool =
    let x, y = pos in
    grid.(x).(y) = 1

  let num_rolls (grid : t) : int =
    let count = ref 0 in
    for x = 0 to Array.length grid - 1 do
      for y = 0 to Array.length grid.(0) - 1 do
        if paper_in_pos grid (x, y) && count_adjacent grid (x, y) < 4 then
          incr count
      done
    done;
    !count
end

let parse_line (s : string option) (acc : string list) : string list =
  match s with None -> acc | Some s -> s :: acc

let part1 () =
  let diagram = Util.read_lines file parse_line in
  let grid = Grid.create diagram in
  Printf.printf "Num rolls of paper: (%d)\n" (Grid.num_rolls grid)
