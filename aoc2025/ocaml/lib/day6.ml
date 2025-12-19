let file = Util.get_input_filename 6
(* let file = Util.get_test_filename 6 *)

module Worksheet = struct
  type operation = Add | Multiply
  type problem = { numbers : int list; operation : operation }
  type t = problem array

  let init (operations : operation array) : t =
    Array.mapi
      (fun i _ -> { numbers = []; operation = operations.(i) })
      operations

  let add_numbers (worksheet : t) (numbers : int array) : t =
    Array.mapi
      (fun i problem ->
        { problem with numbers = numbers.(i) :: problem.numbers })
      worksheet

  let parse_operation (op : string) : operation =
    match op with
    | "+" -> Add
    | "*" -> Multiply
    | x -> failwith ("Invalid operation" ^ x)

  let solve_problem (problem : problem) : int =
    match problem.operation with
    | Add -> List.fold_left ( + ) 0 problem.numbers
    | Multiply -> List.fold_left ( * ) 1 problem.numbers

  let solve (worksheet : t) : int =
    Array.map solve_problem worksheet |> Array.fold_left ( + ) 0
end

module WorksheetParser = struct
  let parse_line (s : string option) (acc : string array list) :
      string array list =
    match s with
    | None -> acc
    | Some s ->
        let row =
          String.split_on_char ' ' s |> List.filter (( <> ) "") |> Array.of_list
        in
        row :: acc

  let parse (file : string) : Worksheet.t =
    let res = Util.parse_lines file parse_line [] in
    match res with
    | [] -> failwith "Invalid worksheet"
    | op_strs :: num_strs ->
        let operations = Array.map Worksheet.parse_operation op_strs in
        let numbers =
          List.map (fun row -> Array.map int_of_string row) num_strs
        in
        let worksheet = Worksheet.init operations in
        List.fold_left Worksheet.add_numbers worksheet numbers
end

let part1 () =
  let worksheet = WorksheetParser.parse file in
  let solution = Worksheet.solve worksheet in
  Printf.printf "Grand total: (%d)\n" solution
