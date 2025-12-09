let get_input_filename (day : int) : string =
  "../inputs/" ^ string_of_int day ^ ".txt"

let get_test_filename (day : int) : string =
  "../input_tests/" ^ string_of_int day ^ ".txt"

let print_return txt =
  print_endline txt;
  txt

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

let parse_lines (name : string) fn acc =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | None ->
        close_in ic;
        fn None acc
    | s -> loop (fn s acc)
  in
  loop acc
