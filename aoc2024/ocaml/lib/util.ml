let get_input_filename (day : int) : string =
  "../inputs/" ^ string_of_int day ^ ".txt"

let get_test_filename (day : int) : string =
  "../input_tests/" ^ string_of_int day ^ ".txt"

let print_return txt =
  print_endline txt;
  txt
