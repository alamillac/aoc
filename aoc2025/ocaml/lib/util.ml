let get_input_filename (day : int) : string =
  "../inputs/" ^ string_of_int day ^ ".txt"

let get_test_filename (day : int) : string =
  "../input_tests/" ^ string_of_int day ^ ".txt"

let print_return txt =
  print_endline txt;
  txt

let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g
