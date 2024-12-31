let file = Util.get_input_filename 4
(* let file = Util.get_test_filename 4 *)

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

let cc c s = s ^ Char.escaped c

module type Day4 = sig
  val loop : char array array -> int
end

module Part1 : Day4 = struct
  let case1 mx i j =
    if
      Array.length mx.(i) >= j + 4
      && ""
         |> cc mx.(i).(j + 1)
         |> cc mx.(i).(j + 2)
         |> cc mx.(i).(j + 3)
         = "MAS"
    then 1
    else 0

  let case2 mx i j =
    if
      Array.length mx >= i + 4
      && Array.length mx.(i) >= j + 4
      && ""
         |> cc mx.(i + 1).(j + 1)
         |> cc mx.(i + 2).(j + 2)
         |> cc mx.(i + 3).(j + 3)
         = "MAS"
    then 1
    else 0

  let case3 mx i j =
    if
      Array.length mx >= i + 4
      && ""
         |> cc mx.(i + 1).(j)
         |> cc mx.(i + 2).(j)
         |> cc mx.(i + 3).(j)
         = "MAS"
    then 1
    else 0

  let case4 mx i j =
    if
      Array.length mx >= i + 4
      && j >= 3
      && ""
         |> cc mx.(i + 1).(j - 1)
         |> cc mx.(i + 2).(j - 2)
         |> cc mx.(i + 3).(j - 3)
         = "MAS"
    then 1
    else 0

  let case5 mx i j =
    if
      j >= 3
      && ""
         |> cc mx.(i).(j - 1)
         |> cc mx.(i).(j - 2)
         |> cc mx.(i).(j - 3)
         = "MAS"
    then 1
    else 0

  let case6 mx i j =
    if
      j >= 3 && i >= 3
      && ""
         |> cc mx.(i - 1).(j - 1)
         |> cc mx.(i - 2).(j - 2)
         |> cc mx.(i - 3).(j - 3)
         = "MAS"
    then 1
    else 0

  let case7 mx i j =
    if
      i >= 3
      && ""
         |> cc mx.(i - 1).(j)
         |> cc mx.(i - 2).(j)
         |> cc mx.(i - 3).(j)
         = "MAS"
    then 1
    else 0

  let case8 mx i j =
    if
      i >= 3
      && Array.length mx.(i) >= j + 4
      && ""
         |> cc mx.(i - 1).(j + 1)
         |> cc mx.(i - 2).(j + 2)
         |> cc mx.(i - 3).(j + 3)
         = "MAS"
    then 1
    else 0

  let count_xmas (mx : char array array) (i : int) (j : int) : int =
    if mx.(i).(j) != 'X' then 0
    else
      case1 mx i j + case2 mx i j + case3 mx i j + case4 mx i j + case5 mx i j
      + case6 mx i j + case7 mx i j + case8 mx i j

  let loop (mx : char array array) : int =
    let iter = Seq.ints 0 |> Seq.take (Array.length mx) in
    Seq.fold_left
      (fun acc i ->
        let iter = Seq.ints 0 |> Seq.take (Array.length mx.(i)) in
        Seq.fold_left (fun ac j -> ac + count_xmas mx i j) acc iter)
      0 iter
end

module Part2 : Day4 = struct
  let case1 mx i j =
    if
      ""
      |> cc mx.(i - 1).(j - 1)
      |> cc mx.(i).(j)
      |> cc mx.(i + 1).(j + 1)
      = "MAS"
      && ""
         |> cc mx.(i - 1).(j + 1)
         |> cc mx.(i).(j)
         |> cc mx.(i + 1).(j - 1)
         = "MAS"
    then 1
    else 0

  let case2 mx i j =
    if
      ""
      |> cc mx.(i - 1).(j - 1)
      |> cc mx.(i).(j)
      |> cc mx.(i + 1).(j + 1)
      = "MAS"
      && ""
         |> cc mx.(i + 1).(j - 1)
         |> cc mx.(i).(j)
         |> cc mx.(i - 1).(j + 1)
         = "MAS"
    then 1
    else 0

  let case3 mx i j =
    if
      ""
      |> cc mx.(i + 1).(j - 1)
      |> cc mx.(i).(j)
      |> cc mx.(i - 1).(j + 1)
      = "MAS"
      && ""
         |> cc mx.(i + 1).(j + 1)
         |> cc mx.(i).(j)
         |> cc mx.(i - 1).(j - 1)
         = "MAS"
    then 1
    else 0

  let case4 mx i j =
    if
      ""
      |> cc mx.(i + 1).(j + 1)
      |> cc mx.(i).(j)
      |> cc mx.(i - 1).(j - 1)
      = "MAS"
      && ""
         |> cc mx.(i - 1).(j + 1)
         |> cc mx.(i).(j)
         |> cc mx.(i + 1).(j - 1)
         = "MAS"
    then 1
    else 0

  let count_xmas (mx : char array array) (i : int) (j : int) : int =
    if mx.(i).(j) != 'A' then 0
    else case1 mx i j + case2 mx i j + case3 mx i j + case4 mx i j

  let loop (mx : char array array) : int =
    let iter = Seq.ints 0 |> Seq.take (Array.length mx - 2) in
    Seq.fold_left
      (fun acc i ->
        let iter = Seq.ints 0 |> Seq.take (Array.length mx.(i) - 2) in
        Seq.fold_left (fun ac j -> ac + count_xmas mx (i + 1) (j + 1)) acc iter)
      0 iter
end

let part1 () =
  let xmas_matrix = read_lines file |> Part1.loop in
  Printf.printf "Count XMAS: (%d)\n" xmas_matrix

let part2 () =
  let xmas_matrix = read_lines file |> Part2.loop in
  Printf.printf "Count X-MAS: (%d)\n" xmas_matrix
