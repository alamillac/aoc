let file = Util.get_input_filename 7
(* let file = Util.get_test_filename 7 *)

module Manifold = struct
  type pos = int
  type t = { start : pos; splitters : pos list list }

  let try_split (row : pos list) (beam : pos) : pos list * int =
    let split = List.exists (fun splitter -> splitter = beam) row in
    if split then ([ beam - 1; beam + 1 ], 1) else ([ beam ], 0)

  let step (beams : pos list) (row : pos list) : pos list * int =
    let split_fn = try_split row in
    let beams_list, num_splits = List.map split_fn beams |> List.split in
    let beams = List.concat beams_list |> List.sort_uniq compare in
    let num_splits = List.fold_left ( + ) 0 num_splits in
    (beams, num_splits)

  let num_splits (manifold : t) : int =
    List.fold_left
      (fun (beams, num_splits) row ->
        let beams, n = step beams row in
        (beams, num_splits + n))
      ([ manifold.start ], 0) manifold.splitters
    |> snd
end

module ManifoldParser = struct
  let parse_line (s : string option)
      (acc : Manifold.pos option * Manifold.pos list list) :
      Manifold.pos option * Manifold.pos list list =
    match s with
    | None -> acc
    | Some s ->
        let start, splitters = acc in
        if start = None then
          let start = String.to_seq s |> Seq.find_index (fun c -> c = 'S') in
          (start, splitters)
        else
          let row =
            String.to_seqi s
            |> Seq.filter (fun (_, c) -> c = '^')
            |> Seq.map (fun (i, _) -> i)
            |> List.of_seq
          in
          (start, row :: splitters)

  let parse (file : string) : Manifold.t =
    let start, splitters = Util.parse_lines file parse_line (None, []) in
    let start = Option.get start in
    let splitters = List.rev splitters in
    { start; splitters }
end

let part1 () =
  let manifold = ManifoldParser.parse file in
  let num_splits = Manifold.num_splits manifold in
  Printf.printf "Beam splits: (%d)\n" num_splits
