defmodule AOC.D20 do
  defp input_file(), do: Util.get_input_filename(20)
  # defp input_file(), do: Util.get_test_filename(20)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp parse_file(lst) do
    lst
    |> Enum.map(fn r ->
      String.to_charlist(r) |> List.to_tuple()
    end)
    |> List.to_tuple()
  end

  defp max_i(map) do
    tuple_size(elem(map, 0)) - 1
  end

  defp max_j(map) do
    tuple_size(map) - 1
  end

  defp get_pos(map, {i, j}) do
    map |> elem(j) |> elem(i)
  end

  defp find_end(map, {i, j}) do
    cond do
      get_pos(map, {i, j}) == ?E -> {i, j}
      i + 1 <= max_i(map) -> find_end(map, {i + 1, j})
      j + 1 <= max_j(map) -> find_end(map, {0, j + 1})
      true -> raise "Not found"
    end
  end

  defp find_end(map) do
    find_end(map, {0, 0})
  end

  defp is_track_point(map, {i, j})
       when j > tuple_size(map) - 1 or i > tuple_size(elem(map, 0)) - 1 or i < 0 or j < 0 do
    false
  end

  defp is_track_point(map, pos) do
    case get_pos(map, pos) do
      ?. -> true
      ?E -> true
      ?S -> true
      _ -> false
    end
  end

  defp is_wall(map, pos) do
    get_pos(map, pos) == ?#
  end

  defp next_move(map, {i, j}, visited) do
    cond do
      is_track_point(map, {i + 1, j}) and not Map.has_key?(visited, {i + 1, j}) -> {i + 1, j}
      is_track_point(map, {i - 1, j}) and not Map.has_key?(visited, {i - 1, j}) -> {i - 1, j}
      is_track_point(map, {i, j + 1}) and not Map.has_key?(visited, {i, j + 1}) -> {i, j + 1}
      is_track_point(map, {i, j - 1}) and not Map.has_key?(visited, {i, j - 1}) -> {i, j - 1}
    end
  end

  defp cheats_in_pos(map, {i, j}) do
    directions = [
      {0, 1, 0, 2},
      {0, -1, 0, -2},
      {1, 0, 2, 0},
      {-1, 0, -2, 0}
    ]

    for {di, dj, ti, tj} <- directions,
        is_wall(map, {i + di, j + dj}) and is_track_point(map, {i + ti, j + tj}) do
      {{i + ti, j + tj}, {i, j}}
    end
  end

  defp find_cheats(map, {i, j}, {visited, cheats, distance})
       when map |> elem(j) |> elem(i) == ?S do
    visited = Map.put(visited, {i, j}, distance)

    for {pos_ini, pos_end} <- cheats do
      Map.get(visited, pos_ini) - Map.get(visited, pos_end) - 2
    end
    |> Enum.filter(fn x -> x > 0 end)
  end

  defp find_cheats(map, pos, {visited, cheats, distance_from_end}) do
    next_pos = next_move(map, pos, visited)
    pos_cheats = cheats_in_pos(map, pos)

    find_cheats(
      map,
      next_pos,
      {Map.put(visited, pos, distance_from_end), pos_cheats ++ cheats, distance_from_end + 1}
    )
  end

  defp find_cheats(map) do
    end_pos = find_end(map)
    find_cheats(map, end_pos, {Map.new(), [], 0})
  end

  def part1 do
    map = input_file() |> read_file() |> parse_file()
    cheats = find_cheats(map)
    min_saves = 100
    num_cheats = cheats |> Enum.filter(fn c -> c >= min_saves end) |> length()

    IO.puts("Number of cheats that save at least #{min_saves} picoseconds #{num_cheats}")
  end
end
