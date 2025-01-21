defmodule AOC.D18 do
  defp input_file(), do: {Util.get_input_filename(18), {70, 70}, 1024}
  # defp input_file(), do: {Util.get_test_filename(18), {6, 6}, 12}

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp parse_file(lst) do
    Enum.map(lst, fn e ->
      [i, j] = String.split(e, ",")
      {String.to_integer(i), String.to_integer(j)}
    end)
  end

  defp generate_map({w, h}, positions) do
    for j <- 0..h do
      for i <- 0..w do
        if MapSet.member?(positions, {i, j}) do
          ?#
        else
          ?.
        end
      end
      |> List.to_tuple()
    end
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

  defp is_valid_move(map, {i, j}) do
    i <= max_i(map) and i >= 0 and j <= max_j(map) and j >= 0 and get_pos(map, {i, j}) != ?#
  end

  defp find_exit(_map, _exit, [], _visited) do
    {:not_found}
  end

  defp find_exit(_map, exit, [{acc, move} | _lst], _visited) when move == exit do
    {:ok, acc}
  end

  defp find_exit(map, exit, [{acc, {i, j}} | lst], visited) do
    if MapSet.member?(visited, {i, j}) do
      visited = MapSet.put(visited, {i, j})
      find_exit(map, exit, lst, visited)
    else
      visited = MapSet.put(visited, {i, j})

      lst =
        lst
        |> append_if_valid(map, {i - 1, j}, acc + 1)
        |> append_if_valid(map, {i + 1, j}, acc + 1)
        |> append_if_valid(map, {i, j - 1}, acc + 1)
        |> append_if_valid(map, {i, j + 1}, acc + 1)
        |> Enum.sort(fn x, y -> elem(x, 0) < elem(y, 0) end)

      find_exit(map, exit, lst, visited)
    end
  end

  defp append_if_valid(lst, map, pos, acc) do
    if is_valid_move(map, pos) do
      [{acc, pos} | lst]
    else
      lst
    end
  end

  defp find_exit(map, start, exit) do
    find_exit(map, exit, [{0, start}], MapSet.new())
  end

  defp find_first_byte(positions, bytes, start, exit) do
    map = generate_map(exit, positions |> Enum.take(bytes) |> MapSet.new())

    case find_exit(map, start, exit) do
      {:not_found} -> bytes
      _ -> find_first_byte(positions, bytes + 1, start, exit)
    end
  end

  def part1 do
    {file, exit, bytes} = input_file()

    positions = file |> read_file() |> parse_file() |> Enum.take(bytes) |> MapSet.new()
    map = generate_map(exit, positions)
    start = {0, 0}
    {:ok, min_steps} = find_exit(map, start, exit)
    IO.puts("Minimum steps to reach exit: #{min_steps}")
  end

  def part2 do
    {file, exit, bytes} = input_file()

    positions = file |> read_file() |> parse_file()
    start = {0, 0}
    first_byte = find_first_byte(positions, bytes + 1, start, exit)
    byte_coordinate = Enum.at(positions, first_byte - 1)
    IO.puts(
      "First byte that will cut off the path to the exit: #{first_byte} Coordinate: #{elem(byte_coordinate, 0)},#{elem(byte_coordinate, 1)}"
    )
  end
end
