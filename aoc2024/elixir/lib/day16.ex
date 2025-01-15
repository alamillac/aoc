defmodule AOC.D16 do
  defp input_file(), do: Util.get_input_filename(16)
  # defp input_file(), do: Util.get_test_filename(16)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp parse_file(lst, map) do
    case lst do
      [] ->
        map |> Enum.reverse()

      [h | t] ->
        row = h |> String.to_charlist()
        parse_file(t, [row | map])
    end
  end

  defp to_tuple(map) do
    Enum.map(map, fn el -> List.to_tuple(el) end) |> List.to_tuple()
  end

  defp parse_file(lst) do
    parse_file(lst, [])
  end

  defp get_pos(maze, {i, j}) do
    maze |> elem(j) |> elem(i)
  end

  defp max_i(maze) do
    tuple_size(elem(maze, 0)) - 1
  end

  defp max_j(maze) do
    tuple_size(maze) - 1
  end

  defp find_start(maze, i, j) do
    if get_pos(maze, {i, j}) == ?S do
      {i, j}
    else
      cond do
        i < max_i(maze) -> find_start(maze, i + 1, j)
        j < max_j(maze) -> find_start(maze, 0, j + 1)
        true -> raise "Not found"
      end
    end
  end

  defp find_start(maze) do
    find_start(maze, 0, 0)
  end

  defp print_map(map) do
    Enum.each(map, fn el ->
      IO.puts(el)
    end)
  end

  defp is_finish(maze, pos) do
    if get_pos(maze, pos) == ?E do
      true
    else
      false
    end
  end

  defp turn_left(direction) do
    case direction do
      ?< -> ?v
      ?^ -> ?<
      ?> -> ?^
      ?v -> ?>
      _ -> raise "Invalid direction"
    end
  end

  defp turn_right(direction) do
    case direction do
      ?< -> ?^
      ?^ -> ?>
      ?> -> ?v
      ?v -> ?<
      _ -> raise "Invalid direction"
    end
  end

  defp go_forward({i, j}, direction) do
    case direction do
      ?< -> {i - 1, j}
      ?^ -> {i, j - 1}
      ?> -> {i + 1, j}
      ?v -> {i, j + 1}
      _ -> raise "Invalid direction"
    end
  end

  defp is_valid_pos(maze, {i, j}) do
    if i > max_i(maze) or i < 0 or j > max_j(maze) or j < 0 or get_pos(maze, {i, j}) == ?# do
      false
    else
      true
    end
  end

  defp append_if_valid(lst, maze, score, pos, direction, visited) do
    if not MapSet.member?(visited, {pos, direction}) and is_valid_pos(maze, pos) do
      [{score, pos, direction} | lst]
    else
      lst
    end
  end

  defp find_paths(maze, score, pos, direction, visited) do
    # 3 options (go forward, turn left, turn right)
    []
    |> append_if_valid(maze, score + 1, go_forward(pos, direction), direction, visited)
    |> append_if_valid(maze, score + 1000, pos, turn_left(direction), visited)
    |> append_if_valid(maze, score + 1000, pos, turn_right(direction), visited)
  end

  defp solve_maze(maze, [{score, pos, direction} | leaves], visited) do
    if is_finish(maze, pos) do
      score
    else
      visited = MapSet.put(visited, {pos, direction})
      new_leaves = find_paths(maze, score, pos, direction, visited)
      sorted_leaves = Enum.sort(leaves ++ new_leaves, fn {v1, _, _}, {v2, _, _} -> v2 >= v1 end)
      solve_maze(maze, sorted_leaves, visited)
    end
  end

  defp solve_maze(maze, start) do
    solve_maze(maze, [{0, start, ?>}], MapSet.new())
  end

  def part1 do
    map = input_file() |> read_file() |> parse_file()
    print_map(map)
    maze = to_tuple(map)
    start = find_start(maze)
    min_score = solve_maze(maze, start)
    IO.puts("Lowest score: #{min_score}")
  end
end
