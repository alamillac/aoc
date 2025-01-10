defmodule AOC.D15 do
  defp input_file(), do: Util.get_input_filename(15)
  # defp input_file(), do: Util.get_test_filename(15)

  defp read_file(file) do
    File.read!(file) |> String.split("\n")
  end

  defp parse_file(lst, map) do
    case lst do
      [] ->
        raise "Invalid data"

      [h | t] when h == "" ->
        {map |> Enum.reverse(), Enum.join(t) |> String.to_charlist()}

      [h | t] ->
        parse_file(t, [h | map])
    end
  end

  defp parse_file(lst) do
    parse_file(lst, [])
  end

  defp get_map_size(walls) do
    MapSet.to_list(walls)
    |> List.foldl({0, 0}, fn {i, j}, {max_i, max_j} ->
      case {i > max_i, j > max_j} do
        {true, true} -> {i, j}
        {true, false} -> {i, max_j}
        {false, true} -> {max_i, j}
        {false, false} -> {max_i, max_j}
      end
    end)
  end

  defp pos_to_map(pos) do
    {robot, objects, walls} = pos
    {w, h} = get_map_size(walls)

    Enum.map(0..w, fn j ->
      Enum.map(0..h, fn i ->
        cond do
          MapSet.member?(walls, {i, j}) -> ?#
          MapSet.member?(objects, {i, j}) -> ?O
          {i, j} == robot -> ?@
          true -> ?.
        end
      end)
      |> List.to_string()
    end)
  end

  defp map_to_pos(map) do
    {_, r, o, w} =
      List.foldl(map, {0, nil, MapSet.new(), MapSet.new()}, fn txt, {j, robot, objects, wall} ->
        {_, r, o, w} =
          String.to_charlist(txt)
          |> List.foldl({0, robot, objects, wall}, fn el, {i, r, o, w} ->
            case el do
              ?# -> {i + 1, r, o, MapSet.put(w, {i, j})}
              ?O -> {i + 1, r, MapSet.put(o, {i, j}), w}
              ?@ -> {i + 1, {i, j}, o, w}
              ?. -> {i + 1, r, o, w}
              _ -> raise "Invalid value"
            end
          end)

        {j + 1, r, o, w}
      end)

    {r, o, w}
  end

  defp print_map(map) do
    Enum.each(map, fn el ->
      IO.puts(el)
    end)
  end

  defp do_move(pos, m, objects, walls) do
    cond do
      MapSet.member?(walls, pos) ->
        # Do not move
        {:error, "Wall collition"}

      MapSet.member?(objects, pos) ->
        # Object collition
        next_pos = get_next_pos(pos, m)

        case do_move(next_pos, m, objects, walls) do
          {:ok, objects} ->
            {:ok, MapSet.delete(objects, pos) |> MapSet.put(next_pos)}

          {:error, _} ->
            {:error, "Object collition"}
        end

      true ->
        {:ok, objects}
    end
  end

  defp get_next_pos({i, j}, m) do
    case m do
      # move rigth
      ?> -> {i + 1, j}
      # move left
      ?< -> {i - 1, j}
      # move up
      ?^ -> {i, j - 1}
      # move down
      ?v -> {i, j + 1}
      _ -> raise "Invalid movement"
    end
  end

  defp do_move(pos, m) do
    {robot, objects, walls} = pos
    next_robot_pos = get_next_pos(robot, m)

    case do_move(next_robot_pos, m, objects, walls) do
      {:ok, objects} -> {next_robot_pos, objects, walls}
      {:error, _} -> {robot, objects, walls}
    end
  end

  defp move(pos, []) do
    pos
  end

  defp move(pos, movs) do
    [m | movs] = movs
    pos = do_move(pos, m)
    move(pos, movs)
  end

  defp get_GPS(objects) do
    Enum.map(objects, fn {i, j} -> 100 * j + i end)
  end

  def part1 do
    {map, movs} = input_file() |> read_file() |> parse_file()
    pos = map_to_pos(map)
    {r, o, w} = move(pos, movs)
    pos_to_map({r, o, w}) |> print_map()

    sum_GPS = MapSet.to_list(o) |> get_GPS() |> Enum.sum()
    IO.puts("Sum of all boxes GPS coordinates: #{sum_GPS}")
  end
end
