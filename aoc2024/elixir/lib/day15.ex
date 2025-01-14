defmodule AOC.D15.P1 do
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

  def pos_to_map(pos) do
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

  def map_to_pos(map) do
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

  def move(pos, []) do
    pos
  end

  def move(pos, movs) do
    [m | movs] = movs
    pos = do_move(pos, m)
    move(pos, movs)
  end
end

defmodule AOC.D15.P2 do
  def convert_to_dmap(map) do
    List.foldl(map, [], fn txt, acc ->
      new_row =
        String.to_charlist(txt)
        |> List.foldl([], fn el, row ->
          case el do
            ?# -> [?#, ?#] ++ row
            ?O -> [?], ?[] ++ row
            ?@ -> [?., ?@] ++ row
            ?. -> [?., ?.] ++ row
            _ -> raise "Invalid value"
          end
        end)
        |> Enum.reverse()
        |> List.to_string()

      [new_row | acc]
    end)
    |> Enum.reverse()
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

  def pos_to_dmap(pos) do
    {robot, objects, walls} = pos
    {w, h} = get_map_size(walls)

    Enum.map(0..h, fn j ->
      Enum.map(0..w, fn i ->
        cond do
          MapSet.member?(walls, {i, j}) -> ?#
          MapSet.member?(objects, {i, j}) -> ?[
          MapSet.member?(objects, {i - 1, j}) -> ?]
          {i, j} == robot -> ?@
          true -> ?.
        end
      end)
      |> List.to_string()
    end)
  end

  def dmap_to_pos(dmap) do
    {_, r, o, w} =
      List.foldl(dmap, {0, nil, MapSet.new(), MapSet.new()}, fn txt, {j, robot, objects, wall} ->
        {_, r, o, w} =
          String.to_charlist(txt)
          |> List.foldl({0, robot, objects, wall}, fn el, {i, r, o, w} ->
            case el do
              ?# -> {i + 1, r, o, MapSet.put(w, {i, j})}
              ?[ -> {i + 1, r, MapSet.put(o, {i, j}), w}
              ?@ -> {i + 1, {i, j}, o, w}
              ?. -> {i + 1, r, o, w}
              ?] -> {i + 1, r, o, w}
              _ -> raise "Invalid value"
            end
          end)

        {j + 1, r, o, w}
      end)

    {r, o, w}
  end

  defp object_collition({i, j}, size, m, objects) do
    case {m, size} do
      {?>, 1} ->
        if MapSet.member?(objects, {i, j}) do
          {true, [{i, j}]}
        else
          {false, nil}
        end

      {?>, 2} ->
        if MapSet.member?(objects, {i + 1, j}) do
          {true, [{i + 1, j}]}
        else
          {false, nil}
        end

      {?<, _} ->
        cond do
          MapSet.member?(objects, {i, j}) -> {true, [{i, j}]}
          MapSet.member?(objects, {i - 1, j}) -> {true, [{i - 1, j}]}
          true -> {false, nil}
        end

      {_, 1} ->
        cond do
          MapSet.member?(objects, {i, j}) -> {true, [{i, j}]}
          MapSet.member?(objects, {i - 1, j}) -> {true, [{i - 1, j}]}
          true -> {false, nil}
        end

      {_, 2} ->
        List.foldl([{i - 1, j}, {i, j}, {i + 1, j}], {false, []}, fn pos, {exist, acc} ->
          if MapSet.member?(objects, pos) do
            {true, [pos | acc]}
          else
            {exist, acc}
          end
        end)
    end
  end

  defp wall_collition({i, j}, size, walls) do
    case size do
      1 -> MapSet.member?(walls, {i, j})
      2 -> MapSet.member?(walls, {i, j}) or MapSet.member?(walls, {i + 1, j})
    end
  end

  defp do_move(pos, size, m, objects, walls) do
    {o_collition, o_pos_lst} = object_collition(pos, size, m, objects)

    cond do
      wall_collition(pos, size, walls) ->
        # Do not move
        {:error, "Wall collition"}

      o_collition ->
        # Object collition
        List.foldl(o_pos_lst, {:ok, objects}, fn o_pos, acc ->
          case acc do
            {:error, _} ->
              acc

            {:ok, objects} ->
              next_pos = get_next_pos(o_pos, m)

              case do_move(next_pos, 2, m, objects, walls) do
                {:ok, objects} ->
                  {:ok, MapSet.delete(objects, o_pos) |> MapSet.put(next_pos)}

                {:error, _} ->
                  {:error, "Object collition"}
              end
          end
        end)

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

    case do_move(next_robot_pos, 1, m, objects, walls) do
      {:ok, objects} -> {next_robot_pos, objects, walls}
      {:error, _} -> {robot, objects, walls}
    end
  end

  def move(pos, []) do
    pos
  end

  def move(pos, movs) do
    [m | movs] = movs
    pos = do_move(pos, m)
    move(pos, movs)
  end
end

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

  defp print_map(map) do
    Enum.each(map, fn el ->
      IO.puts(el)
    end)
  end

  defp get_GPS(objects) do
    Enum.map(objects, fn {i, j} -> 100 * j + i end)
  end

  def part1 do
    {map, movs} = input_file() |> read_file() |> parse_file()
    pos = AOC.D15.P1.map_to_pos(map)
    {r, o, w} = AOC.D15.P1.move(pos, movs)
    AOC.D15.P1.pos_to_map({r, o, w}) |> print_map()

    sum_GPS = MapSet.to_list(o) |> get_GPS() |> Enum.sum()
    IO.puts("Sum of all boxes GPS coordinates: #{sum_GPS}")
  end

  def part2 do
    {map, movs} = input_file() |> read_file() |> parse_file()
    dmap = AOC.D15.P2.convert_to_dmap(map)
    pos = AOC.D15.P2.dmap_to_pos(dmap)
    {r, o, w} = AOC.D15.P2.move(pos, movs)
    AOC.D15.P2.pos_to_dmap({r, o, w}) |> print_map()

    sum_GPS = MapSet.to_list(o) |> get_GPS() |> Enum.sum()
    IO.puts("Sum of all boxes GPS coordinates: #{sum_GPS}")
  end
end
