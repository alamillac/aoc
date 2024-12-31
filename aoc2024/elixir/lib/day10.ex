defmodule AOC.D10 do
  defp input_file(), do: Util.get_input_filename(10)
  # defp input_file(), do: Util.get_test_filename(10)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp to_int_tuple(string, acc) do
    case string do
      <<>> ->
        acc |> Enum.reverse() |> List.to_tuple()

      <<head::binary-size(1), rest::binary>> ->
        {num, _} = Integer.parse(head)
        to_int_tuple(rest, [num] ++ acc)
    end
  end

  defp to_int_tuple(string) do
    to_int_tuple(string, [])
  end

  defp max_i(map) do
    tuple_size(map) - 1
  end

  defp max_j(map) do
    tuple_size(elem(map, 0)) - 1
  end

  defp is_valid_pos(map, i, j) do
    i <= max_i(map) and j <= max_j(map) and i >= 0 and j >= 0
  end

  defp get_pos(map, i, j) do
    if is_valid_pos(map, i, j) do
      map |> elem(i) |> elem(j)
    else
      -1
    end
  end

  defp get_score_pos(map, i, j) do
    get_paths(map, i, j, 0, []) |> Enum.uniq() |> length()
  end

  defp get_paths(map, i, j, initial, acc) do
    value = get_pos(map, i, j)

    if value == initial do
      if value == 9 do
        [{i, j, value}] ++ acc
      else
        get_paths(map, i - 1, j, initial + 1, acc) ++
          get_paths(map, i + 1, j, initial + 1, acc) ++
          get_paths(map, i, j - 1, initial + 1, acc) ++
          get_paths(map, i, j + 1, initial + 1, acc)
      end
    else
      []
    end
  end

  defp get_ratings_pos(map, i, j) do
    get_ratings_pos(map, i, j, 0)
  end

  defp get_ratings_pos(map, i, j, initial) do
    value = get_pos(map, i, j)

    if value == initial do
      if value == 9 do
        1
      else
        get_ratings_pos(map, i - 1, j, initial + 1) +
          get_ratings_pos(map, i + 1, j, initial + 1) +
          get_ratings_pos(map, i, j - 1, initial + 1) +
          get_ratings_pos(map, i, j + 1, initial + 1)
      end
    else
      0
    end
  end

  defp get_score(map, acc, i, j, criteria) do
    score = criteria.(map, i, j) + acc

    if(j > 0) do
      get_score(map, score, i, j - 1, criteria)
    else
      if i > 0 do
        get_score(map, score, i - 1, max_j(map), criteria)
      else
        score
      end
    end
  end

  defp get_score(map, criteria) do
    get_score(map, 0, max_i(map), max_j(map), criteria)
  end

  def part1 do
    map =
      input_file() |> read_file() |> Enum.map(fn s -> to_int_tuple(s) end) |> List.to_tuple()

    score = get_score(map, &get_score_pos/3)
    IO.puts("Sum of all trailheads scores: #{score}")
  end

  def part2 do
    map =
      input_file() |> read_file() |> Enum.map(fn s -> to_int_tuple(s) end) |> List.to_tuple()
    ratings = get_score(map, &get_ratings_pos/3)
    IO.puts("Sum of all trailheads ratings: #{ratings}")
  end
end
