defmodule AOC.D23 do
  defp input_file(), do: Util.get_input_filename(23)
  # defp input_file(), do: Util.get_test_filename(23)

  defp read_file(file) do
    File.read!(file)
    |> String.split("\n")
    |> Enum.filter(fn s -> s != "" end)
    |> Enum.map(fn e ->
      e |> String.split("-") |> MapSet.new()
    end)
  end

  defp eval_connected(_e, [], acc) do
    acc
  end

  defp eval_connected(e, [h | t], acc) do
    diff = MapSet.symmetric_difference(h, e)

    cond do
      MapSet.size(diff) != 2 ->
        eval_connected(e, t, acc)

      Enum.member?(t, diff) ->
        net = MapSet.union(e, h) |> MapSet.union(diff) |> MapSet.to_list()
        eval_connected(e, t, [net | acc])

      true ->
        eval_connected(e, t, acc)
    end
  end

  defp find_connected_computers([], acc) do
    acc
  end

  defp find_connected_computers([h | t], acc) do
    found = eval_connected(h, t, [])
    find_connected_computers(t, found ++ acc)
  end

  defp find_connected_computers(map_connections) do
    find_connected_computers(map_connections, [])
  end

  defp start_with_t(connected_computers) do
    connected_computers
    |> Enum.find_value(false, fn c ->
      c |> String.starts_with?("t")
    end)
  end

  def part1 do
    map_connections = input_file() |> read_file()

    num_connected_computers_t =
      find_connected_computers(map_connections) |> Enum.filter(&start_with_t/1) |> Enum.count()

    IO.puts(
      "Number of connected computers with a computer that starts with t #{num_connected_computers_t}"
    )
  end
end
