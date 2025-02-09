defmodule AOC.D25 do
  defp input_file(), do: Util.get_input_filename(25)
  # defp input_file(), do: Util.get_test_filename(25)

  defp read_file(file) do
    File.read!(file)
    |> String.split("\n")
  end

  defp parse_file([], keys, locks, [], _type) do
    {keys, locks}
  end

  defp parse_file([h | t], keys, locks, [], nil) do
    cond do
      h == "#####" -> parse_file(t, keys, locks, [h], :lock)
      h == "....." -> parse_file(t, keys, locks, [h], :key)
      true -> raise "Invalid type"
    end
  end

  defp parse_file([h | t], keys, locks, acc, :lock) do
    if h == "" do
      parse_file(t, keys, [acc | locks], [], nil)
    else
      parse_file(t, keys, locks, [h | acc], :lock)
    end
  end

  defp parse_file([h | t], keys, locks, acc, :key) do
    if h == "" do
      parse_file(t, [acc | keys], locks, [], nil)
    else
      parse_file(t, keys, locks, [h | acc], :key)
    end
  end

  defp parse_file(lst) do
    parse_file(lst, [], [], [], nil)
  end

  defp to_pin_heights(schema) do
    schema
    |> Enum.slice(1, 5)
    |> List.foldl([0, 0, 0, 0, 0], fn e, acc ->
      row = String.to_charlist(e)

      for {r, a} <- Enum.zip(row, acc) do
        if r == ?# do
          a + 1
        else
          a
        end
      end
    end)
  end

  defp overlaps?(pin_key, pin_lock) do
    Enum.zip(pin_key, pin_lock)
    |> Enum.find_value(
      false,
      fn {pk, pl} ->
        pk + pl > 5
      end
    )
  end

  defp count_unique_pairs(pin_keys, pin_locks) do
    for k <- pin_keys, l <- pin_locks do
      if overlaps?(k, l) do
        0
      else
        1
      end
    end
    |> Enum.sum()
  end

  def part1 do
    {keys, locks} = input_file() |> read_file() |> parse_file()
    keys = keys |> Enum.map(&to_pin_heights/1)
    locks = locks |> Enum.map(&to_pin_heights/1)
    unique_pairs = count_unique_pairs(keys, locks)

    IO.puts("Number of unique lock/key pairs #{unique_pairs}")
  end
end
