defmodule AOC.D11 do
  use Memoize

  defp input_file(), do: Util.get_input_filename(11)
  # defp input_file(), do: Util.get_test_filename(11)

  defp to_int(value) do
    {num, _} = Integer.parse(value)
    num
  end

  defp has_even_digits(val) do
    num_digits = Integer.to_string(val) |> String.length()
    rem(num_digits, 2) == 0
  end

  defp split_stone(stone) do
    num_str = Integer.to_string(stone)
    new_length = round((num_str |> String.length()) / 2)
    <<stone1::binary-size(new_length), stone2::binary>> = num_str
    [to_int(stone1), to_int(stone2)]
  end

  defp apply_rule(stone) do
    if stone == 0 do
      [1]
    else
      if has_even_digits(stone) do
        split_stone(stone)
      else
        [2024 * stone]
      end
    end
  end

  defp apply_rules(stones) do
    List.foldl(stones, [], fn s, acc -> apply_rule(s) ++ acc end)
  end

  defp apply_rules(stones, iter, i) do
    IO.write("\r#{Progress.bar(i, iter)}")

    if i < iter do
      stones = apply_rules(stones)
      apply_rules(stones, iter, i + 1)
    else
      IO.write("\n")
      stones
    end
  end

  defp apply_rules(stones, iter) do
    apply_rules(stones, iter, 0)
  end

  defp read_file(file) do
    File.read!(file)
    |> String.split(" ")
    |> Enum.filter(fn s -> s != "" end)
    |> Enum.map(fn s -> to_int(s) end)
  end

  def part1 do
    stones =
      input_file() |> read_file()

    num_blinking = 25
    num_stones = apply_rules(stones, num_blinking) |> length()
    IO.puts("Number of stones for #{num_blinking}: #{num_stones}")
  end

  defmemo stones_after_blink(_stone, iter) when iter <= 0 do
    raise "Invalid iter"
  end

  defmemo stones_after_blink(stone, 1) do
    gen_stones = apply_rule(stone)
    length(gen_stones)
  end

  defmemo stones_after_blink(stone, iter) do
    gen_stones = apply_rule(stone)

    case gen_stones do
      [stone] ->
        stones_after_blink(stone, iter - 1)

      [stone1, stone2] ->
        stones_after_blink(stone1, iter - 1) + stones_after_blink(stone2, iter - 1)

      _ ->
        raise "Invalid result"
    end
  end

  defp get_num_stones(stones, iter) do
    List.foldl(stones, 0, fn s, acc -> stones_after_blink(s, iter) + acc end)
  end

  def part2 do
    stones =
      input_file() |> read_file()

    num_blinking = 75
    # num_blinking = 25
    num_stones = get_num_stones(stones, num_blinking)
    IO.puts("Number of stones for #{num_blinking}: #{num_stones}")
  end

  def test_progress do
    total = 50

    Enum.each(1..total, fn task ->
      IO.write("\r#{Progress.bar(task, total)}")
      Process.sleep(50)
    end)

    IO.write("\n")
  end
end
