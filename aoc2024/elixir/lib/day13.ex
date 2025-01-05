defmodule ClawMachine do
  defstruct a: nil, b: nil, price: nil
end

defmodule AOC.D13 do
  defp input_file(), do: Util.get_input_filename(13)
  # defp input_file(), do: Util.get_test_filename(13)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp is_int(num) do
    trunc(num) == num
  end

  defp parse(re, str) do
    res = Regex.named_captures(re, str)

    if res == nil do
      {:error, "Not possible to parse"}
    else
      {:ok, %{x: String.to_integer(res["x"]), y: String.to_integer(res["y"])}}
    end
  end

  defp parse_a(str) do
    re = ~r/^Button A: X\+(?<x>\d+), Y\+(?<y>\d+)$/
    parse(re, str)
  end

  defp parse_b(str) do
    re = ~r/^Button B: X\+(?<x>\d+), Y\+(?<y>\d+)$/
    parse(re, str)
  end

  defp parse_price(str) do
    re = ~r/^Prize: X=(?<x>\d+), Y=(?<y>\d+)$/
    parse(re, str)
  end

  defp get_num_token(a, tokens_a, b, tokens_b, price) do
    num_b = (price.y * a.x - price.x * a.y) / (a.x * b.y - b.x * a.y)
    num_a = (price.x - b.x * num_b) / a.x

    if not is_int(num_b) or not is_int(num_a) or num_a > 100 or num_b > 100 do
      {:error, "Not possible to solve it"}
    else
      {:ok, trunc(num_a) * tokens_a + trunc(num_b) * tokens_b}
    end
  end

  defp get_fixed_num_token(a, tokens_a, b, tokens_b, price) do
    p_x = price.x + 10000000000000
    p_y = price.y + 10000000000000
    num_b = (p_y * a.x - p_x * a.y) / (a.x * b.y - b.x * a.y)
    num_a = (p_x - b.x * num_b) / a.x

    if not is_int(num_b) or not is_int(num_a) do
      {:error, "Not possible to solve it"}
    else
      {:ok, trunc(num_a) * tokens_a + trunc(num_b) * tokens_b}
    end
  end

  defp parse_file(lst) do
    {:parse_a, _, res} =
      List.foldl(lst, {:parse_a, %ClawMachine{}, []}, fn txt, {parse_key, machine, acc} ->
        case parse_key do
          :parse_a ->
            {:ok, a} = parse_a(txt)
            {:parse_b, %{machine | a: a}, acc}

          :parse_b ->
            {:ok, b} = parse_b(txt)
            {:parse_price, %{machine | b: b}, acc}

          :parse_price ->
            {:ok, p} = parse_price(txt)
            {:parse_a, %ClawMachine{}, [%{machine | price: p}] ++ acc}
        end
      end)

    res
  end

  def part1 do
    tokens_a = 3
    tokens_b = 1
    input = input_file() |> read_file() |> parse_file()

    total_tokens =
      List.foldl(input, 0, fn m, acc ->
        case get_num_token(m.a, tokens_a, m.b, tokens_b, m.price) do
          {:ok, num_tokens} -> acc + num_tokens
          _ -> acc
        end
      end)

    IO.puts("Tokens needed to win all posible prizes: #{total_tokens}")
  end

  def part2 do
    tokens_a = 3
    tokens_b = 1
    input = input_file() |> read_file() |> parse_file()

    total_tokens =
      List.foldl(input, 0, fn m, acc ->
        case get_fixed_num_token(m.a, tokens_a, m.b, tokens_b, m.price) do
          {:ok, num_tokens} -> acc + num_tokens
          _ -> acc
        end
      end)

    IO.puts("Tokens needed to win all posible prizes (fixed): #{total_tokens}")
  end
end
