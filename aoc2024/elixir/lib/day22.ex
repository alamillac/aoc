defmodule AOC.D22 do
  defp input_file(), do: Util.get_input_filename(22)
  # defp input_file(), do: Util.get_test_filename(22)

  defp read_file(file) do
    File.read!(file)
    |> String.split("\n")
    |> Enum.filter(fn s -> s != "" end)
    |> Enum.map(fn e -> String.to_integer(e) end)
  end

  defp mix(val, secret) do
    Bitwise.bxor(val, secret)
  end

  defp prune(val) do
    Integer.mod(val, 16_777_216)
  end

  defp get_next_secret(secret) do
    p1 = (secret * 64) |> mix(secret) |> prune()
    p2 = (p1 / 32) |> Float.floor() |> trunc() |> mix(p1) |> prune()
    p3 = (p2 * 2048) |> mix(p2) |> prune()
    p3
  end

  defp get_next_secret(secret, ith) do
    1..ith
    |> Enum.reduce(secret, fn _, acc ->
      get_next_secret(acc)
    end)
  end

  defp get_next_secrets(secrets, ith) do
    secrets
    |> Enum.map(fn s ->
      get_next_secret(s, ith)
    end)
  end

  defp get_prices_lst(secret, ith) do
    1..ith
    |> Enum.reduce([secret], fn _, secrets ->
      last_secret = hd(secrets)
      ns = get_next_secret(last_secret)
      [ns | secrets]
    end)
    |> Enum.reverse()
    |> Enum.map(fn s -> Integer.mod(s, 10) end)
  end

  defp get_price_seq(prices_lst) do
    {price_seq, _} =
      prices_lst
      |> List.foldl({[], []}, fn e, acc ->
        case acc do
          {[], []} -> {[], [e]}
          {lst, [e1, e2, e3 | _]} -> {[{e, {e - e1, e1, e2, e3}} | lst], [e - e1, e1, e2, e3]}
          {[], [e1 | t]} -> {[], [e - e1, e1 | t]}
        end
      end)

    price_seq |> Enum.reverse()
  end

  defp get_map_seq([], map_seq) do
    map_seq
  end

  defp get_map_seq([h | t], map_seq) do
    {price, seq} = h

    if Map.has_key?(map_seq, seq) do
      get_map_seq(t, map_seq)
    else
      get_map_seq(t, Map.put(map_seq, seq, price))
    end
  end

  defp get_map_seq(price_seq_lst) do
    price_seq_lst
    |> Enum.map(fn ps -> get_map_seq(ps, Map.new()) end)
  end

  defp reduce_map_seq(map_seq_lst) do
    map_seq_lst
    |> Enum.reduce(0, fn map_seq, max_val ->
      {new_max_val, _} =
        Map.keys(map_seq)
        |> Enum.reduce({max_val, MapSet.new()}, fn k, {max_val, evaluated} ->
          if MapSet.member?(evaluated, k) do
            {max_val, evaluated}
          else
            val =
              map_seq_lst
              |> Enum.reduce(0, fn map_seq, acc ->
                acc + Map.get(map_seq, k, 0)
              end)

            evaluated = MapSet.put(evaluated, k)

            if val > max_val do
              {val, evaluated}
            else
              {max_val, evaluated}
            end
          end
        end)

      new_max_val
    end)
  end

  def part1 do
    initial_secrets = input_file() |> read_file()
    sum_secret_number = get_next_secrets(initial_secrets, 2000) |> Enum.sum()
    IO.puts("Sum of the 2000th secret number generated #{sum_secret_number}")
  end

  def part2 do
    initial_secrets = input_file() |> read_file()

    num_bananas =
      initial_secrets
      |> Enum.map(fn s -> get_prices_lst(s, 2000) end)
      |> Enum.map(fn pl ->
        get_price_seq(pl)
      end)
      |> get_map_seq()
      |> reduce_map_seq()

    IO.puts("Max number of bananas to get #{num_bananas}")
  end
end
