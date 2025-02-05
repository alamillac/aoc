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
    1..ith |> Enum.reduce(secret, fn _, acc ->
      get_next_secret(acc)
    end)
  end

  defp get_next_secrets(secrets, ith) do
    secrets
    |> Enum.map(fn s ->
      get_next_secret(s, ith)
    end)
  end

  def part1 do
    initial_secrets = input_file() |> read_file()
    sum_secret_number = get_next_secrets(initial_secrets, 2000) |> Enum.sum()
    IO.puts("Sum of the 2000th secret number generated #{sum_secret_number}")
  end
end
