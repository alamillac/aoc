defmodule AOC.D24 do
  defp input_file(), do: Util.get_input_filename(24)
  # defp input_file(), do: Util.get_test_filename(24)

  defp read_file(file) do
    File.read!(file)
    |> String.split("\n")
    |> Enum.filter(fn s -> s != "" end)
  end

  defp parse_var(str) do
    re = ~r/^(?<var>[xy]\d+): (?<val>[01])$/
    res = Regex.named_captures(re, str)

    if res == nil do
      {nil, :not_found}
    else
      val = String.to_integer(res["val"])
      {{res["var"], val}, :ok}
    end
  end

  defp parse_op(str) do
    re = ~r/^(?<v1>[\d\w]{3}) (?<op>OR|XOR|AND) (?<v2>[\d\w]{3}) -> (?<res>[\d\w]{3})$/
    res = Regex.named_captures(re, str)

    if res == nil do
      raise "Failed parsing"
    else
      {res["op"], res["v1"], res["v2"], res["res"]}
    end
  end

  defp parse_file([], acc) do
    acc
  end

  defp parse_file([h | t], {vars, ops}) do
    case parse_var(h) do
      {{k, v}, :ok} -> parse_file(t, {Map.put(vars, k, v), ops})
      _ -> parse_file(t, {vars, [parse_op(h) | ops]})
    end
  end

  defp parse_file(file_lst) do
    parse_file(file_lst, {Map.new(), []})
  end

  defp operate(vars, [], []) do
    vars
  end

  defp operate(vars, [], lst) when lst != [] do
    operate(vars, lst, [])
  end

  defp operate(vars, [{op, k1, k2, res} = h | t], lst) do
    if Map.has_key?(vars, k1) and Map.has_key?(vars, k2) do
      v1 = Map.get(vars, k1)
      v2 = Map.get(vars, k2)

      case op do
        "OR" ->
          val = Bitwise.bor(v1, v2)
          operate(Map.put(vars, res, val), t, lst)

        "XOR" ->
          val = Bitwise.bxor(v1, v2)
          operate(Map.put(vars, res, val), t, lst)

        "AND" ->
          val = Bitwise.band(v1, v2)
          operate(Map.put(vars, res, val), t, lst)

        _ ->
          raise "Invalid OP"
      end
    else
      operate(vars, t, [h | lst])
    end
  end

  defp operate(vars, op) do
    operate(vars, op, [])
  end

  defp filter_res({k, _v}) do
    String.starts_with?(k, "z")
  end

  defp to_res([], acc) do
    acc
  end

  defp to_res([{k, val} | t], acc) do
    b = String.replace_prefix(k, "z", "") |> String.to_integer()
    acc = acc + Integer.pow(2, b) * val
    to_res(t, acc)
  end

  defp to_res(z_res) do
    to_res(z_res, 0)
  end

  defp get_res(vars) do
    vars |> Map.to_list() |> Enum.filter(&filter_res/1) |> to_res()
  end

  def part1 do
    {vars, op} = input_file() |> read_file() |> parse_file()
    out = operate(vars, op) |> get_res()

    IO.puts("Output on the wires starting with z #{out}")
  end
end
