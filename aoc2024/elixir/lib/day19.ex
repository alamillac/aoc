defmodule AOC.D19 do
  defp input_file(), do: Util.get_input_filename(19)
  # defp input_file(), do: Util.get_test_filename(19)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp parse_file([h | designs]) do
    patterns = h |> String.split(", ") |> MapSet.new() |> MapSet.to_list()

    {patterns, designs}
  end

  defp patterns_string(patterns) do
    patterns |> Enum.sort() |> Enum.join("|")
  end

  defp is_design_possible([{"", _patterns} | _t], _) do
    true
  end

  defp is_design_possible([{_design, []} | t], checked) do
    is_design_possible(t, checked)
  end

  defp is_design_possible([], _) do
    false
  end

  defp is_design_possible([{design, patterns} | t], checked) do
    valid_patterns =
      Enum.filter(patterns, fn p ->
        String.contains?(design, p)
      end)

    key_checked = {design, patterns_string(patterns)}

    next =
      Enum.filter(valid_patterns, fn p ->
        String.starts_with?(design, p) and not MapSet.member?(checked, key_checked)
      end)
      |> Enum.map(fn p ->
        design = String.replace(design, ~r"^#{p}", "")
        {design, valid_patterns}
      end)

    checked = MapSet.put(checked, key_checked)
    is_design_possible(next ++ t, checked)
  end

  defp is_design_possible(design, patterns) do
    is_design_possible([{design, patterns}], MapSet.new())
  end

  def part1 do
    {patterns, designs} = input_file() |> read_file() |> parse_file()

    num_possible_designs =
      Enum.map(designs, fn design ->
        if is_design_possible(design, patterns) do
          1
        else
          0
        end
      end)
      |> Enum.sum()

    IO.puts("Number of possible designs: #{num_possible_designs}")
  end
end
