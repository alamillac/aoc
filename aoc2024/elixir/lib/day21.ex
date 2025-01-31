defmodule AOC.D21 do
  use Memoize

  defp input_file(), do: Util.get_input_filename(21)
  # defp input_file(), do: Util.get_test_filename(21)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp numeric_keypad() do
    keypad = %{
      ?7 => {0, 0},
      ?8 => {1, 0},
      ?9 => {2, 0},
      ?4 => {0, 1},
      ?5 => {1, 1},
      ?6 => {2, 1},
      ?1 => {0, 2},
      ?2 => {1, 2},
      ?3 => {2, 2},
      ?0 => {1, 3},
      ?A => {2, 3}
    }

    invalid = {0, 3}
    start_pos = {2, 3}
    {keypad, invalid, start_pos}
  end

  defp directional_keypad() do
    keypad = %{
      ?^ => {1, 0},
      ?A => {2, 0},
      ?< => {0, 1},
      ?v => {1, 1},
      ?> => {2, 1}
    }

    invalid = {0, 0}
    start_pos = {2, 0}
    {keypad, invalid, start_pos}
  end

  defp next_pos({i0, j0}, {i1, j1}, invalid) do
    directions = [
      {i0 > i1 and {i1 + 1, j1} != invalid, {i1 + 1, j1}, ?>},
      {i0 < i1 and {i1 - 1, j1} != invalid, {i1 - 1, j1}, ?<},
      {j0 > j1 and {i1, j1 + 1} != invalid, {i1, j1 + 1}, ?v},
      {j0 < j1 and {i1, j1 - 1} != invalid, {i1, j1 - 1}, ?^}
    ]

    for {true, n_pos, action} <- directions do
      {n_pos, action}
    end
  end

  defp get_actions(key_pos, pos, _invalid, acc) when key_pos == pos do
    [[?A | acc]]
  end

  defp get_actions(key_pos, pos, invalid, acc) do
    next_pos(key_pos, pos, invalid)
    |> Enum.flat_map(fn {n_pos, action} ->
      get_actions(key_pos, n_pos, invalid, [action | acc])
    end)
  end

  defmemo get_actions(key_pos, pos, invalid) do
    get_actions(key_pos, pos, invalid, [])
  end

  defp find_actions(_keypad, [], acc) do
    Enum.map(acc, fn ac ->
      Enum.reverse(ac)
    end)
  end

  defp find_actions({keypad_map, invalid, pos}, [c | code], acc) do
    key_pos = Map.get(keypad_map, c)
    actions_lst = get_actions(key_pos, pos, invalid)

    acc =
      for actions <- actions_lst, ac <- acc do
        actions ++ ac
      end

    find_actions({keypad_map, invalid, key_pos}, code, acc)
  end

  defp find_actions(codes_lst, keypad) do
    Enum.map(codes_lst, fn codes ->
      Enum.flat_map(codes, fn code ->
        find_actions(keypad, code, [[]])
      end)
    end)
  end

  defp get_short_actions(actions_lst) do
    Enum.reduce(actions_lst, fn action, acc ->
      if length(action) < length(acc) do
        action
      else
        acc
      end
    end)
  end

  defp get_complexity(codes, actions) do
    Enum.zip_reduce(codes, actions, 0, fn code, action, acc ->
      num_code = code |> List.to_string() |> String.replace(~r/\D/, "") |> String.to_integer()
      acc + num_code * length(action)
    end)
  end

  def part1 do
    codes = input_file() |> read_file() |> Enum.map(fn c -> [String.to_charlist(c)] end)
    num_keypad = numeric_keypad()
    dir_keypad = directional_keypad()

    actions =
      find_actions(codes, num_keypad)
      |> find_actions(dir_keypad)
      |> find_actions(dir_keypad)
      |> Enum.map(fn a ->
        get_short_actions(a)
      end)

    sum_complexities = get_complexity(codes, actions)
    IO.puts("Sum of complexities #{sum_complexities}")
  end
end
