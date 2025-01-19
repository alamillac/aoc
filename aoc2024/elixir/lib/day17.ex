defmodule AOC.D17 do
  defp input_file(), do: Util.get_input_filename(17)
  # defp input_file(), do: Util.get_test_filename(17)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp parse_file(lst) do
    parse_file(lst, {nil, nil, nil, [], :reg_a})
  end

  defp parse_file([h | t], {reg_a, reg_b, reg_c, program, to_parse}) do
    acc =
      case to_parse do
        :reg_a -> {parse_reg_a(h), reg_b, reg_c, program, :reg_b}
        :reg_b -> {reg_a, parse_reg_b(h), reg_c, program, :reg_c}
        :reg_c -> {reg_a, reg_b, parse_reg_c(h), program, :program}
        :program -> {reg_a, reg_b, reg_c, parse_program(h), nil}
        _ -> raise "Invalid file"
      end

    parse_file(t, acc)
  end

  defp parse_file([], {reg_a, reg_b, reg_c, program, _}) do
    {reg_a, reg_b, reg_c, program}
  end

  defp parse_reg_a(str) do
    re = ~r/^Register A: (?<num>\d+)$/
    res = Regex.named_captures(re, str)

    if res == nil do
      raise "Not possible to parse A"
    else
      String.to_integer(res["num"])
    end
  end

  defp parse_reg_b(str) do
    re = ~r/^Register B: (?<num>\d+)$/
    res = Regex.named_captures(re, str)

    if res == nil do
      raise "Not possible to parse B"
    else
      String.to_integer(res["num"])
    end
  end

  defp parse_reg_c(str) do
    re = ~r/^Register C: (?<num>\d+)$/
    res = Regex.named_captures(re, str)

    if res == nil do
      raise "Not possible to parse C"
    else
      String.to_integer(res["num"])
    end
  end

  defp parse_program(str) do
    re = ~r/^Program: (?<data>[\d,]+)$/
    res = Regex.named_captures(re, str)

    if res == nil do
      raise "Not possible to parse program"
    else
      String.split(res["data"], ",")
      |> Enum.map(fn x -> String.to_integer(x) end)
      |> List.to_tuple()
    end
  end

  defp exec_program(program, reg_a, reg_b, reg_c) do
    exec_program(program, reg_a, reg_b, reg_c, [], 0)
  end

  defp exec_program(program, _reg_a, _reg_b, _reg_c, output_buffer, instruction_pointer)
       when instruction_pointer >= tuple_size(program) do
    output_buffer |> Enum.reverse() |> Enum.join(",")
  end

  defp exec_program(program, reg_a, reg_b, reg_c, output_buffer, instruction_pointer) do
    opcode = elem(program, instruction_pointer)
    operand = elem(program, instruction_pointer + 1)

    {reg_a, reg_b, reg_c, output, instruction_pointer} =
      run_instruction(opcode, operand, reg_a, reg_b, reg_c, instruction_pointer)

    output_buffer =
      if output == nil do
        output_buffer
      else
        [output | output_buffer]
      end

    exec_program(program, reg_a, reg_b, reg_c, output_buffer, instruction_pointer)
  end

  defp get_combo_operand(operand, reg_a, reg_b, reg_c) do
    case operand do
      x when x >= 0 and x <= 3 ->
        # literal value
        x

      4 ->
        reg_a

      5 ->
        reg_b

      6 ->
        reg_c

      _ ->
        raise "Invalid value"
    end
  end

  defp run_instruction(opcode, operand, reg_a, reg_b, reg_c, instruction_pointer) do
    case opcode do
      0 ->
        # adv
        reg_a = trunc(reg_a / 2 ** get_combo_operand(operand, reg_a, reg_b, reg_c))
        {reg_a, reg_b, reg_c, nil, instruction_pointer + 2}

      1 ->
        # bxl
        reg_b = Bitwise.bxor(reg_b, operand)
        {reg_a, reg_b, reg_c, nil, instruction_pointer + 2}

      2 ->
        # bst
        reg_b = Integer.mod(get_combo_operand(operand, reg_a, reg_b, reg_c), 8)
        {reg_a, reg_b, reg_c, nil, instruction_pointer + 2}

      3 ->
        # jnz
        if reg_a == 0 do
          {reg_a, reg_b, reg_c, nil, instruction_pointer + 2}
        else
          instruction_pointer = operand
          {reg_a, reg_b, reg_c, nil, instruction_pointer}
        end

      4 ->
        # bxc
        reg_b = Bitwise.bxor(reg_b, reg_c)
        {reg_a, reg_b, reg_c, nil, instruction_pointer + 2}

      5 ->
        # out
        output = Integer.mod(get_combo_operand(operand, reg_a, reg_b, reg_c), 8)
        {reg_a, reg_b, reg_c, output, instruction_pointer + 2}

      6 ->
        # bdv
        reg_b = trunc(reg_a / 2 ** get_combo_operand(operand, reg_a, reg_b, reg_c))
        {reg_a, reg_b, reg_c, nil, instruction_pointer + 2}

      7 ->
        # cdv
        reg_c = trunc(reg_a / 2 ** get_combo_operand(operand, reg_a, reg_b, reg_c))
        {reg_a, reg_b, reg_c, nil, instruction_pointer + 2}

      _ ->
        raise "Invalid opcode"
    end
  end

  defp print_current_iter(iter) do
    if Integer.mod(iter, 1_000_000) == 0 do
      IO.puts("Current iter: #{iter}")
    end
  end

  defp find_reg_a_by_brute_force(program, reg_b, reg_c) do
    # reg_a_init = 4_167_000_000
    reg_a_init = 0
    find_reg_a_by_brute_force(program, reg_a_init, reg_b, reg_c, 0, 0, reg_a_init)
  end

  defp find_reg_a_by_brute_force(
         program,
         _reg_a,
         reg_b,
         reg_c,
         instruction_pointer,
         output_idx,
         reg_a_init
       )
       when instruction_pointer >= tuple_size(program) do
    if tuple_size(program) == output_idx do
      reg_a_init
    else
      reg_a_init = reg_a_init + 1
      print_current_iter(reg_a_init)
      find_reg_a_by_brute_force(program, reg_a_init, reg_b, reg_c, 0, 0, reg_a_init)
    end
  end

  defp find_reg_a_by_brute_force(
         program,
         reg_a,
         reg_b,
         reg_c,
         instruction_pointer,
         output_idx,
         reg_a_init
       ) do
    opcode = elem(program, instruction_pointer)
    operand = elem(program, instruction_pointer + 1)

    {reg_a, reg_b, reg_c, output, instruction_pointer} =
      run_instruction(opcode, operand, reg_a, reg_b, reg_c, instruction_pointer)

    if output == nil or output == elem(program, output_idx) do
      output_idx =
        if output != nil do
          output_idx + 1
        else
          output_idx
        end

      find_reg_a_by_brute_force(
        program,
        reg_a,
        reg_b,
        reg_c,
        instruction_pointer,
        output_idx,
        reg_a_init
      )
    else
      reg_a_init = reg_a_init + 1
      print_current_iter(reg_a_init)
      find_reg_a_by_brute_force(program, reg_a_init, reg_b, reg_c, 0, 0, reg_a_init)
    end
  end

  defp valid_program(program, reg_a, reg_b, reg_c) do
    valid_program(program, reg_a, reg_b, reg_c, 0, 0)
  end

  defp valid_program(
         program,
         _reg_a,
         _reg_b,
         _reg_c,
         instruction_pointer,
         output_idx
       )
       when instruction_pointer >= tuple_size(program) do
    if tuple_size(program) == output_idx do
      true
    else
      false
    end
  end

  defp valid_program(
         program,
         reg_a,
         reg_b,
         reg_c,
         instruction_pointer,
         output_idx
       ) do
    opcode = elem(program, instruction_pointer)
    operand = elem(program, instruction_pointer + 1)

    {reg_a, reg_b, reg_c, output, instruction_pointer} =
      run_instruction(opcode, operand, reg_a, reg_b, reg_c, instruction_pointer)

    if output == nil or output == elem(program, output_idx) do
      output_idx =
        if output != nil do
          output_idx + 1
        else
          output_idx
        end

      valid_program(
        program,
        reg_a,
        reg_b,
        reg_c,
        instruction_pointer,
        output_idx
      )
    else
      false
    end
  end

  defp find_reg_aux(from, expected_output, program_fn) do
    Stream.iterate(from, fn v -> v + 1 end)
    |> Stream.map(fn reg_a ->
      output = program_fn.(reg_a)
      {reg_a, output}
    end)
    |> Stream.filter(fn {_, output} -> output == expected_output end)
    |> Enum.take(1)
    |> case do
      [{reg_a, _}] -> reg_a
      [] -> raise "Not found"
    end
  end

  defp find_reg(program, reg_b, reg_c) do
    last_idx = tuple_size(program) - 1
    find_reg(program, reg_b, reg_c, last_idx, 0, "")
  end

  defp find_reg(program, reg_b, reg_c, idx, tmp_reg_a, tmp_output) when idx >= 0 do
    program_number = elem(program, idx)
    tmp_output = "#{program_number}#{tmp_output}"

    tmp_reg_a =
      find_reg_aux(tmp_reg_a * 8, tmp_output, fn reg_a ->
        exec_program(program, reg_a, reg_b, reg_c)
      end)

    find_reg(program, reg_b, reg_c, idx - 1, tmp_reg_a, ",#{tmp_output}")
  end

  defp find_reg(_program, _reg_b, _reg_c, idx, reg_a, _tmp_output) when idx < 0 do
    reg_a
  end

  def part1 do
    {reg_a, reg_b, reg_c, program} = input_file() |> read_file() |> parse_file()
    output = exec_program(program, reg_a, reg_b, reg_c)

    IO.puts("Program output: #{output}")
  end

  def part2 do
    {_, reg_b, reg_c, program} = input_file() |> read_file() |> parse_file()
    reg_a = find_reg(program, reg_b, reg_c)

    IO.puts("Register A: #{reg_a}")
  end

  def part2_1 do
    # By brute force with recursion -_(o_o)_-
    {_, reg_b, reg_c, program} = input_file() |> read_file() |> parse_file()
    reg_a = find_reg_a_by_brute_force(program, reg_b, reg_c)

    IO.puts("Register A: #{reg_a}")
  end

  def part2_2 do
    # By brute force -_(o_o)_-
    {_, reg_b, reg_c, program} = input_file() |> read_file() |> parse_file()

    reg_a =
      0
      |> Stream.iterate(&(&1 + 1))
      |> Stream.map(fn reg_a ->
        print_current_iter(reg_a)

        if valid_program(program, reg_a, reg_b, reg_c) do
          {:ok, reg_a}
        else
          {:skip, nil}
        end
      end)
      |> Stream.filter(fn e -> match?({:ok, _}, e) end)
      |> Enum.take(1)
      |> case do
        [{:ok, reg_a}] -> reg_a
        [] -> raise "Not found"
      end

    IO.puts("Register A: #{reg_a}")
  end

  def part2_3 do
    # By brute force -_(o_o)_-
    {_, reg_b, reg_c, program} = input_file() |> read_file() |> parse_file()

    reg_a =
      0
      |> Stream.iterate(&(&1 + 1))
      |> Stream.map(fn reg_a ->
        print_current_iter(reg_a)
        Task.async(fn -> {reg_a, valid_program(program, reg_a, reg_b, reg_c)} end)
      end)
      # This serialize the task. So there is no advantage here
      |> Stream.map(&Task.await/1)
      |> Stream.filter(fn {_reg_a, result} -> result end)
      |> Enum.take(1)
      |> case do
        [{reg_a, true}] -> reg_a
        _ -> raise "Not found"
      end

    IO.puts("Register A: #{reg_a}")
  end

  def part2_4 do
    # By brute force -_(o_o)_-
    {_, reg_b, reg_c, program} = input_file() |> read_file() |> parse_file()
    IO.puts("Schedulers: #{System.schedulers_online()}")

    reg_a =
      0
      |> Stream.iterate(&(&1 + 1))
      |> Task.async_stream(
        fn reg_a ->
          print_current_iter(reg_a)

          if valid_program(program, reg_a, reg_b, reg_c) do
            {:ok, reg_a}
          else
            {:skip, nil}
          end
        end,
        max_concurrency: System.schedulers_online(),
        timeout: :infinity
      )
      |> Stream.filter(fn e -> match?({:ok, {:ok, _}}, e) end)
      |> Enum.take(1)
      |> case do
        [{:ok, {:ok, reg_a}}] -> reg_a
        [] -> raise "Not found"
      end

    IO.puts("Register A: #{reg_a}")
  end
end
