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

  defp exec_program(program, _reg_a, _reg_b, _reg_c, output_buffer, instruction_pointer)
       when instruction_pointer >= tuple_size(program) do
    output_buffer |> Enum.reverse() |> Enum.join(",")
  end

  defp exec_program(program, reg_a, reg_b, reg_c, output_buffer, instruction_pointer) do
    opcode = elem(program, instruction_pointer)
    operand = elem(program, instruction_pointer + 1)

    {reg_a, reg_b, reg_c, output_buffer, instruction_pointer} =
      run_instruction(opcode, operand, reg_a, reg_b, reg_c, output_buffer, instruction_pointer)

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

  defp run_instruction(opcode, operand, reg_a, reg_b, reg_c, output_buffer, instruction_pointer) do
    case opcode do
      0 ->
        # adv
        reg_a = trunc(reg_a / 2 ** get_combo_operand(operand, reg_a, reg_b, reg_c))
        {reg_a, reg_b, reg_c, output_buffer, instruction_pointer + 2}

      1 ->
        # bxl
        reg_b = Bitwise.bxor(reg_b, operand)
        {reg_a, reg_b, reg_c, output_buffer, instruction_pointer + 2}

      2 ->
        # bst
        reg_b = Integer.mod(get_combo_operand(operand, reg_a, reg_b, reg_c), 8)
        {reg_a, reg_b, reg_c, output_buffer, instruction_pointer + 2}

      3 ->
        # jnz
        if reg_a == 0 do
          {reg_a, reg_b, reg_c, output_buffer, instruction_pointer + 2}
        else
          instruction_pointer = operand
          {reg_a, reg_b, reg_c, output_buffer, instruction_pointer}
        end

      4 ->
        # bxc
        reg_b = Bitwise.bxor(reg_b, reg_c)
        {reg_a, reg_b, reg_c, output_buffer, instruction_pointer + 2}

      5 ->
        # out
        output = Integer.mod(get_combo_operand(operand, reg_a, reg_b, reg_c), 8)
        {reg_a, reg_b, reg_c, [output | output_buffer], instruction_pointer + 2}

      6 ->
        # bdv
        reg_b = trunc(reg_a / 2 ** get_combo_operand(operand, reg_a, reg_b, reg_c))
        {reg_a, reg_b, reg_c, output_buffer, instruction_pointer + 2}

      7 ->
        # cdv
        reg_c = trunc(reg_a / 2 ** get_combo_operand(operand, reg_a, reg_b, reg_c))
        {reg_a, reg_b, reg_c, output_buffer, instruction_pointer + 2}

      _ ->
        raise "Invalid opcode"
    end
  end

  def part1 do
    {reg_a, reg_b, reg_c, program} = input_file() |> read_file() |> parse_file()
    output = exec_program(program, reg_a, reg_b, reg_c, [], 0)

    IO.puts("Program output: #{output}")
  end
end
