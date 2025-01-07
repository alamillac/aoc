defmodule AOC.D14 do
  defp input_file(), do: {Util.get_input_filename(14), {101, 103}}
  # defp input_file(), do: {Util.get_test_filename(14), {11, 7}}

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp parse(txt) do
    re = ~r/^p=(?<p1>\d+),(?<p2>\d+) v=(?<v1>-?\d+),(?<v2>-?\d+)$/
    res = Regex.named_captures(re, txt)

    if res == nil do
      {:error, "Not possible to parse"}
    else
      {:ok,
       %{
         p: {
           String.to_integer(res["p1"]),
           String.to_integer(res["p2"])
         },
         v: {
           String.to_integer(res["v1"]),
           String.to_integer(res["v2"])
         }
       }}
    end
  end

  defp parse_file(file) do
    lst = read_file(file)

    Enum.map(lst, fn el ->
      {:ok, r} = parse(el)
      r
    end)
  end

  defp simulate(robots, size, secs) do
    {w, h} = size

    Enum.map(robots, fn r ->
      {v0, v1} = r.v
      {p0, p1} = r.p

      p0 = Integer.mod(p0 + v0 * secs, w)
      p1 = Integer.mod(p1 + v1 * secs, h)

      p0 =
        if p0 < 0 do
          w - p0
        else
          p0
        end

      p1 =
        if p1 < 0 do
          h - p1
        else
          p1
        end

      %{r | p: {p0, p1}}
    end)
  end

  defp get_safety_factor(robots, {w, h}) do
    mid_w = Float.floor(w / 2)
    mid_h = Float.floor(h / 2)

    {c1, c2, c3, c4} =
      List.foldl(robots, {0, 0, 0, 0}, fn r, {c1, c2, c3, c4} ->
        {p0, p1} = r.p

        if p0 == mid_w or p1 == mid_h do
          {c1, c2, c3, c4}
        else
          case {p0 < mid_w, p1 < mid_h} do
            {true, true} -> {c1 + 1, c2, c3, c4}
            {true, false} -> {c1, c2 + 1, c3, c4}
            {false, true} -> {c1, c2, c3 + 1, c4}
            {false, false} -> {c1, c2, c3, c4 + 1}
          end
        end
      end)

    c1 * c2 * c3 * c4
  end

  defp map(robots) do
    List.foldl(robots, %{}, fn r, acc ->
      val = Map.get(acc, r.p, 0)
      Map.put(acc, r.p, val + 1)
    end)
  end

  defp draw_map(map_robots, {w, h}) do
    i_range = 0..(w - 1)
    j_range = 0..(h - 1)

    Enum.each(j_range, fn j ->
      Enum.each(i_range, fn i ->
        val = Map.get(map_robots, {i, j})

        if val == nil do
          IO.write(" ")
        else
          IO.write(".")
        end
      end)

      IO.write("\n")
    end)
  end

  defp single_robot_pos(robots, size, secs_init, secs_end) do
    Stream.unfold(secs_init, fn
      secs when secs == secs_end ->
        nil

      secs ->
        m = simulate(robots, size, secs) |> map()
        {{secs, m}, secs + 1}
    end)
    |> Stream.filter(fn {_secs, map_robots} ->
      Map.values(map_robots) |> Enum.max() == 1
    end)
  end

  def part1 do
    {file, size} = input_file()
    robots = parse_file(file)
    secs = 100
    factor = simulate(robots, size, secs) |> get_safety_factor(size)
    IO.puts("Safety factor after #{secs} seconds: #{factor}")
  end

  def part2 do
    {file, size} = input_file()
    robots = parse_file(file)

    # secs_range = 0..10000
    # Enum.each(secs_range, fn secs ->
    #   IO.puts("Map from secs #{secs}")
    #   simulate(robots, size, secs) |> map() |> draw_map(size)
    #   IO.write("\n")
    #   Process.sleep(100)
    # end)

    single_robot_pos(robots, size, 0, 10000)
    |> Enum.each(fn {secs, map} ->
      IO.puts("Map from secs #{secs}")
      draw_map(map, size)
      IO.write("\n")
      Process.sleep(100)
    end)
  end
end
