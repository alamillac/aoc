defmodule AOC.D12 do
  defp input_file(), do: Util.get_input_filename(12)
  # defp input_file(), do: Util.get_test_filename(12)

  defp read_file(file) do
    File.read!(file) |> String.split("\n") |> Enum.filter(fn s -> s != "" end)
  end

  defp to_tuple(string, acc) do
    case string do
      <<>> ->
        acc |> Enum.reverse() |> List.to_tuple()

      <<h::binary-size(1), t::binary>> ->
        to_tuple(t, [h] ++ acc)
    end
  end

  defp to_tuple(string) do
    to_tuple(string, [])
  end

  defp to_matrix(lst) do
    lst |> Enum.map(fn s -> to_tuple(s) end) |> List.to_tuple()
  end

  defp max_i(map) do
    tuple_size(map) - 1
  end

  defp max_j(map) do
    tuple_size(elem(map, 0)) - 1
  end

  defp get_value(map, {i, j}) do
    map |> elem(i) |> elem(j)
  end

  defp get_new_point({i, j}, map) do
    if j + 1 <= max_j(map) do
      {:ok, {i, j + 1}}
    else
      if i + 1 <= max_i(map) do
        {:ok, {i + 1, 0}}
      else
        {:done, nil}
      end
    end
  end

  defp is_valid_pos(map, {i, j}) do
    i <= max_i(map) and j <= max_j(map) and i >= 0 and j >= 0
  end

  defp is_from_same_region(map, {i, j}, type) do
    if not is_valid_pos(map, {i, j}) do
      false
    else
      get_value(map, {i, j}) == type
    end
  end

  defp eval_perimeter(map, pos, type, region_points, pending_points, acc) do
    if is_from_same_region(map, pos, type) do
      if not MapSet.member?(region_points, pos) do
        region_points = MapSet.put(region_points, pos)
        pending_points = [pos] ++ pending_points
        {region_points, pending_points, acc}
      else
        {region_points, pending_points, acc}
      end
    else
      {region_points, pending_points, acc + 1}
    end
  end

  defp get_perimeter(map, {i, j}, plant_type, region_points, acc, pending_points) do
    region_points = MapSet.put(region_points, {i, j})
    {region_points, pending_points, acc} =
      eval_perimeter(map, {i - 1, j}, plant_type, region_points, pending_points, acc)

    {region_points, pending_points, acc} =
      eval_perimeter(map, {i + 1, j}, plant_type, region_points, pending_points, acc)

    {region_points, pending_points, acc} =
      eval_perimeter(map, {i, j - 1}, plant_type, region_points, pending_points, acc)

    {region_points, pending_points, acc} =
      eval_perimeter(map, {i, j + 1}, plant_type, region_points, pending_points, acc)

    case pending_points do
      [] -> {region_points, acc}
      [h | t] -> get_perimeter(map, h, plant_type, region_points, acc, t)
    end
  end

  defp get_perimeter(map, {i, j}) do
    get_perimeter(map, {i, j}, get_value(map, {i, j}), MapSet.new(), 0, [])
  end

  defp get_price_of_region(map, {i, j}) do
    {region_points, perimeter} = get_perimeter(map, {i, j})
    area = MapSet.size(region_points)
    price = area * perimeter
    {region_points, price}
  end

  defp get_price(map, pos, visited_points, acc) do
    {visited_points, acc} =
      if MapSet.member?(visited_points, pos) do
        {visited_points, acc}
      else
        {region_points, price} = get_price_of_region(map, pos)
        visited_points = MapSet.union(visited_points, region_points)
        acc = acc + price
        {visited_points, acc}
      end

    case get_new_point(pos, map) do
      {:ok, new_point} -> get_price(map, new_point, visited_points, acc)
      {:done, _} -> acc
    end
  end

  defp get_price(map) do
    get_price(map, {0, 0}, MapSet.new(), 0)
  end

  def part1 do
    map =
      input_file() |> read_file() |> to_matrix()

    price = get_price(map)
    IO.puts("Total price of fencing all regions: #{price}")
  end
end
