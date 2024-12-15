defmodule Day11 do
  @file_path "data/task.data"

  defp read_data(file_path) do
    case File.read(file_path) do
      {:ok, content} ->
        [first_line | _] = String.split(content, "\n")
        first_line |> String.split(" ") |> Enum.map(&String.to_integer/1)

      {:error, reason} ->
        IO.puts("Failed to read file: #{reason}")
        {:error, reason}
    end
  end

  def solve_like_noob(iterations) do
    data = read_data(@file_path)

    result =
      Enum.reduce(1..iterations, data, fn _, acc -> acc |> Enum.flat_map(&replace_naïve/1) end)

    length(result)
  end

  def solve_like_master_tought_me(iterations) do
    data = read_data(@file_path)

    init =
      Enum.reduce(data, %{}, fn val, acc ->
        acc |> Map.put(val, 1)
      end)

    Enum.reduce(1..iterations, init, fn _, acc ->
      Enum.reduce(acc, %{}, &replace_len/2)
    end)
    |> Enum.reduce(0, fn {_, v}, acc -> acc + v end)
  end

  defp even_digits(number) do
    number |> digits_len() |> rem(2) == 0
  end

  defp digits_len(number) do
    1 + (number |> :math.log10() |> floor())
  end

  def split_number(number) when is_integer(number) do
    half_len = div(digits_len(number), 2)
    divider = 10 ** half_len
    {div(number, divider), rem(number, divider)}
  end

  def replace_naïve(val) do
    cond do
      val == 0 ->
        [1]

      even_digits(val) ->
        digits = Integer.digits(val)
        half_len = div(length(digits), 2)
        {first_half, second_half} = Enum.split(digits, half_len)
        [first_half |> Integer.undigits(), second_half |> Integer.undigits()]

      {true} ->
        [val * 2024]
    end
  end

  def replace_len(val, acc) do
    add = fn count -> fn v -> v + count end end

    case val do
      {0, count} ->
        acc |> Map.update(1, count, add.(count))

      {n, count} ->
        if even_digits(n) do
          {left, right} = split_number(n)

          acc
          |> Map.update(left, count, add.(count))
          |> Map.update(right, count, add.(count))
        else
          acc |> Map.update(n * 2024, count, add.(count))
        end
    end
  end
end

IO.inspect(Day11.solve_like_noob(25))
IO.inspect(Day11.solve_like_master_tought_me(75))
