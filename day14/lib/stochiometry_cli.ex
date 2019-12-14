# mix escript.build && ./stochiometry input

defmodule Stochiometry.CLI do
  def main(args) do
    conversions = load_converions(args)
    IO.inspect(conversions)

    IO.inspect(calc(conversions, [{"FUEL", 1}], []))
  end

  def load_converions(args) do
    File.read!(args)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [ingredients | [result | _]] = String.trim(line) |> String.split(" => ")

      deps =
        String.split(ingredients, ",")
        |> Enum.map(&split_part/1)

      {n, c} = split_part(result)
      %{n => {c, deps}}
    end)
    |> Enum.reduce(&Map.merge/2)
  end

  def calc(conversions, [{source, requested} | remainder], residuals) do
    IO.inspect({[source, requested], residuals})

    if source == "ORE" do
      requested + calc(conversions, remainder, residuals)
    else
      {count, deps} = conversions[source]

      batches = Float.ceil(requested / count)

      next =
        Enum.map(deps, fn {dep, required} ->
          {dep, batches * required}
        end)

      calc(conversions, combine(remainder ++ next), residuals)
    end
  end

  def calc(conversions, [], residuals) do
    0
  end

  def combine(lst) do
    IO.inspect(["combine", lst])

    r =
      Enum.group_by(lst, fn {n, c} ->
        n
      end)
      |> Map.values()
      |> Enum.map(fn similar ->
        {name, _} = List.first(similar)
        {name, Enum.reduce(similar, 0, fn {_, v}, acc -> acc + v end)}
      end)

    IO.inspect(["combine", r])

    r
  end

  def split_part(part) do
    [count | [name | _]] = String.trim(part) |> String.split(" ")
    {count, _} = Integer.parse(count)
    {name, count}
  end
end
