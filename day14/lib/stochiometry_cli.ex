# mix escript.build && ./stochiometry input

defmodule Stochiometry.CLI do
  def main(args) do
    conversions = load_converions(args)
    IO.inspect(conversions)
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

      [split_part(result), deps]
    end)
  end

  def split_part(part) do
    [count | [name | _]] = String.trim(part) |> String.split(" ")
    {count, _} = Integer.parse(count)
    [name, count]
  end
end
