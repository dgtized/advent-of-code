# mix escript.build && ./halting input
defmodule Halting.CLI do
  def main(args) do
    program =
      File.read!(args)
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn line ->
        [instruction, arg] = String.trim(line) |> String.split(" ")
        [instruction, to_value(arg)]
      end)

    run(program)
  end

  def to_value(arg) do
    {value, ""} = Integer.parse(arg)
    value
  end

  def run(program) do
    step(program, 0, 0, %{}, 0)
  end

  def step(program, ip, acc, history, order) do
    [instruction, arg] = Enum.at(program, ip)

    IO.inspect([instruction, arg, ip, acc, order])
    [nip, nacc] = interpret(instruction, arg, ip, acc)
    seen = Map.get(history, nip)

    cond do
      seen ->
        IO.puts("Infinite loop, acc:#{acc} jumping to #{nip}")
        false

      nip >= length(program) ->
        IO.puts("Terminates with acc:#{acc} jumping to #{nip}")
        true

      true ->
        step(program, nip, nacc, Map.put(history, ip, order), order + 1)
    end
  end

  def interpret(instruction, arg, ip, acc) do
    case instruction do
      "nop" ->
        [ip + 1, acc]

      "acc" ->
        [ip + 1, acc + arg]

      "jmp" ->
        [ip + arg, acc]
    end
  end
end
