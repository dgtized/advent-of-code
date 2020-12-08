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

    IO.puts("First Star")
    run(program)
    IO.puts("Second Star")
    permute(program, 1)
  end

  def to_value(arg) do
    {value, ""} = Integer.parse(arg)
    value
  end

  def run(program) do
    [terminates, acc, nip] = step(program, 0, 0, %{}, 0)

    if terminates do
      IO.puts("Terminates with acc:#{acc} jumping to #{nip}")
    else
      IO.puts("Infinite loop, acc:#{acc} jumping to #{nip}")
    end
  end

  def permute(program, offset) do
    # IO.inspect(["permute", offset])

    change =
      Enum.reverse(program)
      |> Enum.drop(offset)
      |> Enum.take_while(fn [instruction, _] -> instruction == "acc" end)
      |> length

    next_change = offset + change

    [terminates, _, _] = step(swap(program, next_change), 0, 0, %{}, 0)

    if terminates do
      IO.puts("Permuting @ #{offset}")
      run(swap(program, next_change))
    else
      permute(program, next_change + 1)
    end
  end

  def swap(program, change) do
    offset = length(program) - change
    [instruction, arg] = Enum.at(program, offset)

    s =
      if instruction == "nop" do
        "jmp"
      else
        "nop"
      end

    # IO.puts("changing to #{s} at #{offset}")
    List.replace_at(program, offset, [s, arg])
  end

  def step(program, ip, acc, history, order) do
    [instruction, arg] = Enum.at(program, ip)

    # IO.inspect([instruction, arg, ip, acc, order])
    [nip, nacc] = interpret(instruction, arg, ip, acc)
    seen = Map.get(history, nip)

    cond do
      # infinite loop
      seen ->
        [false, acc, nip]

      # terminates
      nip >= length(program) ->
        [true, acc, nip]

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
