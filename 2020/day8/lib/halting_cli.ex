# mix escript.build && ./halting input
defmodule Halting.CLI do
  def main(args) do
    program =
      File.read!(args)
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn line ->
        [instruction, arg] = String.trim(line) |> String.split(" ")
        {value, ""} = Integer.parse(arg)
        [instruction, value]
      end)

    IO.puts("First Star")
    run(program)
    IO.puts("Second Star")
    permute(program, 1)
  end

  def run(program) do
    [terminates, acc, nip] = step(program)

    if terminates do
      IO.puts("Terminates with acc:#{acc} jumping to #{nip}")
    else
      IO.puts("Infinite loop, acc:#{acc} jumping to #{nip}")
    end
  end

  def permute(program, offset) do
    change =
      Enum.reverse(program)
      |> Enum.drop(offset)
      |> Enum.take_while(fn [instruction, _] -> instruction == "acc" end)
      |> length

    next_change = offset + change
    [terminates, _, _] = step(swap(program, next_change))

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

    List.replace_at(program, offset, [s, arg])
  end

  def step(program) do
    step(program, 0, 0, %{}, 0)
  end

  def step(program, ip, acc, history, order) do
    [instruction, arg] = Enum.at(program, ip)
    [nip, nacc] = interpret(instruction, arg, ip, acc)
    seen = Map.get(history, nip)

    cond do
      seen ->
        # infinite loop
        [false, acc, nip]

      nip >= length(program) ->
        # terminates
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
