#!/usr/bin/env ruby

# usage: ruby dive.rb input

filename = ARGV[0] || "input"

cmds = IO.readlines(filename).map do |line|
  m = line.match(/(\w+) (\d+)/)
  [m[1], m[2].to_i]
end

horizontal = 0
depth = 0
cmds.each do |cmd,amt|
  case cmd
  when /forward/
    horizontal += amt
  when /down/
    depth += amt
  when /up/
    depth -= amt
  end
end

puts horizontal
puts depth

puts horizontal * depth
