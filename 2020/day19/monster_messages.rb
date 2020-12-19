#!/usr/bin/env ruby

class Path
  def self.from(input)
    if(input.kind_of?(Array) && input.all? { |x| x.kind_of?(String)})
      Path.new(input.join)
    else
      Path.new(input)
    end
  end

  attr_reader :seq
  def initialize(seq)
    @seq = seq
  end

  def terminal?
    @seq.is_a?(String)
  end

  def can_substitute?(id)
    !terminal? && @seq.include?(id)
  end

  def substitute(id, value)
    if(!terminal?)
      Path.from(seq.flat_map { |x| x == id ? value : x })
    else
      self
    end
  end

  def inspect
    "#Path:%p" % [seq]
  end
end

class Rule
  attr_reader :id, :choices
  def initialize(id, choices)
    @id, @choices = id, choices
  end

  def terminal?(n = 1)
    choices.all?(&:terminal?) && choices.size <= n
  end

  def substitute(rule)
    alts = choices.flat_map do |path|
      if(path.can_substitute?(rule.id))
        Array(rule.choices).map do |rpath|
          path.substitute(rule.id, rpath.seq)
        end
      else
        [path]
      end
    end

    Rule.new(id, alts)
  end
end

def simplify(rules)
  while terminal = rules.find { |r| r.terminal?(10_000000) }
    break if rules.size == 1
    rules.delete(terminal)
    rules = rules.map { |r| r.substitute(terminal) }
  end

  rules
end

def matching?(rules, msg)
  #index = rules.each_with_object({}) { |acc,x| acc[x.id] = x }

  r = rules.find { |r| r.id == 0 }
  r.choices.map(&:seq).include?(msg)
end

def parse(file)
  lines = IO.readlines(file)

  rules = lines.take_while { |line| line.match(/\d+:/) }.map do |line|
    m = line.match(/(\d+): (.*)$/)
    choices = if terminal = m[2].match(/"(.)"/)
      [Path.new(terminal[1])]
    else
      m[2].split("|").map do |r|
        Path.from(r.chomp.split(" ").map(&:to_i))
      end
    end

    Rule.new(m[1].to_i, choices)
  end

  messages = lines.drop(rules.size + 1).map(&:chomp)

  puts "Initial rules: %d" % [rules.size]
  rules = simplify(rules.dup)
  puts "Simplified rules: %d" % [rules.size]

  if(rules.size == 1)
    choices = rules.first.choices
    puts "Choices: %d" % [choices.size]
  end

  if(rules.size > 3 || rules.size == 1 && rules[0].choices.size < 32)
    puts rules.sort_by(&:id).map(&:inspect).join("\n")
  end

  count = 0
  messages.each do |msg|
    if matching?(rules, msg)
      puts msg
      count += 1
    end
  end

  puts "First Star: #{count}"
end

parse("input")
