#!/usr/bin/env ruby

class Rule
  attr_reader :id, :choices
  def initialize(id, choices)
    @id, @choices = id, choices
  end

  def terminal?(n = 1)
    choices.all? { |x| x.kind_of?(String) } && choices.size <= n
  end

  def collapse
    if terminal?
      self
    elsif choices.kind_of?(Array)
      Rule.new(
        id,
        choices.map do |path|
          if(path.kind_of?(Array) && path.all? { |x| x.kind_of?(String) })
            path.join
          else
            path
          end
        end
      )
    end
  end

  def substitute(rule)
    if choices.kind_of?(Array)
      alts = []
      choices.each do |path|
        if(path.kind_of?(Array) && path.include?(rule.id))
          Array(rule.choices).each do |rpath|
            alts << path.map { |x| x == rule.id ? rpath : x }
          end
        else
          alts << path
        end
      end
      Rule.new(id, alts)
    else
      self
    end
  end
end

def simplify(rules)
  while terminal = rules.find { |r| r.terminal?(1) }
    break if rules.size == 1
    rules.delete(terminal)
    rules = rules.map { |r| r.substitute(terminal).collapse }
  end

  rules
end

def matching?(rules, msg)
  #index = rules.each_with_object({}) { |acc,x| acc[x.id] = x }

  r = rules.find { |r| r.id == 0 }
  r.choices.include?(msg)
end

def parse(file)
  lines = IO.readlines(file)

  rules = lines.take_while { |line| line.match(/\d+:/) }.map do |line|
    m = line.match(/(\d+): (.*)$/)
    choices = if terminal = m[2].match(/"(.)"/)
      [terminal[1]]
    else
      m[2].split("|").map { |r| r.chomp.split(" ").map(&:to_i)}
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
