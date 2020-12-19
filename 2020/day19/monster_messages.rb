#!/usr/bin/env ruby

def collapse(input)
  if last = input.find { |x| x.kind_of?(String) }
    idx = input.index(last)
    remaining = input.drop(idx)
    contiguous = remaining.take_while { |x| x.kind_of?(String) }
    (input.take(idx) + [contiguous.join] + collapse(remaining.drop(contiguous.size)))
  else
    input
  end
end

class Path
  def self.from(input)
    if(input.kind_of?(Array) && input.all? { |x| x.kind_of?(String)})
      Path.new(input.join)
    elsif input.kind_of?(Array) && input.any? { |x| x.kind_of?(String) }
      Path.new(collapse(input))
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

  def self.from(id, alts)
    if alts.all? { |x| x.terminal? && x.seq.size >= 5 }
      Rule.new(id, [Path.new("(%s)" % alts.map(&:seq).join("|"))])
    else
      Rule.new(id, alts)
    end
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

    Rule.from(id, alts)
  end

  def references
    choices.flat_map(&:seq).select { |x| x.kind_of?(Integer) }
  end

  def inspect
    "#<Rule:%d %p>" % [@id, @choices]
  end
end

def simplify(rules)
  30.times do |depth|
    while terminal = rules.find { |r| r.terminal?(depth) }
      break if rules.size == 1
      rules.delete(terminal)
      rules = rules.map { |r| r.substitute(terminal) }
    end
  end

  rules
end

def matching?(rules, msg)
  #index = rules.each_with_object({}) { |acc,x| acc[x.id] = x }

  r = rules.find { |r| r.id == 0 }
  Regexp.compile("^%s$" % r.choices.map(&:seq).first) =~ msg
end

def production(rules)
  rule = rules.first

  subs = rule
  refs = rule.references
  (refs.map { |id| rules.find { |r| r.id == id }}).
    each { |x| subs = subs.substitute(x) }

  #p({count: rules.size, rule: rule, subs: subs})
  rules[0] = subs

  reject = []
  refs.each do |ref|
    if rules.none? { |r| r.references.include?(ref) }
      reject << ref
    end
  end

  rules.reject { |x| reject.include?(x.id) }
end

def first_star(rules, messages)
  puts "Initial rules: %d" % [rules.size]
  rules = simplify(rules)
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
  first_star(rules.dup, messages)

  # 100.times do |i|
  #   rules = production(rules.sort_by(&:id).dup)
  # end

end

parse("input")
