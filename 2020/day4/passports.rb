#!/usr/bin/env ruby

# usage: ruby passports.rb input
filename = ARGV[0] || "input"

passports = IO.read(filename).split("\n\n").map do |raw|
  passport = {}
  raw.scan(/([a-z]{3}):(\S+)/).each do |m|
    passport[m[0]] = m[1]
  end
  passport
end

printf "Passports: %d\n", passports.size

expected = %w[byr iyr eyr hgt hcl ecl pid cid]

valid_by_fields = passports.select do |passport|
  diff = expected - passport.keys
  diff.empty? || diff == ['cid']
end

printf "Valid Passports: %d\n", valid_by_fields.size

fully_valid = valid_by_fields.select do |passport|
  conditions = []

  byr = passport['byr'].match(/(\d{4})/)[1].to_i
  conditions << (byr >= 1920 && byr <= 2002)

  iyr = passport['iyr'].match(/(\d{4})/)[1].to_i
  conditions << (iyr >= 2010 && iyr <= 2020)

  eyr = passport['eyr'].match(/(\d{4})/)[1].to_i
  conditions << (eyr >= 2020 && eyr <= 2030)

  conditions << (
    m = passport['hgt'].match(/(\d+)(cm|in)/)
    if m
      hgt = m[1].to_i
      if m[2] == 'cm'
        hgt >= 150 && hgt <= 193
      elsif m[2] == 'in'
        hgt >= 59 && hgt <= 76
      end
    end
  )

  conditions << passport['hcl'].match(/#[0-9a-f]{6}$/)

  conditions << %w[amb blu brn gry grn hzl oth].include?(passport['ecl'])

  # Lost ~15 minutes on the single example with >9 digits because I forgot ^$
  # bounds on my first stab at the condition
  conditions << passport['pid'].match(/^\d{9}$/)

  conditions.all?
end

printf "Fully Validated %d\n", fully_valid.size
