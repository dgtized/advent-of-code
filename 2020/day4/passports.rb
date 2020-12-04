#!/usr/bin/env ruby

passports = IO.read("input").split("\n\n").map do |raw|
  passport = {}
  raw.scan(/([a-z]{3}):(\S+)/).each { |m|
    passport[m[0]] = m[1]
  }
  passport
end

expected = %w{byr iyr eyr hgt hcl ecl pid cid}

valid = passports.select do |passport|
  diff = expected - passport.keys
  diff.empty? || diff == ["cid"]
end

puts "Valid Passports: ", valid.size
