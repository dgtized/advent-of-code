#!/usr/bin/gawk -f

# usage: ./passwords.awk input

{
  split($1, bounds, /-/)
  lower = strtonum(bounds[1])
  upper = strtonum(bounds[2])
  character = gensub(/:/, "", "g", $2)
  password = $3
  rule = gensub("[^" character "]", "", "g", password)
  len = length(rule)
  # printf "%d %d %s %s %s %d\n", lower, upper, character, password, rule, len
  if (len >= lower && len <= upper) {
    valid++
  } else {
    invalid++
  }

  c1 = substr(password, lower, 1) == character
  c2 = substr(password, upper, 1) == character

  # printf "%d %d %s %s %s %s\n", lower, upper, character, password, c1, c2
  # if just one position matches the expected character
  if(c1 + c2 == 1) {
    valid2++
  }
}

END {
  print "Valid: " valid
  print "Invalid: " invalid
  print "Total: " valid + invalid

  print "Valid By Position: " valid2
}
