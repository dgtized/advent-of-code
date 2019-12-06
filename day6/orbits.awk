#!/usr/bin/gawk -f

BEGIN {
  FS=")"
}

{
  bodies[$1] = 1
  bodies[$2] = 1
  orbits[$2] = $1
}

END {
  total = 0;
  for(body in bodies) {
    indirect = 0
    current = body
    while(current != "COM") {
      current = orbits[current]
      indirect++
    }

    total += indirect;
  }

  print "checksum: " total
}
