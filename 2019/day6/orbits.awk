#!/usr/bin/gawk -f

# ./orbits.awk input

BEGIN { FS=")" } # split on field separator )

# loop through input & record orbits and all bodies seen
{
  bodies[$1] = 1
  bodies[$2] = 1
  orbits[$2] = $1
}

END {
  # calculate checksum by counting path to COM from all bodies
  total = 0
  for(body in bodies) {
    current = body
    while(current != "COM") {
      current = orbits[current]
      total++
    }
  }
  print "checksum: " total

  # compute distance from santa until COM
  distance = 0
  current = "SAN"
  while(current != "COM") {
    current = orbits[current]
    path[current] = distance
    distance++
  }

  # walk back from YOU until it intersects path from SAN
  distance = 0
  current = "YOU"
  while(current != "COM") {
    current = orbits[current]
    if (current in path) {
      print "intersection @" current " " distance " " path[current] " -> " distance + path[current]
      break
    }
    distance++
  }
}
