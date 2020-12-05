#!/bin/bash -ex

# At competition time I forgot how to push onto a C++ vector, sort, and then
# display it. However, realized I could just post-process my original scoring
# output as the seatID was the last column.

# Grab the seatIDs and sort numerically, and then print out any seats that don't
# match the pattern current = last + 1
awk '{ print $4 }' |
    sort -n |
    awk '{ if (last > 0 && $1 != last + 1) { print "Missing Seat: " last + 1}; last = $1 }'
