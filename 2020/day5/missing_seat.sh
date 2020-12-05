#!/bin/bash -ex

awk '{ print $4 }' |
    sort -n |
    awk '{ c = $1; if (last > 0 && c != last + 1) { print "Missing Seat: " last + 1}; last = $1 }'
