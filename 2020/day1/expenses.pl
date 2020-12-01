#!/usr/bin/env perl

# usage: perl expenses.pl input

use strict;
use warnings;
use 5.010;

chomp(my @expenses = <>);

# Separate the expenses into likely pairs of elements <1000 and >=1000 to reduce
# search space
my @lessthan = grep("$_" < 1000, @expenses);
my @greaterthan = grep("$_" >= 1000, @expenses);

# Find expense pairs that add up to 2020 and report the product
for my $less (@lessthan) {
  for my $greater (@greaterthan) {
    if (($less + $greater) == 2020) {
      printf "first star: %d %d %d\n", $less, $greater, $less * $greater;
    }
  }
}

# Find expense triplets that add up to 2020 and report the product
# Note: this reports the solution twice, as it loops over the smaller set twice.
for my $less (@lessthan) {
  for my $less2 (@lessthan) {
    next if $less == $less2;
    for my $greater (@greaterthan) {
      if (($less + $less2 + $greater) == 2020) {
        printf "second star: %d %d %d %d\n", $less, $less2, $greater, $less * $less2 * $greater;
      }
    }
  }
}
