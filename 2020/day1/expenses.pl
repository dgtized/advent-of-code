#!/usr/bin/env perl

use strict;
use warnings;
use 5.010;

chomp(my @expenses = <>);

my @lessthan = grep("$_" < 1000, @expenses);
my @greaterthan = grep("$_" >= 1000, @expenses);

for my $less (@lessthan) {
  for my $greater (@greaterthan) {
    if (($less + $greater) == 2020) {
      printf "first star: %d %d %d\n", $less, $greater, $less * $greater;
    }
  }
}

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
