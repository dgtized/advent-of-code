#!/usr/bin/env perl

# usage: perl sonar.pl input

use strict;
use warnings;
use v5.10;

my @readings = <>;
chomp(@readings);

my $count = 0;
my $last = $readings[0];
for my $v (@readings[1..scalar(@readings)-1]) {
  if ($v > $last) {
    $count++;
  }
  $last = $v;
}
say "First Star: ", $count;

my $count2 = 0;
for(my $i = 0; $i < scalar(@readings); $i++) {
  if ($i > 2) {
    my $d = $readings[$i-3];
    my $c = $readings[$i-2];
    my $b = $readings[$i-1];
    my $a = $readings[$i];
    my $s1 = $d + $c + $b;
    my $s2 = $c + $b + $a;
    if ($s2 > $s1) {
      $count2++;
    }
  }
}

say "Second Star: ", $count2;
