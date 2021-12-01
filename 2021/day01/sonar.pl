#!/usr/bin/env perl

# usage: perl sonar.pl input

use strict;
use warnings;
use 5.010;

chomp(my @readings = <>);

my $last = shift @readings;
my $count = 0;

for my $v (@readings) {
  if ($v > $last) {
    $count++;
  }
  $last = $v;
}

say $count;
