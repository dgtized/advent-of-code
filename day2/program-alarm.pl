#!/usr/bin/env perl

use strict;
use warnings;
use 5.010;

sub trace {
  my ($a, $b, @memory) = @_;
  my $ip = 0;

  $memory[1] = $a;
  $memory[2] = $b;

  #printf "%s\n", join ",", @memory;
  while (my $op = $memory[$ip]) {
    my ($arg1, $arg2, $target) = @memory[$ip+1, $ip+2, $ip+3];
    my ($mem1, $mem2) = @memory[$arg1, $arg2];
    my $result = 0;
    if ($op == 1) {
      $result = $mem1 + $mem2;
    } elsif ($op == 2) {
      $result = $mem1 * $mem2;
    } elsif ($op == 99) {
    } else {
      die "invalid operation"
    }

    $memory[$target] = $result;

    #printf "[%d] %d - %d, %d -> %d [%d, %d -> %d]\n", $ip, $op, $arg1, $arg2, $target, $mem1, $mem2, $result;
    #printf "%s\n", join ",", @memory;

    $ip += 4;
  }

  return $memory[0];
}

my $input = <>;

chomp $input;
my @memory = split /,/, $input;

printf("%d\n", trace(12, 2, @memory));
