#!/usr/bin/env perl

$dir = $ARGV[0];

print STDERR "processing $dir\n";

shift;


if($#ARGV >= 0) {
    for (@ARGV) {
        if (/^-r/) {
	  $root = 1;
	}
      }
  }


$cmd = "ls -1 $dir |";
print STDERR "$cmd\n";
open(A,$cmd);
while (<A>) {
  chop;
  $file = $dir . "/" . $_;
  $name = "-a/work/clas/disk1/weygand/oduOut/" . $_ . ".view";
  print STDERR "processing file $file \n";
  $cmd = "g11p2pi_k0  $name -p2 -X1 -C1 -F1 $file |";
  print STDERR "$cmd\n";
  open(B,$cmd);
  while(<B>) {
    print;
  }
  close(B);
}

