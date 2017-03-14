#!/usr/bin/env perl
$filename = $ARGV[0];
$revision = $ARGV[1];
open (CVS,"cvs log $filename |");
$lprint = 0;
while (<CVS>) {
    if (/revision $revision$/) {
	$lprint = 1;
    }
    if (/-----------/ || /==========/) {
	$lprint = 0;
    }
    if ($lprint) {
	print;
    }
}
close (CVS);
exit
