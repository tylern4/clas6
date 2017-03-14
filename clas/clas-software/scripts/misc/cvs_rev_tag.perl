#!/usr/bin/env perl
$filename = $ARGV[0];
$tag = $ARGV[1];
open (CVS,"cvs log $filename |");
while (<CVS>) {
    #print;
    if (/$tag:/) {
	@field = split(/\s+/);
	print $field[2];
    }
}
close (CVS);
exit
