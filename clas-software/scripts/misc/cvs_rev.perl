#!/usr/bin/env perl
$filename = $ARGV[0];
open (CVS,"cvs status $filename |");
while (<CVS>) {
    #print;
    if (/Working revision/) {
	@field = split(/\s+/);
	print "$field[3]";
    }
}
close (CVS);
exit
