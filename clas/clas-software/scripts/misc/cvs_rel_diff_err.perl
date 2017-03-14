#!/usr/bin/env perl
open (ERROR,"/tmp/cvs_error.tmp");
while (<ERROR>) {
    if (/no longer in the repository/) {
	@field = split(/\s+/);
	$filename = $field[2];
	print "file: $filename\n";
	$revision = `cvs_rev.perl $filename`;
	$comment = `cvs_com.perl $filename $revision`;
	print $comment;
	print "---------------------------------\n";
    }
}
close (ERROR);
exit
