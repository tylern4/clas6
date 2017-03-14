#!/usr/bin/env perl
$date = $ARGV[0];
print "Doing cvs diff against \"$date\"\n";
open (CVS,"cvs -n -q update -D \"$date\" |");
while (<CVS>) {
    @field = split(/\s+/);
    if ($field[0] eq "U") {
	$filename = $field[1];
	print "file: $filename\n";
	print "---------------------------------\n";
	print "originally tried:\n";
	$revision_orig = `cvs_rev_date.perl $filename \"$date\"`;
        $comment_orig = `cvs_com.perl $filename $revision_orig`;
	print $comment_orig;
	print "---------------------------------\n";
	print "used in this release:\n";
	$revision = `cvs_rev.perl $filename`;
        $comment = `cvs_com.perl $filename $revision`;
	print $comment;
	print "=================================\n";
    }
}
close (CVS);
exit
