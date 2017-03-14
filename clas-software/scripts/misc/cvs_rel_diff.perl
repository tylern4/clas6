#!/usr/bin/env perl
$tag = $ARGV[0];
open (CVS,"cvs -n -q update -r $tag |");
while (<CVS>) {
    @field = split(/\s+/);
    if ($field[0] eq "U") {
	$filename = $field[1];
	print "file: $filename\n";
	$revision = `cvs_rev.perl $filename`;
	$revision_old = `cvs_rev_tag.perl $filename $tag`;
	print "old revision $revision_old\n";
	print "new ";
	if ($revision eq "No") {
	    $comment = "revision none\ndoes not appear in this release\n";
	} else {
	    $comment = `cvs_com.perl $filename $revision`;
	}
	print $comment;
	print "---------------------------------\n";
    }
}
close (CVS);
exit
