#!/usr/bin/env perl
$filename = $ARGV[0];
$date = $ARGV[1];
system("'rm' -f /tmp/cvs_rev_date.tmp");
system("cvs update -p -D \"$date\" $filename 1> /dev/null 2>/tmp/cvs_rev_date.tmp");
open(CVS,"/tmp/cvs_rev_date.tmp");
while (<CVS>) {
    if (/VERS:/) {
	@field = split(/\s+/);
	$revision = $field[1];
	print $revision;
    }
}
close(CVS);
system("'rm' -f /tmp/cvs_rev_date.tmp");
exit;
