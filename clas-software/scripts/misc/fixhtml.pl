#!/usr/local/bin/perl

#
# fixhtml.pl
#
# $Id : $
#        S. McAleer
#        Florida State University
#        December 10, 1997

# This file appends the offline analysis web pages. It takes as an argument
# number for a cooked run and the table based html file that conatins all
# the cooked runs. It outputs an ammended html file of the same name.

$run = $ARGV[0];
$html_file = "/home/manak/public_html/offline_analysis_dec97.html";
$tmp = "/home/manak/public_html/tmp.html";

rename ($html_file, "$tmp");

if (-e "$tmp" && -r "$tmp") {
    open (IHTML, "$tmp");
} else {
    die " HTML file either doesn't exist or isn't readable!\n";
}

open(OHTML, ">$html_file");

while (<IHTML>) {
    if ($_ =~ /<\/TABLE>/) {
	print OHTML "<TR ALIGN=CENTER>\n";
	print OHTML "<TD> <B><font COLOR= \"#AF0000 \"> $run</font></B> </TR>\n";
	print OHTML "<TD> <B><font COLOR= \"#AF0000 \"> run$run.cooked</font></B> </TR>\n";
    } 
    print OHTML $_;
}
close (IHTML);
close (OHTML);

unlink("$tmp");


