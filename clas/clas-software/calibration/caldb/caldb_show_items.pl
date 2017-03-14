#!/usr/bin/env perl
#
# $Id: caldb_show_items.pl,v 1.5 2001/12/07 21:22:49 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowItems($dbh, $system, $subsystem, \@itemArray);

# display the results

if ($oneline) {
    $lead_char = ' ';
} else {
    $lead_char = "\n";
}
foreach $is (0 .. $#itemArray) {
    if ($is == 0) {
	print "$itemArray[$is]";
    } else {
	print "$lead_char$itemArray[$is]";
    }
}
print "\n";

exit 0;

sub ProcessCommandLine {

    $user = "clasuser";
    $password = "";
    $database = "calib";

    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if ($usage || $help) {
	&PrintUsage();
	exit 0;
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_items.pl [system=<system>] [subsystem=<subsystem>] \\
    [hostname=<hostname of db server>] [oneline=<non-zero>] [help=<non-zero>]

example:
caldb_show_items.pl
EOM
    return;
}

# end of perl script
