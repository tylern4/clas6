#!/usr/bin/env perl
#
# $Id: caldb_show_systems.pl,v 1.7 2001/12/07 21:22:49 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowSystems($dbh, $subsystem, $item, \@systemArray);

# display the results

if ($oneline) {
    $lead_char = ' ';
} else {
    $lead_char = "\n";
}
foreach $is (0 .. $#systemArray) {
    if ($is == 0) {
	print "$systemArray[$is]";
    } else {
	print "$lead_char$systemArray[$is]";
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
    if ($help) {
	&PrintUsage();
	exit 0;
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_systems.pl [subsystem=<subsystem>] [item=<item>] \\
    [hostname=<hostname of db server>] [oneline=<non-zero>] [help=<non-zero>]

example:
caldb_show_systems.pl
EOM
    return;
}

# end of perl script
