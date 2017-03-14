#!/usr/bin/env perl
#
# $Id: caldb_show_subsystems.pl,v 1.6 2003/03/10 21:44:16 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowSubsystems($dbh, $system, $item, \@subsystemArray);

# display the results

if ($oneline) {
    $lead_char = ' ';
} else {
    $lead_char = "\n";
}
foreach $is (0 .. $#subsystemArray) {
    if ($is == 0) {
	print "$subsystemArray[$is]";
    } else {
	print "$lead_char$subsystemArray[$is]";
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
    if ($s) {
	$system = $s;
    }
    if ($i) {
	$item = $i;
    }
    if ($help) {
	&PrintUsage();
	exit 0;
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_subsystem.pl [system|s=<subsystem>] [item|i=<item>] \\
    [hostname=<hostname of db server>] [oneline=<non-zero>] [help=<non-zero>]

example:
caldb_show_subsystems.pl
EOM
    return;
}

# end of perl script
