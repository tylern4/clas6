#!/usr/bin/env perl
#
# $Id: caldb_show_item_info.pl,v 2.4 2002/09/20 14:33:03 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowItemInfo($dbh, $system, $subsystem, $item, \%itemInfo);

# display the results

print "systemId = $itemInfo{systemId}\n";
print "subsystemId = $itemInfo{subsystemId}\n";
print "itemId = $itemInfo{itemId}\n";
print "length = $itemInfo{length}\n";
print "type = $itemInfo{type}\n";
print "description = $itemInfo{description}\n";

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
    if (!$system) {
	if ($s) {
	    $system = $s;
	} else {
	    print "error: system not defined\n";
	    &PrintUsage();
	    exit 1;
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    print "error: subsystem not defined\n";
	    &PrintUsage();
	    exit 1;
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
	    print "error: item not defined\n";
	    &PrintUsage();
	    exit 1;
	}
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_item_info.pl system|s=<system> subsystem|ss=<subsystem> \\
    item|i=<item> [hostname=<hostname of db server>] [help=<non-zero>]

example:
caldb_show_item_info.pl system=DC_DOCA subsystem=xvst_params item=SL3
EOM
    return;
}

# end of perl script
