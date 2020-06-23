#!/usr/bin/env perl
#
# $Id: caldb_show_sets_item.pl,v 2.3 2001/06/14 21:37:22 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowSetsItem($dbh, $system, $subsystem, $item, \@itemValue);

# display the results

foreach $iiv (0 .. $#itemValue) {
    if ($itemValue[$iiv]{minRunSource} eq '') {
	$itemValue[$iiv]{minRunSource} = 'NULL';
    }
    if ($itemValue[$iiv]{maxRunSource} eq '') {
	$itemValue[$iiv]{maxRunSource} = 'NULL';
    }
    print "$itemValue[$iiv]{itemValueId} $itemValue[$iiv]{author}";
    print " $itemValue[$iiv]{time} $itemValue[$iiv]{minRunSource}";
    print " $itemValue[$iiv]{maxRunSource} \'$itemValue[$iiv]{comment}\'\n";
}

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
	print "error: system not defined\n";
	&PrintUsage();
	exit 1;
    }
    if (!$subsystem) {
	print "error: subsystem not defined\n";
	&PrintUsage();
	exit 1;
    }
    if (!$item) {
	print "error: item not defined\n";
	&PrintUsage();
	exit 1;
    }
}

sub PrintUsage {
    print <<EOM;
Makes a list of all constant set ID's for the specified
system/subsystem/item.

Usage:
caldb_show_sets_item.pl system=<system> subsystem=<subsystem> item=<item> \\
    [hostname=<hostname of db server>] [help=<non-zero>]

example:
caldb_show_sets_item.pl system=DC_DOCA subsystem=xvst_params item=SL3
EOM
    return;
}

# end of perl script
