#!/usr/bin/env perl
#
# $Id: caldb_show_history.pl,v 1.5 2001/06/14 12:32:50 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

$ncol = 6;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowHistory($dbh, $system, $subsystem, $item, $run,
			$runIndexTable, \@historyInfo);

# display the results

foreach $ir (0 .. $#historyInfo) {
    foreach $ic (0 .. $ncol - 1) {
	if ($ic != 0) {print ' ';}
	print "$historyInfo[$ir][$ic]";
    }
    print "\n";
}

exit 0;

sub ProcessCommandLine {

    $user = "clasuser";
    $password = "";
    $database = "calib";

    if ($help) {
	&PrintUsage();
	exit 0;
    }
    if (!$system) {
	if ($s) {
	    $system = $s;
	} else {
	    &PrintUsage();
	    die "error: system not defined\n";
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    &PrintUsage();
	    die "error: subsystem not defined\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
	    &PrintUsage();
	    die "error: item not defined\n";
	}
    }
    if (!$run) {
	if ($r) {
	    $run = $r;
	} else {
	    &PrintUsage();
	    die "error: run not defined\n";
	}
    }
    if (!$runIndexTable) {
	if ($it) {
	    $runIndexTable = $it;
	} else {
	    $runIndexTable = "RunIndex";
	    print "using runIndexTable = $runIndexTable\n";
	}
    }
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_history.pl s=<system> ss=<subsystem> i=<item> r=<run number> \\
    [it=<run index table>] [hostname=<hostname of db server>] \\
    [help=<non-zero>]

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  r or run for run number
  it or runIndexTable for run index table name

example:
caldb_show_history.pl s=DC_DOCA ss=t_max i=Sector3 r=23000
EOM
    return;
}
