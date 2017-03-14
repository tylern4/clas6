#!/usr/bin/env perl
#
# Dumps calibration constants given system, subsystem, item, run number and
# (optionally) the run index table name.
#
# $Id: caldb_show_constants_run.pl,v 2.4 2001/06/15 15:46:47 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ReadConstants($dbh, $system, $subsystem, $item, $run,
			$runIndexTable, $time, \@itemvalue);

# display the results

$ncol = $#itemvalue; # index of last array item

$index = $itemvalue[0];
$author = $itemvalue[1];
$time_constants = $itemvalue[2];
$source_min = $itemvalue[3];
$source_max = $itemvalue[4];
$comment = $itemvalue[5];

if (!$quiet) { 
    print "$index $author $time_constants $source_min $source_max\n";
    print "$comment\n";
}

for ($i = 6; $i <= $ncol; $i++) {
    print "$itemvalue[$i]\n";
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
	    if ($verbose) {print "using runIndexTable = $runIndexTable\n";}
	}
    }
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if (!$time){
	if ($t) {
	    $time = $t;
	}
    }
    if (!$quiet){
	if ($q) {
	    $quiet = $q;
	}
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_constants_run.pl s=<system> ss=<subsystem> i=<item> r=<run> \\
    [it=<run index table>] [t=<time of validity>] \\
    [hostname=<hostname of db server>] [q=<non-zero to suppress some info>] \\
    [help=<non-zero>]
 

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  r or run for run number
  it or runIndexTable for run index table name
  t or time for time of validity
  q or quiet for suppression of extra information

example:
caldb_show_constants_run.pl s=DC_DOCA ss=t_max i=Sector6 r=20000
EOM
    return;
}

# end of perl script
