#!/usr/bin/env perl
#
# $Id: caldb_show_sets_run.pl,v 2.1 2001/06/14 19:44:03 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

$ncol = 9; # number of columns returned from queury

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowSetsRun($dbh, $run,
			$runIndexTable, $time, \@itemInfo);

# display the results

for ($iitem = 0; $iitem <= $#itemInfo; $iitem++) {
    for ($icol = 0; $icol < $ncol; $icol++) {
	if ($icol != 0) {print ' ';}
	print "$itemInfo[$iitem][$icol]";
    }
    print "\n";
}

exit 0;

sub ProcessCommandLine {

    $user = "clasuser";
    $password = "";
    $database = "calib";

    if ($help) {
	PrintUsage();
	exit 0;
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
    if (!$time){
	if ($t) {
	    $time = $t;
	}
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_sets_run.pl r=<run> [it=<run index table>] \\
    [t=<time of validity>] [hostname=<hostname of db server>] \\
    [help=<non-zero>]

alternate flag names:
  r or run for run number
  it or runIndexTable for run index table name
  t or time for time of validity

example:
caldb_show_sets_run.pl r=20000
EOM
    return;
}
