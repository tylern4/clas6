#!/usr/bin/env perl
#
# $Id: caldb_show_run_ranges.pl,v 1.8 2001/12/13 21:34:02 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

$ncol = 6;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowRunRanges($dbh, $system, $subsystem, $item,
			$runIndexTable, $time,$runMin,$runMax, \@runRangeInfo);

# display the results

foreach $ir (0 .. $#runRangeInfo) {
    $runMin_this = $runRangeInfo[$ir][0];
    foreach $ic (0 .. $ncol - 2) {
	if ($ic != 0) {print ' ';}
	print "$runRangeInfo[$ir][$ic]";
    }
    if ($const) {ShowConstants($runMin_this);}
    print " $runRangeInfo[$ir][$ncol - 1]";
    print "\n";
}

exit 0;

sub ShowConstants {
    my ($run) = @_;
    ReadConstants($dbh, $system, $subsystem, $item, $run, $runIndexTable,
		  $time, \@constants_array);
    if ($first) {
	$icon_last = 6;
    } else {
	$icon_last = $#constants_array;
    }
    for $icon (6 .. $icon_last) {
	print " $constants_array[$icon]";
    }
    return;
}

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
    if (!$runMin) {
        if ($min) {
            $runMin = $min;
	} else {
	    $runMin=1;
	}
    }
    if (!$runMax) {
        if ($max) {
            $runMax = $max;
        } else {
	    $runMax=1000000;
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
purpose: shows effective run ranges for a specific item

usage:
caldb_show_run_ranges.pl system|s=<system> subsystem|ss=<subsystem> \\
    item|i=<item> [runMin|min=<run min>] [runMax|max=<run max>] \\
    [const=<non-zero>] [first=<non-zero>] \\
    [runIndexTable|it=<run index table>] [time|t=<time of validity>] \\
    [hostname=<hostname of db server>] [help=<non-zero>]

example:
caldb_show_run_ranges.pl s=mom_corr ss=theta_func i=sector6
EOM
    return;
}
