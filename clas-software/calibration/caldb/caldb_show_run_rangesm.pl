#!/usr/bin/env perl
#
# $Id: caldb_show_run_rangesm.pl,v 2.4 2001/11/29 12:49:57 avakian Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

$ncol = 7;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowRunRanges($dbh, $system, $subsystem, $item,
			$runIndexTable, $time,$minRun,$maxRun, \@runRangeInfo);

# display the results

$itemValueTable = $system.'_'.$subsystem.'_'.$item;
$field=($channel<9 ? v_000.$channel : ($channel<99 ?  v_00.$channel: ($channel<999 ?  v_0.$channel : v_.$channel)));

foreach $ir (0 .. $#runRangeInfo) {
$itemvalueId = $runRangeInfo[$ir][2];

$sql="SELECT $field FROM  $itemValueTable WHERE itemvalueId=$itemvalueId";   
       $sth = $dbh->prepare($sql);
       $sth->execute;
    $value=$sth->fetchrow_array;
    foreach $ic (0 .. $ncol - 1) {
	if ($ic != 0) {print ' ';}
	print "$runRangeInfo[$ir][$ic]";
    }
    print "$field $value \n";
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
    if (!$channel) {
	if ($c) {
	    $channel = $c;
	} else { $channel=1;}
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
    if (!$minRun) {
	if ($min) {
	    $minRun = $min;
	} else {
            $minRun=1;
	    if(!runP) {print "Warning!: minRun not defined, use minRun=1\n";}
	}
    }
    if (!$maxRun) {
	if ($max) {
	    $maxRun = $max;
	} else {
            $maxRun=1000000;
	    if(!runP) {print "Warning!: minRun not defined, use maxRun=1000000\n";}
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
caldb_show_run_rangesm.pl s=<system> ss=<subsystem> i=<item> [c=<channel>] \\
    [min=<min run>] [max=<max run>] [it=<run index table>] [t=<time of validity>] \\
    [hostname=<hostname of db server>] [help=<non-zero>]

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  min or minRun for minRun
  max or maxRun for minRun
  it or runIndexTable for run index table name
  t or time for time of validity
  c or channel   channel number to print
example:
caldb_show_run_rangesm.pl s=RUN_CONTROL ss=currents i=torus  min=27350 max=27500
EOM
    return;
}
