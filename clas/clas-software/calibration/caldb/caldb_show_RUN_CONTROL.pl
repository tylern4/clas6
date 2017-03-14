#!/usr/bin/env perl
#
# caldb_copy_run.pl
# tacking an existing set of calibration constants for a particular
# specified run and RunIndex and applying to user specified new 
# validity range and RunIndex table. The set of systems, subsystems 
# and items could be defined from the command line
#
# H.Avakian 09/30/01
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();


$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

# loop over all runs


format STDOUT_TOP =
 
run#    Energy(MeV)  Torus    mini-Torus         
--------------------------------------
.
format STDOUT =
@<<<<<<<@<<<<<<<@>>>>>>>>>@>>>>>>>>>@>>>>>>>>>>
$run,$val1,$val2,$val3
. 




for ($run = $runMin; $run <= $runMax; $run++) {

$subsystem="beam";
$item="energy";
#print "$run  $item $subsystem \n";
$status = ReadConstants($dbh, $system, $subsystem, $item, $run,
			$runIndexTable, $time, \@itemvalue);

# display the results
$index1 = $itemvalue[0];
$author1 = $itemvalue[1];
$time_constants1 = $itemvalue[2];
$comment1 = $itemvalue[5];
$val1 = $itemvalue[6];

$subsystem="currents";
$item="torus";
#print "$run  $item $subsystem \n";
$status = ReadConstants($dbh, $system, $subsystem, $item, $run,
			$runIndexTable, $time, \@itemvalue);

# display the results
$index2 = $itemvalue[0];
$author2 = $itemvalue[1];
$time_constants2 = $itemvalue[2];
$comment2 = $itemvalue[5];
$val2 = $itemvalue[6];
#
$subsystem="currents";
$item="minitorus";
#print "$run  $item $subsystem \n";
$status = ReadConstants($dbh, $system, $subsystem, $item, $run,
			$runIndexTable, $time, \@itemvalue);

# display the results
$index3 = $itemvalue[0];
$author3 = $itemvalue[1];
$time_constants3 = $itemvalue[2];
$comment3 = $itemvalue[5];
$val3 = $itemvalue[6];
write;
#print "$run  $val1 $val2 $val3 \n";
}

exit 0;

sub ProcessCommandLine {

    $database = "calib";
    $system = "RUN_CONTROL";
    $user="clasuser";
    $password="";

    if ($help) {
	&PrintUsage();
	exit 0;
    }
    if (!$runMin) {
        if ($min) {
            $runMin = $min;
        } else {
            &PrintUsage();
            die "error: runMin not defined\n";
        }
    }
    if (!$runMax) {
        if ($max) {
            $runMax = $max;
        } else {
            &PrintUsage();
            die "error: runMax not defined\n";
        }
    }
    if ($runMax < $runMin) {
            die "error: $runMax < $runMin \n";
	}
    if (!$runIndexTable) {
	if ($it) {
	    $runIndexTable=$it;
	} else {
	    $runIndexTable = "RunIndex";
	}
    }
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if (!$time){
	if ($t) {
	    $time = $t;
	} else {
	    $time = '2037-1-1';
	}
    }
    if (!$quiet){
	if ($q) {
	    $quiet = $q;
	}
    }
  if (!$quiet) {print "using RunIndex = $runIndexTable from $hostname\n";}
}


sub PrintUsage {
    print <<EOM;
function: Show RUN_CONTROL constants in range of runs (runMin to runMax).

usage:
caldb_show_RUN_CONTROL.pl  min=<run min> max=<run max>  \\
    it=<run index table> \\
    [t=<time of validity>] \\
    [hostname=<hostname of db server>]  \\
    [help=<non-zero>] [q=<non-zero to suppress some info>]
 

alternate flag names:
  min or runMin
  max or runMax
  it or  runIndexTable for source run index table name
  t or time for time of validity
  q or quiet for suppression of extra information

example:
caldb_.show_RUN_CONTROL.pl  min=30000 max=40000 it=RunIndex 
EOM
    return;
}

# end of perl script
