#!/usr/bin/env perl
#
# Sets the value for a specific RUN_CONTROL subsystem item
# given the vlaue and the validity range
# 
# H. Avagyan 10/01
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# connect to database

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

# timelimit
$timeLimit=($time ? $time : '2037-1-1');

# get RunPeriod
 print "set_RUN_CONTROL: $runMin - $runMax \n";

&checkRunPeriod($dbh,$runMin,$runMax,\$runP);

if(!$runP) { die  "inconsistent entry: check range with runinfo! \n";}
else { print "  $runIndexTable ($runP)  insert $v to $system $subsystem $item \n";}

# define subsystems/items

push (@constants, $v);

$status = WriteConstantSet($dbh, $system, $subsystem, $item, $runMin,
			   $runMin, $commentConstants, \$itemValueId,
			   @constants);

$status = LinkConstantSet($dbh, $system, $subsystem, $item, $runIndexTable,
			  $runMin, $runMax, $itemValueId, $commentIndex,
			  \$runIndexId);


print "$item entry $v  on  $hostname (runIndexId = $runIndexId, itemValueId = $itemValueId)\n";


exit 0;

sub ProcessCommandLine {


    $system=RUN_CONTROL;
    $user = $ENV{USER};

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
    if ($runMax < $runMin) {die "error: $runMax less than $runMin \n";}

    if (!defined $v) {
	    &PrintUsage();
            die "error:  define the input value\n";                 
    }

    if (!$runIndexTable) {
        if ($it) {
            $runIndexTable = $it;
        } else {
	    &PrintUsage();
            die "error:  runIndexTable not defined\n";                 
        }
    }

    if (!time) {
	$time = '2037-01-01';
    }

    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }

    if (!$database){
	$database = "calib";
    }

}


sub PrintUsage {
    print <<EOM;
usage:
caldb_set_RUN_CONTROL.pl runP=<run period>  it=<source run index table name> 
    ss=<subsystem> i=<item> \\
    min=<run min> max=<run max>  \\
    v=<value>
    [time=<time strobe>] \\
    [user=<MySQL user name>] [hostname=<hostname of db server>] \\

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  min
  max
  it        run index table name
   v        value for a corresponding item in RUN_CONTROL

example:
caldb_set_RUN_CONTROL.pl ss=currents i=torus it=calib_user.RunIndexeg1b \\
  min=50000 max=50010 v=3375.0
EOM
    return;
}
