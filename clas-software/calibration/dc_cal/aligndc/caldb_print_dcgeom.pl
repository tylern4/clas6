#!/usr/bin/env perl
#
# Dumps DC_GEOM calibration constants given run number and
# (optionally) the run index table name, for s=DC_GEOM; ss=dc and toroid;
# i=(xyz)align and (xyz)rot.
#
# Hacked, from a script by Mark Ito, by S. Morrow on 5/7/02.
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ReadConstants($dbh, DC_GEOM, toroid, align, $run,
			$runIndexTable, $time, \@itemvalue_talign);
$status = ReadConstants($dbh, DC_GEOM, toroid, rot, $run,
			$runIndexTable, $time, \@itemvalue_trot);
$status = ReadConstants($dbh, DC_GEOM, dc, xalign, $run,
			$runIndexTable, $time, \@itemvalue_xalign);
$status = ReadConstants($dbh, DC_GEOM, dc, yalign, $run,
			$runIndexTable, $time, \@itemvalue_yalign);
$status = ReadConstants($dbh, DC_GEOM, dc, zalign, $run,
			$runIndexTable, $time, \@itemvalue_zalign);
$status = ReadConstants($dbh, DC_GEOM, dc, xrot, $run,
			$runIndexTable, $time, \@itemvalue_xrot);
$status = ReadConstants($dbh, DC_GEOM, dc, yrot, $run,
			$runIndexTable, $time, \@itemvalue_yrot);
$status = ReadConstants($dbh, DC_GEOM, dc, zrot, $run,
			$runIndexTable, $time, \@itemvalue_zrot);

# display the results

$ncol = $#itemvalue_xalign; # index of last array item

$index = $itemvalue[0];
$author = $itemvalue[1];
$time_constants = $itemvalue[2];
$source_min = $itemvalue[3];
$source_max = $itemvalue[4];
$comment = $itemvalue[5];

#if (!$quiet) { 
#    print "$index $author $time_constants $source_min $source_max\n";
#    print "$comment\n";
#}

print "DC geometry for run: $run\n";
print "Toriod align(xyz): $itemvalue_talign[6] $itemvalue_talign[7] $itemvalue_talign[8] \n";
print "Toriod rot(xyz): $itemvalue_trot[6] $itemvalue_trot[7] $itemvalue_trot[8] \n";

$reg = 1;
$sec = 1;
for ($i = 6; $i <= $ncol; $i++) {
  if ($i>11) { $reg = 2; };
  if ($i>17) { $reg = 3; };
    print "DC align sec:$sec reg:$reg (xyz):  $itemvalue_xalign[$i]  $itemvalue_yalign[$i]  $itemvalue_zalign[$i] \n";
  $sec++;
  if ($sec>6) { $sec = 1 };
}

$reg = 1;
$sec = 1;
for ($i = 6; $i <= $ncol; $i++) {
  if ($i>11) { $reg = 2; };
  if ($i>17) { $reg = 3; };
    print "DC rot sec:$sec reg:$reg (xyz):  $itemvalue_xrot[$i]  $itemvalue_yrot[$i]  $itemvalue_zrot[$i] \n";
  $sec++;
  if ($sec>6) { $sec = 1 };
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
caldb_print_dcgeom.pl r=<run> \\
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
caldb_print_dcgeom.pl r=30910
EOM
    return;
}

# end of perl script
