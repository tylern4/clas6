#!/usr/bin/env perl
#
# caldb_export_run.pl
# tacking an existing set of calibration constants for a particular
# specified run=NNNNN and RunIndex and exporting to text files in a 
# caldb_exportNNNNN directory
# sytem,subsystem and items could be defined from the command line
#
# H.Avakian 03/20/02
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);
$status = ShowSSI($dbh, $system, $subsystem, $item, \@itemArrayRef);

# create the directory
$caldb_export='caldb_export'.$runSource;

if (CheckOutDir($caldb_export) eq 0) { print "Create new dir $caldb_export \n";}

# loop over all subsystems and items
for ($iitem = 0; $iitem <= $#itemArrayRef; $iitem++) {
 $system=$itemArrayRef[$iitem][0];
 $subsystem=$itemArrayRef[$iitem][1];
 $item=$itemArrayRef[$iitem][2];
if (!$quiet){
  print "Now exporting $system $subsystem $item of $runIndexTable as of $time to $caldb_export\n";
}
&PrintItem2File($dbh, $system, $subsystem, $item, $runSource, $runIndexTable, $time,
       \@itemvalueref);
}
exit 0;

#
#
#
sub CheckOutDir {my($destdir) = @_;
if (-d "$destdir") {
$iok=1; 
## print "use existing directory $destdir \n";
} else {
##print "create new directory $destdir\n";
mkdir("$destdir",07777) || die "eror: unable to mkdir $destdir!!!!\n";
$iok=0;
}
return $iok;
	     }
#
#
#
sub PrintItem2File {
    my($dbh, $system, $subsystem, $item, $runSource, $runIndexTable, $time,
       $itemvalueref) = @_;

$f_name=$caldb_export.'/'.$system;
if (CheckOutDir($f_name) eq 0) { print "Create new  $f_name \n";}

$status = ReadConstants($dbh, $system, $subsystem, $item, $runSource,
			$runIndexTable, $time, \@itemvalue);

# display the results

$ncol = $#itemvalue; # index of last array item

$index = $itemvalue[0];
$author = $itemvalue[1];
$time_constants = $itemvalue[2];
$source_min = $itemvalue[3];
$source_max = $itemvalue[4];
$comment = $itemvalue[5];

$f_name=$f_name.'/'.$subsystem.'_'.$item;

unless (open(OUTF,">$f_name")) {die "File $f_name: open failed!\n";
}else{ 

for ($i = 6; $i <= $ncol; $i++) {
    print OUTF "$itemvalue[$i]\n";
}
    close(OUTF);
}
}
sub ProcessCommandLine {

    $database = "calib";
    $user = 'clasuser';
    $password="";

    if ($help) {
	&PrintUsage();
	exit 0;
    }
    if (!$system) {
	if ($s) {
	    $system = $s;
	} else {
	    print "warning:system not defined\n  copy all systems\n";
#	    &PrintUsage();
#	    die "error: system not defined\n";
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    print "warning:subsystem not defined\n      linking all subsystem for $system\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
  print "warning:item not defined\n        linking all items  for $system  $subsystem\n";
	}
    }
    if (!$runSource) {
	if ($r) {
	    $runSource = $r;
	} else {
	    &PrintUsage();
	    die "error: Run not defined\n";
	}
    }
    if (!$runIndexTable) {
	if ($it) {
	    $runIndexTable = $it;
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
  if (!$quiet) {print "using runIndexTable = $runIndexTable from $hostname\n";}
}


sub PrintUsage {
    print <<EOM;
function: Export constants for a given run to a directory caldb_export<run>.

usage:
caldb_export_run.pl   r=<runSource> \\
    [s=<system>] [ss=<subsystem>] [i=<item>] \\
    [it=<source run index table>] \\
    [t=<time of validity>] \\
    [hostname=<hostname of db server>]  \\
    [help=<non-zero>] [q=<non-zero to suppress some info>]
 

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  r or runSource for reference run number
  it or runIndexTable for source run index table name
  q or quiet for suppression of extra information

example:
caldb_export_run.pl s=DC_DOCA ss=xvst_params i=SL1 it=RunIndex r=28000 \\
EOM
    return;
}

# end of perl script
